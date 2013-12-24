{-# OPTIONS_GHC -fno-warn-orphans #-}

module BookCereal where

import Codec.Binary.UTF8.String (decode, encode)
import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (void)
import Data.Bits (shift, shiftR, (.&.), (.|.))
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import Data.Word (Word8, Word32)
import Data.Serialize (Serialize (get, put), Putter)
import Data.Serialize.Get (Get, runGet, getByteString, getWord8, getWord16le, getWord32le, skip)
import Data.Serialize.IEEE754 (getFloat64le, putFloat64le)
import Data.Serialize.Put (Put, runPut, putByteString, putWord8, putWord16le, putWord32le)
import Foreign.Marshal.Array (pokeArray)
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

import Book

--
-- reading and writing binary file with Data.Serialize.* in cereal package
--

readHeaderC :: Get Header
readHeaderC = do
    w0 <- getWord8
    w1 <- getWord8
    Header     (toNbitsWord8 4 4 w0)
               (toNbitsWord8 2 2 w0)
               (toNbitsWord8 0 2 w0)
               (toBool 7 w1)
               (toBool 6 w1)
               (toBool 5 w1)
               (toBool 4 w1)
               (toBool 3 w1)
           <$> (getWord32le <* skip 2)

writeHeaderC :: Header -> Put
writeHeaderC (Header v m s a0 a1 a2 a3 a4 n) = do
    putWord8 $ ((v  `shift` 4) .|. (m `shift` 2)  .|. s :: Word8)
    putWord8 $ (bit a0 7 .|. (bit a1 6)
                         .|. (bit a2 5)
                         .|. (bit a3 4)
                         .|. (bit a4 3))
    putWord32le n
    putZero 2
  where
    bit :: Bool -> Int -> Word8
    bit True  b = 1 `shift` b
    bit False _ = 0

readPersonC :: Get Person
readPersonC =
    Person <$> getUTF8StringNul 128
           <*> readAddressC
           <*> (getWord16le <* skip 6)

writePersonC :: Person -> Put
writePersonC (Person n a s) = do
    putUTF8StringNul 128 n
    put a
    putWord16le s
    putZero 6

readAddressC :: Get Address
readAddressC =
    Address <$> getUTF8StringNul 128
            <*> getUTF8StringNul  64
            <*> getUTF8StringNul  16
            <*> getFloat64le
            <*> getFloat64le

writeAddressC :: Address -> Put
writeAddressC (Address r c pc la lo) = do
    putUTF8StringNul 128 r
    putUTF8StringNul  64 c
    putUTF8StringNul  16 pc
    putFloat64le la
    putFloat64le lo

toNbitsWord8 :: Int -> Int -> Word8 -> Word8
toNbitsWord8 off size w = w `shiftR` off .&. (2^size - 1)

toBool :: Int -> Word8 -> Bool
toBool off w = w .&. (1 `shift` off) /= 0 

getUTF8StringNul :: Int -> Get String
getUTF8StringNul n = toStringNul <$> getByteString n

putUTF8StringNul :: Int -> String -> Put
putUTF8StringNul n s = do
    putByteString $ IO.unsafePerformIO $ S.create n $ \p -> do
        void $ S.memset p 0 (fromIntegral n)
        pokeArray p $ take (n - 1) . encode $ s

toStringNul :: S.ByteString -> String
toStringNul = decode . getArray0 . S.unpack
  where
    getArray0 []     = []
    getArray0 (0:_)  = []
    getArray0 (w:ws) = w : getArray0 ws

putZero :: Int -> Put
putZero 0 = return ()
putZero n = putWord8 0 >> putZero (n - 1)

instance Serialize Header where
    get = readHeaderC
    put = writeHeaderC

instance Serialize Person where
    get = readPersonC
    put = writePersonC

instance Serialize Address where
    get = readAddressC
    put = writeAddressC 

readPeopleC :: Word32 -> Get [Person]
readPeopleC 0 = return []
readPeopleC n = do
    p <- get
    ps <- readPeopleC (n - 1)
    return (p:ps)

writePeopleC :: [Person] -> Put
writePeopleC [] = return ()
writePeopleC (p:ps) = do
    put p
    writePeopleC ps

readBookC :: Get (Header, [Person])
readBookC = do
    header <- get
    people <- readPeopleC (number header)
    return (header, people)

readC :: FilePath -> IO (Header, [Person])
readC f = do
    bs <- S.readFile f
    case runGet readBookC bs of
      (Left  err)  -> fail err
      (Right book) -> return book

writeC :: FilePath -> (Header, [Person]) -> IO ()
writeC f (header, people) = IO.withFile f IO.WriteMode $ \h ->
    S.hPut h $ runPut $ do
        put header
        writePeopleC people

serializeBookC :: Putter Book
serializeBookC (Book header people) = do
    put header
    put people

deserializeBookC :: Get Book
deserializeBookC = Book <$> get <*> get

instance Serialize Book where
    get = deserializeBookC
    put = serializeBookC

serializeC :: FilePath -> Book -> IO ()
serializeC f book = IO.withFile f IO.WriteMode $ \h ->
    S.hPut h $ runPut $ put book

deserializeC :: FilePath -> IO Book
deserializeC f = do
    bs <- S.readFile f
    case runGet get bs of
        (Left err)   -> fail err
        (Right book) -> return book
