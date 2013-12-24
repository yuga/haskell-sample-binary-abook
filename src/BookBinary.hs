{-# OPTIONS_GHC -fno-warn-orphans #-}

module BookBinary where

import Codec.Binary.UTF8.String (decode, encode)
import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (void)
import Data.Binary (Binary)
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Bits.Put as Bits
import Data.Binary (Binary (get, put), decodeFile, encodeFile)
import Data.Binary.Builder.Internal (writeN)
import Data.Binary.Get (Get, getByteString, getWord32le, getWord16le, runGetOrFail, skip)
import Data.Binary.Get.Internal (readNWith)
import Data.Binary.Put (Put, putBuilder, putWord16le, putWord32le, runPut)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import Data.Word (Word32)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (peek, poke))
import qualified System.IO as IO

import Book

--import Control.Monad (join)
--import Debug.Trace (trace, traceShow)
--join traceShow $ something

--
-- reading and writing binary file with Data.Binary.* in binary package
--

readHeaderB :: Get Header
readHeaderB = do
    header <- Bits.runBitGet $
        Header <$> Bits.getWord8 4
               <*> Bits.getWord8 2
               <*> Bits.getWord8 2
               <*> Bits.getBool
               <*> Bits.getBool
               <*> Bits.getBool
               <*> Bits.getBool
               <*> Bits.getBool
    header <$> getWord32le <* skip 2

writeHeaderB :: Header -> Put
writeHeaderB (Header v m s a0 a1 a2 a3 a4 n) = do
    Bits.runBitPut $ do
        Bits.putWord8 4 v
        Bits.putWord8 2 m
        Bits.putWord8 2 s
        Bits.putBool a0
        Bits.putBool a1
        Bits.putBool a2
        Bits.putBool a3
        Bits.putBool a4
    putWord32le n
    putZero 2

readPersonB :: Get Person
readPersonB =
    Person <$> (getUTF8StringNul 128)
           <*> get
           <*> (getWord16le <* skip 6)

writePersonB :: Person -> Put
writePersonB (Person n a s) = do
    putUTF8StringNul 128 n
    put a
    putWord16le s
    putZero 6

readAddressB :: Get Address
readAddressB =
    Address <$> (getUTF8StringNul 128)
            <*> (getUTF8StringNul  64)
            <*> (getUTF8StringNul  16)
            <*> getDouble
            <*> getDouble

writeAddressB :: Address -> Put
writeAddressB (Address r c pc la lo) = do
    putUTF8StringNul 128 r
    putUTF8StringNul  64 c
    putUTF8StringNul  16 pc
    putDouble la
    putDouble lo

getUTF8StringNul :: Int -> Get String
getUTF8StringNul n = toStringNul <$> getByteString n

toStringNul :: S.ByteString -> String
toStringNul = decode . getArray0 . S.unpack
  where
    getArray0 []     = []
    getArray0 (0:_)  = []
    getArray0 (w:ws) = w : getArray0 ws

putUTF8StringNul :: Int -> String -> Put
putUTF8StringNul n s = do
    putBuilder $ writeN n $ \p -> do
        void $ S.memset p 0 (fromIntegral n)
        pokeArray p $ take (n - 1) . encode $ s

getDouble :: Get Double
getDouble = readNWith 8 $ peek . castPtr

putDouble :: Double -> Put
putDouble d = putBuilder $ writeN 8 $ \p -> poke (castPtr p) d

putZero :: Int -> Put
putZero n = putBuilder $ writeN n $ \p -> void $ S.memset p 0 (fromIntegral n)

instance Binary Header where
    get = readHeaderB
    put = writeHeaderB

instance Binary Person where
    get = readPersonB
    put = writePersonB

instance Binary Address where
    get = readAddressB
    put = writeAddressB

readPeopleB :: Word32 -> Get [Person]
readPeopleB 0 = return []
readPeopleB n = do
    p <- get
    ps <- readPeopleB (n - 1)
    return $ p:ps

readBookB :: Get (Header, [Person])
readBookB = do
    header <- get
    people <- readPeopleB (number header)
    return (header, people)

writePeopleB :: [Person] -> Put
writePeopleB [] = return ()
writePeopleB (p:ps) = do
    put p
    writePeopleB ps

readB :: FilePath -> IO (Header, [Person])
readB f = do
    bs <- L.readFile f
    case runGetOrFail readBookB bs of
      (Left  (_, _, err))  -> fail err
      (Right (_, _, book)) -> return book

writeB :: FilePath -> (Header, [Person]) -> IO ()
writeB f (header, people) = IO.withFile f IO.WriteMode $ \h ->
    L.hPut h $ runPut $ do
        put header
        writePeopleB people

instance Binary Book 

serializeB :: FilePath -> Book -> IO ()
serializeB = encodeFile

deserializeB :: FilePath -> IO Book
deserializeB = decodeFile
