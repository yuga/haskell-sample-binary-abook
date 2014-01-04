{-# OPTIONS_GHC -fno-warn-orphans #-}

module BookBinaryStrict where

import Codec.Binary.UTF8.String (decode)
import Control.Applicative ((<$>), (<*>), (<*))
import qualified Data.Binary.Strict.BitGet as BitGet
import Data.Binary.Strict.Get
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import Data.Word (Word32)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (peek))
import qualified System.IO.Unsafe as IO

import Book hiding (header, people)

readHeaderBS :: Get Header
readHeaderBS = do
    bs <- getByteString 2
    case BitGet.runBitGet bs getHeaderFrontPart of
        Left err -> fail err
        Right header -> header <$> getWord32le <* skip 2
  where
    getHeaderFrontPart = do
        v  <- BitGet.getAsWord8 4
        m  <- BitGet.getAsWord8 2
        s  <- BitGet.getAsWord8 2
        a0 <- BitGet.getBit
        a1 <- BitGet.getBit
        a2 <- BitGet.getBit
        a3 <- BitGet.getBit
        a4 <- BitGet.getBit
        return $ Header v m s a0 a1 a2 a3 a4

readPersonBS :: Get Person
readPersonBS =
    Person <$> getUTF8StringNul 128
           <*> readAddressBS
           <*> getWord16le <* skip 6

readAddressBS :: Get Address
readAddressBS =
    Address <$> getUTF8StringNul 128
            <*> getUTF8StringNul  64
            <*> getUTF8StringNul  16
            <*> getDouble
            <*> getDouble

getUTF8StringNul :: Int -> Get String
getUTF8StringNul n = toStringNul <$> getByteString n

toStringNul :: S.ByteString -> String
toStringNul = decode . getArray0 . S.unpack
  where
    getArray0 []     = []
    getArray0 (0:_)  = []
    getArray0 (w:ws) = w : getArray0 ws

getDouble :: Get Double
getDouble = do
    bs <- getByteString 8
    return $ IO.unsafePerformIO $ S.unsafeUseAsCString bs $ peek . castPtr 

readPeopleBS :: Word32 -> Get [Person]
readPeopleBS 0 = return []
readPeopleBS n = do
    p <- readPersonBS
    ps <- readPeopleBS (n - 1)
    return (p:ps)

readBookBS :: Get (Header, [Person])
readBookBS = do
    header <- readHeaderBS
    people <- readPeopleBS (number header)
    return (header, people)

readBS :: FilePath -> IO (Header, [Person])
readBS f = do
    bs <- S.readFile f
    case runGet readBookBS bs of
      (Left  err,  _) -> fail err
      (Right book, _) -> return book
