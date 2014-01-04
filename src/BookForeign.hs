{-# OPTIONS_GHC -fno-warn-orphans #-}

module BookForeign where

import Codec.Binary.UTF8.String (decode, encode)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)
import Data.Bits (shift, shiftR, (.&.), (.|.))
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import Data.Word (Word8, Word32)
import Foreign.C.String (peekCString, withCStringLen)
import Foreign.Marshal (allocaBytes, copyBytes, peekArray0, pokeArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import qualified System.IO as IO

import Book hiding (header, people)

instance Storable Header where
    sizeOf _ = 8
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0 :: IO Word8
        a <- peekByteOff p 1 :: IO Word8
        n <- peekByteOff p 2 :: IO Word32
        return $ Header (v `shiftR` 4 .&. 0x0F)
                        (v `shiftR` 2 .&. 0x03)
                        (v `shiftR` 0 .&. 0x03) 
                        (a .&. 0x80 == 0x80)
                        (a .&. 0x40 == 0x40)
                        (a .&. 0x20 == 0x20)
                        (a .&. 0x10 == 0x10)
                        (a .&. 0x08 == 0x08)
                        n
    poke p (Header v m s a0 a1 a2 a3 a4 n) = do
        pokeByteOff p 0  ((v  `shift` 4) .|. (m `shift` 2)  .|. s :: Word8)
        pokeByteOff p 1  (bit a0 7 
                             .|. (bit a1 6)
                             .|. (bit a2 5)
                             .|. (bit a3 4)
                             .|. (bit a4 3))
        pokeByteOff p 2 n
      where
        bit :: Bool -> Int -> Word8
        bit True  b = 1 `shift` b
        bit False _ = 0

instance Storable Person where
    sizeOf _ = 360
    alignment _ = 8
    peek p = Person <$> peekByteOffUTF8String p 0 128
                    <*> peekByteOff p 128
                    <*> peekByteOff p 352
    poke p (Person n a s) = do
        pokeUTF8StringByteOff p 0 128 n
        pokeByteOff p 128 a
        pokeByteOff p 352 s

instance Storable Address where
    sizeOf _ = 224
    alignment _ = 8
    peek p = Address <$> peekByteOffUTF8String p   0 128
                     <*> peekByteOffUTF8String p 128  64
                     <*> peekByteOffUTF8String p 192  16
                     <*> peekByteOff p 208
                     <*> peekByteOff p 216
    poke p (Address r c pc la lo) = do
        pokeUTF8StringByteOff p   0 128 r
        pokeUTF8StringByteOff p 128  64 c
        pokeUTF8StringByteOff p 192  16 pc 
        pokeByteOff           p 208     la
        pokeByteOff           p 216     lo

peekByteOffCString :: Ptr a -> Int -> Int -> IO String
peekByteOffCString ptr off cap =
    --bracket (mallocBytes cap) free $ \tptr -> do
    allocaBytes cap $ \tptr -> do
        copyBytes tptr (ptr `plusPtr` off) (cap - 1)
        void $ S.memset (castPtr tptr `plusPtr` (cap - 1)) 0 1
        peekCString tptr

peekByteOffUTF8String :: Ptr a -> Int -> Int -> IO String
peekByteOffUTF8String ptr off cap =
    --bracket (mallocBytes cap) free $ \wptr -> do
    allocaBytes cap $ \wptr -> do
        copyBytes wptr (ptr `plusPtr` off) (cap - 1)
        void $ S.memset (castPtr wptr `plusPtr` (cap - 1)) 0 1
        decode <$> peekArray0 0 wptr

pokeCStringByteOff :: Ptr a -> Int -> Int -> String -> IO ()
pokeCStringByteOff ptr off cap s = withCStringLen s $ \(cptr,clen) -> do
    let p = ptr `plusPtr` off
    void $ S.memset (castPtr p) 0 (fromIntegral cap)
    copyBytes p cptr (min clen (cap - 1))

pokeUTF8StringByteOff :: Ptr a -> Int -> Int -> String -> IO ()
pokeUTF8StringByteOff ptr off cap s = do
    let p = ptr `plusPtr` off
    void $ S.memset (castPtr p) 0 (fromIntegral cap)
    pokeArray p $ take (cap - 1) . encode $ s

readHeaderF :: Ptr Header -> IO Header
readHeaderF = peek

readPeopleF :: Word32 -> Ptr Person -> IO [Person]
readPeopleF 0 _ = return []
readPeopleF n ptr = do
    p  <- peek ptr
    ps <- readPeopleF (n-1) (ptr `plusPtr` sizeOf (undefined :: Person))
    return (p:ps)

readF :: FilePath -> IO (Header, [Person])
readF f = do
    bs <- S.readFile f
    S.unsafeUseAsCString bs $ \p -> do
        header <- readHeaderF (castPtr p)
        people <- readPeopleF (number header) (castPtr p `plusPtr` sizeOf (undefined :: Header))
        return (header,people)

writeF :: FilePath -> (Header, [Person]) -> IO ()
writeF f (header, people) = IO.withFile f IO.WriteMode $ \h -> do
    bsh <- S.create (sizeOf (undefined :: Header)) $ \p ->
        poke (castPtr p) header
    bsp <- flip mapM people $ \person ->
        S.create (sizeOf (undefined :: Person)) $ \p ->
            poke (castPtr p) person 
    mapM_ (S.hPut h) (bsh:bsp)
