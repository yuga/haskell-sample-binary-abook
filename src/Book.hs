{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Book where

import Data.Map.Lazy (Map)
import Data.Word (Word8, Word16, Word32)
import GHC.Generics (Generic)

--import Debug.Trace

--
-- File Format (test.dat)
--
-- most significant bit is numbered 0.
--
-- --------
-- (Header)
-- version   |   4 bits  | natural number
-- type      |   2 bits  | natural number
-- subtype   |   2 bits  | natural number
-- attr1     |   1 bits  | boolean 
-- attr2     |   1 bits  | boolean
-- attr3     |   1 bits  | boolean
-- attr4     |   1 bits  | boolean
-- attr5     |   1 bits  | boolean
-- unused    |   3 bits  | reserved
-- number    |   4 bytes | natural number (littele endian)
-- padding   |   2 bytes | unused
-- --------
-- (Person)
-- name      | 128 bytes | null terminated string
-- road      | 128 bytes | null terminated string
-- city      |  64 bytes | null terminated string
-- postcode  |  16 bytes | null terminated string
-- latitude  |   8 bytes | 64bit little endian IEEE 754 format
-- longitude |   8 bytes | 64bit little endian IEEE 754 format
-- salary    |   2 bytes | natural number (littele endian)
-- padding   |   6 bytes | unused
-- ----
-- .. PersonをHeaderのnumberの値だけ繰り返し ..
-- --------

data Header = Header
    { version  :: !Word8
    , mtype    :: !Word8
    , stype    :: !Word8
    , attr0    :: !Bool
    , attr1    :: !Bool
    , attr2    :: !Bool
    , attr3    :: !Bool
    , attr4    :: !Bool
    , number   :: !Word32
    }
    deriving (Show)

data Person = Person
    { name     :: !String
    , address  :: !Address
    , salary   :: !Word16
    }
    deriving (Show)

data Address = Address
    { road      :: !String
    , city      :: !String
    , postcode  :: !String
    , latitude  :: !Double
    , longitude :: !Double
    }
    deriving (Show)

data Book = Book Header People
          deriving (Generic, Show)

type People = Map String Person
