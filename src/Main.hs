module Main where

import Control.Applicative ((<*>))
import Control.Exception (IOException, catch)
import Data.List (foldl')
import qualified Data.Map.Lazy as Map
import Prelude hiding (read)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure), exitWith)
import qualified System.IO as IO
import qualified System.IO.Error as IO

import Book hiding (header, people)
import BookBinary
import BookBinaryStrict
import BookCereal
import BookForeign

-- 
-- main
--

data Context = Context
    { mode  :: Mode
    , file  :: FilePath
    , read  :: FilePath -> IO (Header, [Person])
    , write :: FilePath -> (Header, [Person]) -> IO ()
    , serialize :: FilePath -> Book -> IO ()
    , deserialize :: FilePath -> IO Book
    }

data Mode = Read | Write | Serialize | Deserialize

defaultContext :: Context
defaultContext = Context Read "" readDefault writeDefault serializeDefault deserializeDefault

argspec :: [OptDescr (Context -> Context)]
argspec =
    [ Option ['r'] ["read"]          (ReqArg setRead        "FILE") "file to read"
    , Option ['w'] ["write"]         (ReqArg setWrite       "FILE") "file to write"
    , Option ['s'] ["serialize"]     (ReqArg setSerialize   "FILE") "file to serialize"
    , Option ['d'] ["deserialize"]   (ReqArg setDeserialize "FILE") "file to deserialize"
    , Option ['f'] ["foreign"]       (NoArg  setForeign)            "use Foreign to read and write"
    , Option ['b'] ["binary"]        (NoArg  setBinary)             "use Data.Binary to read and write"
    , Option []    ["binary-strict"] (NoArg  setBinaryStrict)       "use Data.Binary.Strict to only read"
    , Option ['c'] ["cereal"]        (NoArg  setCereal)             "use Data.Serialize to read and write"
    ]

readDefault :: FilePath -> IO (Header, [Person])
readDefault _ = ioError $ IO.userError "this library has no functions to read from binary file" 

writeDefault :: FilePath -> (Header, [Person]) -> IO ()
writeDefault _ _ = ioError $ IO.userError "this library has no functions to write in binary file" 

serializeDefault :: FilePath -> Book -> IO ()
serializeDefault _ _ = ioError $ IO.userError "this library has no functions to serialize Haskell data" 

deserializeDefault :: FilePath -> IO Book
deserializeDefault _ = ioError $ IO.userError "this library has no functions to deserialize Haskell data" 

setRead :: String -> Context -> Context
setRead f c = c { mode = Read, file = f }

setWrite :: String -> Context -> Context
setWrite f c = c { mode = Write, file = f }

setSerialize :: String -> Context -> Context
setSerialize f c = c { mode = Serialize, file = f }

setDeserialize :: String -> Context -> Context
setDeserialize f c = c { mode = Deserialize, file = f }

setForeign :: Context -> Context
setForeign c = c { read = readF, write = writeF }

setBinary :: Context -> Context
setBinary c = c { read = readB, write = writeB, serialize = serializeB, deserialize = deserializeB }

setBinaryStrict :: Context -> Context
setBinaryStrict c = c { read = readBS }

setCereal :: Context -> Context
setCereal c = c { read = readC, write = writeC, serialize = serializeC, deserialize = deserializeC }

bookData :: (Header, [Person])
bookData =
    ( Header 1 2 3 True False False True False 3
    , [ Person "foo" (Address "aaaaaa" "tokyo" "111-1111" 1.1 1.2) 61
      , Person "bar" (Address "bbbbbb" "osaka" "222-2222" 2.1 2.2) 62
      , Person "baz" (Address "cccccc" "kyoto" "333-3333" 3.1 3.2) 63
      ]
    )

book :: Book
book = Book header people
  where
    (header, people') = bookData
    people = foldl' (\m person -> Map.insert (name person) person m) Map.empty people'

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    let header = "Usage: " ++ prog ++ " [OPTIONS]\n"
        usage  = usageInfo header argspec
    case getOpt Permute argspec args of
      (os, _, []  ) -> let ctx = foldl (.) id os defaultContext
                       in if null (file ctx)
                              then die ("-r or -w is required.\n" ++ usage) 1
                              else catch (process ctx) $ \e -> die (show (e::IOException) ++ "\n" ++ usage) 1
      (_,  _, errs) -> die (concat errs ++ usage) 1

process :: Context -> IO ()
process ctx = case mode ctx of
    Read -> do
        (header, people) <- read <*> file $ ctx
        print header
        print people
    Write -> (write <*> file $ ctx) bookData
    Serialize -> (serialize <*> file $ ctx) book
    Deserialize -> print =<< (deserialize <*> file $ ctx)

die :: String -> Int -> IO a
die s c = IO.hPutStr IO.stderr s >> exitWith (ExitFailure c)
