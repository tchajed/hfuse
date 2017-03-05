module Main where

import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse

import Control.Applicative
import Options

import Foreign.Marshal.Alloc (allocaBytes)
import Control.Concurrent.MVar

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

ack :: Int -> Int -> Int
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

data Computation =
    Fibonacci Int
  | Ackermann Int
  | Alloc Int
  | Noop

data State = State
  { lock :: MVar () }

newState :: IO State
newState = pure State <*> newMVar ()

data Operation = Operation
  { computation :: Computation
  , doLock :: Bool }

doComputation :: Computation -> IO ()
doComputation (Fibonacci n) = fib n `seq` return ()
doComputation (Ackermann n) = ack 3 n `seq` return ()
doComputation (Alloc n) = allocaBytes n (\_ -> return ())
doComputation Noop = return ()

doOperation :: Operation -> State -> IO ()
doOperation (Operation comp doLock) s =
  if doLock then
    withMVar (lock s) $ \_ -> doComputation comp
  else
    doComputation comp

data FsOptions = FsOptions
  { fibonacci :: Int
  , ackermann :: Int
  , allocbytes :: Int
  , optGlobalLock :: Bool }

instance Options FsOptions where
  defineOptions = pure FsOptions
    <*> simpleOption "fib" 0
       "argument for fibonacci"
    <*> simpleOption "ack" 0
       "argument for ackermann(3, n)"
    <*> simpleOption "alloc" 0
       "bytes to allocate"
    <*> simpleOption "lock" False
       "acquire global lock for each operation"

parseComputation :: Int -> Int -> Int -> Either String Computation
parseComputation fibN ackN bytes =
  let numPassed = sum $ map fromEnum [fibN>0, ackN>0, bytes>0] in
    case numPassed of
      0 -> Right Noop
      1 -> case () of _
                        | fibN > 0 -> Right $ Fibonacci fibN
                        | ackN > 0 -> Right $ Ackermann ackN
                        | bytes > 0 -> Right $ Alloc bytes
      _ -> Left "multiple computations chosen"

parseOperation :: FsOptions -> Either String Operation
parseOperation (FsOptions fibN ackN bytes doLock) = do
  c <- parseComputation fibN ackN bytes
  return $ Operation c doLock

main :: IO ()
main = runCommand $ \opts args -> do
  case parseOperation opts of
    Left err -> error err
    Right op -> do
      s <- newState
      fuseRun "slowfs" args (slowFSOps op s) defaultExceptionHandler

type HT = ()

-- TODO: have a file /default that is configurable from command line
-- support /fib/<n> to calculate fibonacci
-- support /alloc/<n> to allocate bytes
-- maybe list default, fib/, alloc/ in ReadDirectory

slowFSOps :: Operation -> State -> FuseOperations HT
slowFSOps op s = defaultFuseOps { fuseGetFileStat = slowGetFileStat op s
                                , fuseOpen        = slowOpen
                                , fuseOpenDirectory = slowOpenDirectory
                                , fuseReadDirectory = slowReadDirectory
                                , fuseGetFileSystemStats = slowGetFileSystemStats
                                }

slowPath :: FilePath
slowPath = "/default"

slowOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
slowOpen path mode flags
    | path == slowPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = 0
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

slowGetFileStat :: Operation -> State -> FilePath -> IO (Either Errno FileStat)
slowGetFileStat _ _ "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx

-- this is the slow operation
slowGetFileStat op s path | path == slowPath = do
    doOperation op s
    ctx <- getFuseContext
    return $ Right $ fileStat ctx

slowGetFileStat _ _ _ =
    return $ Left eNOENT

slowOpenDirectory "/" = return eOK
slowOpenDirectory _   = return eNOENT

slowReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
slowReadDirectory "/" = do
    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(slowName,    fileStat ctx)
                   ]
    where (_:slowName) = slowPath
slowReadDirectory _ = return (Left (eNOENT))

slowGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
slowGetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
