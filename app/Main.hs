{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Array.IO
import Data.Word
import Control.Monad.State
import Control.Lens hiding (noneOf)
import Data.Bits
import Data.ByteString as B hiding (putStrLn, count, head)
import System.IO
import Data.Binary.Get
import Text.Parsec
import Data.Binary
import System.Console.CmdArgs
import Numeric
import Intel hiding (hexWord16, fromHex)
import Binary
import Control.Monad.Loops

import Core

data Args = Args { verbose :: Bool,
                   file :: String,
                   org :: String,
                   entry :: String } deriving (Show, Data, Typeable)

clargs = Args { verbose = False, org = "0", entry = "0", file = "test.bin" }

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

data FileSpec = Intel String | Binary String Word16 deriving Show

hexWord16 :: Stream s m Char => ParsecT s u m Word16
hexWord16 = fromHex <$> count 4 hexDigit

fromHex :: (Num a, Eq a) => String -> a
fromHex = fst . head . readHex

filespec = do
    a <- anyChar
    case a of
        'i' -> do
            char ':'
            filename <- many (noneOf ",")
            return (Intel filename)
        'b' -> do
            char ':'
            filename <- many (noneOf ":")
            char ':'
            address <- hexWord16
            return (Binary filename address)

filespecs = filespec `sepBy1` (char ',')

test = do
    let Right u = parse filespecs "" "b:BASIC:8000,i:ROM"
    print u

loadFile arr (Intel f) = readIntel arr f
loadFile arr (Binary f o) = readBinary arr f o

main = do
    test

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    -- hSetEcho stdin False
    args <- cmdArgs clargs
    print args

    arr <- newArray (0, 0xffff) 0 :: IO (IOUArray Int Word8)

    --arr <- newArray (0, 0x10000) 0 :: IO (IOUArray Int Word8)
    --handle <- openFile (file args) ReadMode
    --n <- hGetArray handle arr 0x10000
    --putStrLn $ "Read " ++ show n ++ " bytes"
    --hClose handle
    --let [(origin, _)] = readHex (org args)

    --putStrLn $ "Relocating to " ++ showHex origin ""
    --forM_ [(n-1), (n-2)..0] $ \i -> do
    --    b <- readArray arr i
    --    writeArray arr (i+origin) b

    arr <- newArray (0, 0xffff) 0 :: IO (IOUArray Int Word8)

    --readIntel arr (file args)
    putStrLn $ "Running from " ++ file args
    let Right specs = parse filespecs "" (file args)
    forM_ specs $ \spec ->
        loadFile arr spec
        
    let [(entryPoint, _)] = readHex (entry args)
    let state = S { _mem = arr,  _clock = 0, _regs = R entryPoint 0 0 0 0 0xff,
                    _debug = verbose args}
    
    putStrLn $ "Executing from 0x" ++ showHex entryPoint ""
    -- flip runStateT state (forever step) -- (times 20 step)
    {-
    flip execStateT state $ do
                                iterateUntil id $ do
                                    step
                                    brk <- aboutToBrk
                                    addr <- use (regs . pc)
                                    return $ brk && addr/=0x09d1
                                debug .= True
                                dumpState
                                liftIO $ print "Done"
    -}
    flip execStateT state $ forever step
