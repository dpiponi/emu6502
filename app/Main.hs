{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Array.IO
import Data.Word
import Control.Monad.State
import Control.Lens
import Data.Bits
import Data.ByteString as B hiding (putStrLn)
import System.IO
import Data.Binary.Get
import Data.Binary
import System.Console.CmdArgs
import Numeric

import Core

data Args = Args { verbose :: Bool,
                   file :: String,
                   org :: String,
                   entry :: String } deriving (Show, Data, Typeable)

clargs = Args { verbose = False, org = "0", entry = "0", file = "test.bin" }

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

main = do
    args <- cmdArgs clargs
    print args
    arr <- newArray (0, 0x10000) 0 :: IO (IOUArray Int Word8)
    handle <- openFile (file args) ReadMode
    n <- hGetArray handle arr 0x10000
    putStrLn $ "Read " ++ show n ++ " bytes"
    hClose handle
    let [(origin, _)] = readHex (org args)

    putStrLn $ "Relocating to " ++ showHex origin ""
    forM_ [(n-1), (n-2)..0] $ \i -> do
        b <- readArray arr i
        writeArray arr (i+origin) b
        
    let [(entryPoint, _)] = readHex (entry args)
    let state = S { _mem = arr,  _clock = 0, _regs = R entryPoint 0 0 0 0 0,
                    _debug = verbose args}
    flip runStateT state (forever step) -- (times 20 step)
