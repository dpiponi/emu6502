{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Array.IO
import Data.Word
import Control.Monad.State
import Control.Lens
import Data.Bits
import Data.ByteString as B
import System.IO
import Data.Binary.Get
import Data.Binary
import System.Console.CmdArgs

import Core

data Args = Args { debugFlag :: Bool } deriving (Show, Data, Typeable)

clargs = Args { debugFlag = False}

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

main = do
    args <- cmdArgs clargs
    print args
    arr <- newArray (0, 0x10000) 0 :: IO (IOUArray Int Word8)
    handle <- openFile "test.bin" ReadMode
    n <- hGetArray handle arr 128
--     print $ "read " ++ show n ++ " bytes"
    hClose handle
--    print contents
--   let n = B.length contents
--    let dat = B.unpack contents
--    arr <- listArray (0, n-1) dat
    --print arr
    --writeArray arr 0x0000 0x01
    let state = S { _mem = arr,  _clock = 0, _regs = R 0 0 0 0 0 0,
                    _debug = debugFlag args}
    flip runStateT state (forever step)
