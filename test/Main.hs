module Main where

import Data.Array.IO
import Test.HUnit
import Data.Word
import Data.Bits
import Control.Monad.State
import Control.Monad
import Control.Lens
import Control.Monad.Loops
import System.IO
import Numeric

import Core

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

test1 = do
    let ins = [
            0xa9, 0xff,         -- LDA #$FF
            0x29, 0xf0,         -- AND #$F0
            0x0d, 0x09, 0x00,   -- ORA DATA1
            0x49, 0xf7,         -- EOR $F7
            0x07                -- DATA1: .BYTE $07
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    state' <- flip execStateT state (do
        step
        step
        step
        step)
    assertEqual "A == 0" (state' ^. regs . a) 0
    assertEqual "clock == 10" (state' ^. clock) 10

test2 = do
    let ins = [
            0xa5, 0x06,         -- LDA Z:DATA1
            0x01, 0x07,         -- ORA (ADDR1, X)
            0x51, 0x09,         -- EOR (ADDR2), Y
            0x55,               -- DATA1: .BYTE $55
            0x0b, 0x00,         -- ADDR1: .WORD DATA2
            0x0c, 0x00,         -- ADDR2: .WORD DATA3
            0xaa,               -- DATA2: .BYTE $AA
            0xff                -- DATA3: .BYTE $FF
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    state' <- flip execStateT state (do
        step
        step
        step)
    assertEqual "A == 0x00" (state' ^. regs . a) 0x00
    assertEqual "clock == 14" (state' ^. clock) 14

testAdd i j k (fC, fS, fV) = do
    let ins = [
            0xa9, 0x06,         -- LDA #0x06
            0x69, 0x07          -- ADC #0x07
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    writeArray arr 1 i
    writeArray arr 3 j
    state' <- flip execStateT state (do
        step
        step)
    let rA = state' ^. regs . a
    let rC = state' ^. regs . flagC
    let rS = state' ^. regs . flagS
    let rV = state' ^. regs . flagV
    assertEqual "A" rA k
    assertEqual "C" rC fC
    assertEqual "S" rS fS
    assertEqual "V" rV fV

test3 = do
    -- Examples from
    -- http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    testAdd 0x50 0x10 0x60 (False, False, False)
    testAdd 0x50 0x50 0xa0 (False, True, True)
    testAdd 0x50 0x90 0xe0 (False, True, False)
    testAdd 0x50 0xd0 0x20 (True, False, False)
    testAdd 0xd0 0x10 0xe0 (False, True, False)
    testAdd 0xd0 0x50 0x20 (True, False, False)
    testAdd 0xd0 0x90 0x60 (True, False, True)
    testAdd 0xd0 0xd0 0xa0 (True, True, False)

testSub i j k (fC, fS, fV) = do
    let ins = [
            0xa9, 0x06,         -- LDA #0x06
            0xe9, 0x07          -- SBC #0x07
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0x1, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    writeArray arr 1 i
    writeArray arr 3 j
    state' <- flip execStateT state (do
        step
        step)
    let rA = state' ^. regs . a
    let rC = state' ^. regs . flagC
    let rS = state' ^. regs . flagS
    let rV = state' ^. regs . flagV
    assertEqual "A" rA k
    assertEqual "C" rC fC
    assertEqual "S" rS fS
    assertEqual "V" rV fV

test4 = do
    -- Examples from
    -- http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    testSub 0x50 0xf0 0x60 (False, False, False)
    testSub 0x50 0xb0 0xa0 (False, True, True)
    testSub 0x50 0x70 0xe0 (False, True, False)
    testSub 0x50 0x30 0x20 (True, False, False)
    testSub 0xd0 0xf0 0xe0 (False, True, False)
    testSub 0xd0 0xb0 0x20 (True, False, False)
    testSub 0xd0 0x70 0x60 (True, False, True)
    testSub 0xd0 0x30 0xa0 (True, True, False)

test5 = do
    let ins = [
            0xad, 0x08, 0x00,   -- LDA DATA1
            0x69, 0x02,         -- ADC #$02
            0x8d, 0x08, 0x00,   -- STA DATA1
            0xff                -- DATA1: .BYTE $ff
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    state' <- flip execStateT state (do
        step
        step
        step)
    assertEqual "A == 1" (state' ^. regs . a) 1 -- XXX check memory not A
    assertEqual "clock == 10" (state' ^. clock) 10
    let fC = state' ^. regs . flagC
    assertEqual "C" fC True

test6 = do
    let ins = [
            0x2e, 0x0c, 0x00,   -- ROL DATA1
            0x2e, 0x0d, 0x00,   -- ROL DATA2
            0x2e, 0x0e, 0x00,   -- ROL DATA3
            0x2e, 0x0f, 0x00,   -- ROL DATA4
            0xef,               -- DATA1: .BYTE $EF
            0xcd,               -- DATA1: .BYTE $CD
            0xab,               -- DATA1: .BYTE $AB
            0x89                -- DATA1: .BYTE $89
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    state' <- flip execStateT state (do
        step
        step
        step
        step)
    let m = state' ^. mem
    m1 <- readArray m 0xc
    m2 <- readArray m 0xd
    m3 <- readArray m 0xe
    m4 <- readArray m 0xf
    assertEqual "m1 == 0xde" m1 0xde
    assertEqual "m2 == 0x9b" m2 0x9b
    assertEqual "m3 == 0x57" m3 0x57
    assertEqual "m4 == 0x13" m4 0x13
    assertEqual "clock == 24" (state' ^. clock) 24

readWord32 :: IOUArray Int Word8 -> Word16 -> IO Word32
readWord32 m p = do
    b0 <- readArray m (fromIntegral p)
    b1 <- readArray m (fromIntegral (p+1))
    b2 <- readArray m (fromIntegral (p+2))
    b3 <- readArray m (fromIntegral (p+3))
    return $ fromIntegral b0+(fromIntegral b1 `shift` 8)+(fromIntegral b2 `shift` 16)+(fromIntegral b3 `shift` 24)

test7 = do
    let ins = [
            0xa9, 0x00,         -- LDA #$00 (2 clocks)
            0x69, 0x01,         -- LOOP: ADC #$01 (2 clocks)
            0x4c, 0x02, 0x00    -- JMP LOOP (3 clocks)
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    let n = 255
    state' <- flip execStateT state (times (2*n) step)
    let m = state' ^. mem
    assertEqual "A == n" (state' ^. regs . a) (i8 n)
    assertEqual "clock == 5*n-1" (state' ^. clock) (fromIntegral $ 5*n-1)

test8 = do
    let ins = [
            0xa9, 0x00,         -- LDA #$00 (2 clocks)
            0x69, 0x01,         -- LOOP: ADC #$01 (2 clocks)
            0x50, 0xfc          -- BVC LOOP (2* clocks)
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    state' <- flip execStateT state (times 257 step)
    let m = state' ^. mem
    assertEqual "A == 0x80" (state' ^. regs . a) 0x80
    assertEqual "clock == 5*128" (state' ^. clock) (fromIntegral $ 5*128+1)

testDiv i j k = do
    let ins = [
                0xa9, i,           -- LDA i
                0x8d, 0x2a, 0x00,  -- STA NUM
                0xa9, j,           -- LDA j
                0x8d, 0x2b, 0x00,  -- STA DEN
                0x20, 0x13, 0x00,  -- JSR DIV
                0xad, 0x2a, 0x00,  -- LDA NUM
                0x4c, 0x10, 0x00,  -- LOOP: JMP LOOP
                0xa9, 0x00,        -- DIV: LDA #$00 
                0xa2, 0x08,        -- LDX #$08 
                0x0e, 0x2a, 0x00,  -- ASL NUM
                0x2a,              -- ROL A    
                0xcd, 0x2b, 0x00,  -- CMP DEN
                0x90, 0x03,        -- BCC $0023
                0xed, 0x2b, 0x00,  -- SBC DEN
                0x2e, 0x2a, 0x00,  -- ROL NUM
                0xca,              -- DEX      
                0xd0, 0xf1,        -- BNE $001A
                0x60,              -- RTS      
                0x00,              -- NUM: .BYTE 0
                0x00               -- DEN: .BYTE 0
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    state' <- flip execStateT state (times 256 step)
    let rA = state' ^. regs . a
    assertEqual "A" rA k

test9 = do
    forM_ [0,17..255] $ \i ->
        forM_ [1,3..24] $ \j -> do
            testDiv i j (i `div` j)

test10 = do
    let ins = [
                0xca,   -- DEX
                0xca,   -- DEX
                0x8a,   -- TXA
                0xa8,   -- TAY
                0x00    -- BRK
            ]
    arr <- newListArray (0, 2047) ins :: IO (IOUArray Int Word8)
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0, _p=0, _a=0, _x=0, _y=0, _s=0 },
                    _debug=False}
    state' <- flip execStateT state $ iterateUntil id $ do
                                        step
                                        aboutToBrk
    let rY = state' ^. regs . y
    assertEqual "Y" 0xfe rY

runFile :: String -> Bool -> IO State6502
runFile fileName verbose = do
    arr <- newArray (0, 0x10000) 0 :: IO (IOUArray Int Word8)
    handle <- openFile fileName ReadMode
    n <- hGetArray handle arr 0x10000
    hClose handle
    let state = S { _mem = arr,  _clock = 0,
                    _regs = R { _pc=0x0000, _p=0, _a=0, _x=0, _y=0, _s=0xff },
                    _debug = verbose }
    state' <- flip execStateT state $ iterateUntil id $ do
                                        step
                                        aboutToBrk
    return state'

test11 = do
    state <- runFile "test1.bin" False
    let ra = state ^. regs . a
    assertEqual "a" 0x33 ra

test12 = do
    state <- runFile "test12.bin" False
    let resultAddr = 0x001b
    sum <- readWord32 (state ^. mem) resultAddr
    assertEqual "sum=11111110" 0x11111110 sum

test13 = do
    state <- runFile "test13.bin" False
    let rA = state ^. regs . a
    assertEqual "A" 33 rA

test14 = do
    state <- runFile "test14.bin" False
    let rA = state ^. regs . a
    assertEqual "A" 1 rA

test15 = do
    state <- runFile "test15.bin" True
    let rA = state ^. regs . a
    assertEqual "A" 0x0d rA

tests = TestList [TestLabel "test1" (TestCase test1),
                  TestLabel "test2" (TestCase test2),
                  TestLabel "test3" (TestCase test3),
                  TestLabel "test4" (TestCase test4),
                  TestLabel "test5" (TestCase test5),
                  TestLabel "test6" (TestCase test6),
                  TestLabel "test7" (TestCase test7),
                  TestLabel "test8" (TestCase test8),
                  TestLabel "test9" (TestCase test9),
                  TestLabel "test10" (TestCase test10),
                  TestLabel "test11" (TestCase test11),
                  TestLabel "test12" (TestCase test12),
                  TestLabel "test13" (TestCase test13),
                  TestLabel "test14" (TestCase test14),
                  TestLabel "test15" (TestCase test15)]

main = do
    runTestTT tests
