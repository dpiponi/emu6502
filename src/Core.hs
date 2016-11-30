{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE Strict #-}

-- http://nesdev.com/6502_cpu.txt

module Core(Emu6502(..),step,i8,i16,make16) where
--module Core where

import Data.Array.IO
import Data.Word
import Control.Monad.State
import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.ByteString as B hiding (putStr, putStrLn, getLine, length)
import System.IO
import Data.Binary.Get
import Data.Binary
import Data.Int
import Numeric
import qualified Data.ByteString.Internal as BS (c2w, w2c)

class (Monad m, MonadIO m) => Emu6502 m where
    readMemory :: Word16 -> m Word8
    writeMemory :: Word16 -> Word8 -> m ()
    getPC :: m Word16
    tick :: Int -> m ()
    putC :: Bool -> m ()
    getC :: m Bool
    putZ :: Bool -> m ()
    getZ :: m Bool
    putI :: Bool -> m ()
    getI :: m Bool
    putD :: Bool -> m ()
    getD :: m Bool
    putB :: Bool -> m ()
    getB :: m Bool
    putV :: Bool -> m ()
    getV :: m Bool
    putN :: Bool -> m ()
    getN :: m Bool
    getA :: m Word8
    putA :: Word8 -> m ()
    getS :: m Word8
    putS :: Word8 -> m ()
    getX :: m Word8
    putX :: Word8 -> m ()
    getP :: m Word8
    putP :: Word8 -> m ()
    getY :: m Word8
    putY :: Word8 -> m ()
    putPC :: Word16 -> m ()
    addPC :: Int -> m ()
    illegal :: Word8 -> m ()

    debugStr :: String -> m ()
    debugStrLn :: String -> m ()

{-# INLINABLE dumpRegisters #-}
dumpRegisters :: Emu6502 m => m ()
dumpRegisters = do
    -- XXX bring clock back
    --tClock <- use clock
    --debugStr $ "clock = " ++ show tClock
    regPC <- getPC
    debugStr $ " pc = " ++ showHex regPC ""
    regP <- getP
    debugStr $ " flags = " ++ showHex regP ""
    debugStr $ "(N=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
    debugStr $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
    debugStr $ ",B=" ++ showHex (regP `shift` ((-4)) .&. 1) ""
    debugStr $ ",D=" ++ showHex (regP `shift` ((-3)) .&. 1) ""
    debugStr $ ",I=" ++ showHex (regP `shift` ((-2)) .&. 1) ""
    debugStr $ ",Z=" ++ showHex (regP `shift` ((-1)) .&. 1) ""
    debugStr $ ",C=" ++ showHex (regP .&. 1) ""
    regA <- getA 
    debugStr $ ") A = " ++ showHex regA ""
    regX <- getX
    debugStr $ " X = " ++ showHex regX ""
    regY <- getY
    debugStrLn $ " Y = " ++ showHex regY ""
    regS <- getS
    debugStrLn $ " N = " ++ showHex regS ""

{-# INLINABLE dumpMemory #-}
dumpMemory :: Emu6502 m => m ()
dumpMemory = do
    regPC <- getPC
    b0 <- readMemory regPC
    b1 <- readMemory (regPC+1)
    b2 <- readMemory (regPC+2)
    debugStr $ "(PC) = "
    debugStr $ showHex b0 "" ++ " "
    debugStr $ showHex b1 "" ++ " "
    debugStrLn $ showHex b2 ""

{-# INLINABLE dumpState #-}
dumpState :: Emu6502 m => m ()
dumpState = do
    dumpMemory
    dumpRegisters

{-# INLINE make16 #-}
make16 :: Word8 -> Word8 -> Word16
make16 lo hi = (i16 hi `shift` 8)+i16 lo

{-# INLINE incPC #-}
incPC :: Emu6502 m => m ()
incPC = addPC 1

{-# INLINABLE read16 #-}
read16 :: Emu6502 m => Word16 -> m Word16
read16 addr = do
    lo <- readMemory addr
    hi <- readMemory (addr+1)
    return $ make16 lo hi

{-# INLINABLE read16zp #-}
read16zp :: Emu6502 m => Word8 -> m Word16
read16zp addr = do
    lo <- readMemory (i16 addr)
    hi <- readMemory (i16 addr+1)
    return $ make16 lo hi

-- http://www.emulator101.com/6502-addressing-modes.html

{-# INLINE i8 #-}
i8 :: Integral a => a -> Word8
i8 = fromIntegral

{-# INLINE i16 #-}
i16 :: Integral a => a -> Word16
i16 = fromIntegral

{-# INLINE iz #-}
iz :: Integral a => a -> Int
iz = fromIntegral

{-# INLINABLE writeIndirectX #-}
writeIndirectX :: Emu6502 m => Word8 -> m ()
writeIndirectX src = do
    --incPC
    p0 <- getPC
    offsetX <- getX
    zpAddr <- readMemory p0
    addr <- read16zp (zpAddr+offsetX)
    writeMemory addr src
    putPC $ p0+1
    tick 6

{-# INLINABLE writeZeroPage #-}
writeZeroPage :: Emu6502 m => Word8 -> m ()
writeZeroPage src = do
    --incPC
    p0 <- getPC
    addr <- readMemory p0
    writeMemory (i16 addr) src
    putPC $ p0+1
    tick 3

{-# INLINABLE writeAbsolute #-}
writeAbsolute :: Emu6502 m => Word8 -> m()
writeAbsolute src = do
    --incPC
    p0 <- getPC
    addr <- read16 p0
    writeMemory addr src
    putPC $ p0+2
    tick 4

{-# INLINABLE writeIndirectY #-}
writeIndirectY :: Emu6502 m => Word8 -> m ()
writeIndirectY src = do
    --incPC
    p0 <- getPC
    offsetY <- getY
    addr <- readMemory p0 >>= read16zp
    writeMemory (addr+i16 offsetY) src
    putPC $ p0+1
    tick 6

{-# INLINABLE writeZeroPageX #-}
writeZeroPageX :: Emu6502 m => Word8 -> m ()
writeZeroPageX src = do
    --incPC
    p0 <- getPC
    offsetX <- getX
    zpAddr <- readMemory p0
    writeMemory (i16 $ zpAddr+offsetX) src
    putPC $ p0+1
    tick 4

{-# INLINABLE writeAbsoluteY #-}
writeAbsoluteY :: Emu6502 m => Word8 -> m ()
writeAbsoluteY src = do
    --incPC
    p0 <- getPC
    offsetY <- getY
    baseAddr <- read16 p0
    writeMemory (baseAddr+i16 offsetY) src
    putPC $ p0+2
    tick 5

{-# INLINABLE writeAbsoluteX #-}
writeAbsoluteX :: Emu6502 m => Word8 -> m ()
writeAbsoluteX src = do
    --incPC
    p0 <- getPC
    offsetX <- getX
    baseAddr <- read16 p0
    writeMemory (baseAddr+i16 offsetX) src
    putPC $ p0+2
    tick 5

{-# INLINABLE putData #-}
putData :: Emu6502 m => Word8 -> Word8 -> m ()
putData bbb src = do
    p0 <- getPC
    case bbb of
        0b000 -> writeIndirectX src -- (zero page, X)
        0b001 -> writeZeroPage src
        0b010 -> readMemory p0 >>= illegal -- XXX imm. check in caller
        0b011 -> writeAbsolute src
        0b100 -> writeIndirectY src -- (zero page), Y
        0b101 -> writeZeroPageX src
        0b110 -> writeAbsoluteY src
        0b111 -> writeAbsoluteX src

{-# INLINABLE readIndirectX #-}
readIndirectX :: Emu6502 m => m Word8
readIndirectX = do
    --incPC
    p0 <- getPC
    offsetX <- getX
    zpAddr <- readMemory p0
    let addrAddr = zpAddr+offsetX
    addr <- read16zp addrAddr
    src <- readMemory addr
    putPC $ p0+1
    tick 6
    return src

{-# INLINABLE readZeroPage #-}
readZeroPage :: Emu6502 m => m Word8
readZeroPage = do
    --incPC
    p0 <- getPC
    addr <- readMemory p0
    src <- readMemory (i16 addr)
    putPC $ p0+1
    tick 3
    return src

{-# INLINABLE readImmediate #-}
readImmediate :: Emu6502 m => m Word8
readImmediate = do
    --incPC
    p0 <- getPC
    src <- readMemory p0
    putPC $ p0+1
    tick 2
    return src

{-# INLINABLE readAbsolute #-}
readAbsolute :: Emu6502 m => m Word8
readAbsolute = do
    --incPC
    p0 <- getPC
    src <- read16 p0 >>= readMemory
    putPC $ p0+2
    tick 4
    return src

{-# INLINABLE readIndirectY #-}
readIndirectY :: Emu6502 m => m Word8
readIndirectY = do
    --incPC
    p0 <- getPC
    offsetY <- getY
    addr <- readMemory p0 >>= read16zp
    let newAddr = addr+i16 offsetY
    let carry = (newAddr .&. 0xff00) /= (addr .&. 0xff00)
    src <- readMemory (addr+i16 offsetY)
    putPC (p0+1)
    tick $ if carry then 6 else 5
    return src

{-# INLINABLE readZeroPageX #-}
readZeroPageX :: Emu6502 m => m Word8
readZeroPageX = do
    --incPC
    p0 <- getPC
    offsetX <- getX
    zpAddr <- readMemory p0
    src <- readMemory (i16 $ zpAddr+offsetX)
    putPC (p0+1)
    tick 4
    return src

{-# INLINABLE readZeroPageY #-}
readZeroPageY :: Emu6502 m => m Word8
readZeroPageY = do
    --incPC
    p0 <- getPC
    offsetY <- getY
    zpAddr <- readMemory p0
    src <- readMemory (i16 $ zpAddr+offsetY)
    putPC (p0+1)
    tick 4
    return src

{-# INLINABLE readAbsoluteX #-}
readAbsoluteX :: Emu6502 m => m Word8
readAbsoluteX = do
    --incPC
    p0 <- getPC
    offsetX <- getX
    baseAddr <- read16 p0
    let addr = baseAddr+i16 offsetX
    let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
    src <- readMemory addr
    putPC $ p0+2
    tick $ if carry then 5 else 4
    return src

{-# INLINABLE readAbsoluteY #-}
readAbsoluteY :: Emu6502 m => m Word8
readAbsoluteY = do
    --incPC
    p0 <- getPC
    offsetY <- getY
    baseAddr <- read16 p0
    let addr = baseAddr+i16 offsetY
    let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
    src <- readMemory addr
    putPC $ p0+2
    tick $ if carry then 5 else 4
    return src

{-# INLINABLE getData #-}
getData :: Emu6502 m => Word8 -> m Word8
getData bbb = do
    p0 <- getPC
    case bbb of
        0b000 -> readIndirectX
        0b001 -> readZeroPage
        0b010 -> readImmediate
        0b011 -> readAbsolute
        0b100 -> readIndirectY
        0b101 -> readZeroPageX
        0b110 -> readAbsoluteY
        0b111 -> readAbsoluteX

{-# INLINABLE ins_bra #-}
ins_bra :: Emu6502 m => m Bool -> Bool -> m ()
ins_bra getFlag value = do
    --incPC
    f <- getFlag
    p0 <- getPC
    let oldP = p0+1
    if value && f || not value && not f
        then do
            debugStrLn "Taking branch"
            offset <- readMemory p0 -- XXX or ^^^
            let newP = if offset < 0x80 then oldP+i16 offset else oldP+i16 offset-0x100
            tick $ if newP .&. 0xff00 == oldP .&. 0xff00 then 3 else 4
            putPC newP
        else do
            debugStrLn "Not taking branch"
            tick 2
            putPC oldP

{-# INLINABLE ins_set #-}
ins_set :: Emu6502 m => (Bool -> m ()) -> Bool -> m ()
ins_set putFlag value = do
    --incPC
    putFlag value
    getPC >>= putPC
    tick 2

{-# INLINABLE ins_nop #-}
ins_nop :: Emu6502 m => m ()
ins_nop = do
    --incPC
    tick 2


{-# INLINABLE ins_jmp #-}
ins_jmp :: Emu6502 m => m ()
ins_jmp = do
    --incPC
    addr <- getPC >>= read16
    putPC addr
    tick 3

{-# INLINE nonwhite #-}
nonwhite :: Word8 -> String
nonwhite ra | ra < 32 = "()"
nonwhite ra = "'" ++ [BS.w2c ra] ++ "'"

{-# INLINABLE ins_jmp_indirect #-}
ins_jmp_indirect :: Emu6502 m => m ()
ins_jmp_indirect = do
    --incPC
    getPC >>= read16 >>= read16 >>= putPC
    tick 5

withZeroPage :: Emu6502 m => (Word8 -> m Word8) -> m ()
withZeroPage op = do
    --incPC
    p0 <- getPC
    addr <- readMemory p0
    readMemory (i16 addr) >>= op >>= writeMemory (i16 addr)
    tick 5
    putPC $ p0+1

withAccumulator :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAccumulator op = do
    --incPC
    p0 <- getPC
    getA >>= op >>= putA
    tick 2

withAbsolute :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAbsolute op = do
    --incPC
    p0 <- getPC
    addr <- read16 p0
    dst <- readMemory addr >>= op
    putPC $ p0+2
    writeMemory addr dst
    tick 6

withZeroPageX :: Emu6502 m => (Word8 -> m Word8) -> m ()
withZeroPageX op = do
    --incPC
    p0 <- getPC
    offsetX <- getX
    zpAddr <- readMemory p0
    let addr = zpAddr+offsetX
    src <- readMemory (i16 addr)
    putPC $ p0+1
    dst <- op src
    writeMemory (i16 addr) dst
    tick 6

withZeroPageY :: Emu6502 m => (Word8 -> m Word8) -> m ()
withZeroPageY op = do
    --incPC
    p0 <- getPC
    offsetY <- getY
    zpAddr <- readMemory p0
    let addr = zpAddr+offsetY
    src <- readMemory (i16 addr)
    putPC $ p0+1
    dst <- op src
    writeMemory (i16 addr) dst
    tick 6

withAbsoluteX :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAbsoluteX op = do
    --incPC
    p0 <- getPC
    offsetX <- getX
    baseAddr <- read16 p0
    let addr = baseAddr+i16 offsetX
    let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
    src <- readMemory addr
    putPC $ p0+2
    dst <- op src
    writeMemory addr dst
    tick 7

withAbsoluteY :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAbsoluteY op = do
    --incPC
    p0 <- getPC
    offsetY <- getY
    baseAddr <- read16 p0
    let addr = baseAddr+i16 offsetY
    let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
    src <- readMemory addr
    putPC $ p0+2
    dst <- op src
    writeMemory addr dst
    tick 7

-- Need to separate R/W/RW XXX
{-# INLINABLE withData02 #-}
withData02 :: Emu6502 m =>
              Word8 -> Bool ->
              (Word8 -> m Word8) ->
              m ()
withData02 bbb useY op = case bbb of
    0b000 -> getPC >>= readMemory >>= illegal -- XXX reread mem. Should check in caller.
    0b001 -> withZeroPage op
    0b010 -> withAccumulator op
    0b011 -> withAbsolute op
    0b101 -> if useY then withZeroPageY op else withZeroPageX op
    0b111 -> if useY then withAbsoluteY op else withAbsoluteX op

    otherwise -> error "Unknown addressing mode"

{-# INLINABLE getData02 #-}
getData02 :: Emu6502 m =>
              Word8 -> Bool ->
              (Word8 -> m ()) ->
              m ()
getData02 bbb useY op = case bbb of
    0b000 -> readImmediate >>= op
    0b001 -> readZeroPage >>= op
    0b010 -> error "Must write back to A"
    0b011 -> readAbsolute >>= op
    0b101 -> if useY
                then readZeroPageY >>= op
                else readZeroPageX >>= op
    0b111 -> if useY
            then readAbsoluteY >>= op
            else readAbsoluteX >>= op

    otherwise -> error "Unknown addressing mode"

{-# INLINABLE setN #-}
setN :: Emu6502 m => Word8 -> m ()
setN r = putN $ r >= 0x80

{-# INLINABLE setZ #-}
setZ :: Emu6502 m => Word8 -> m ()
setZ r = putZ $ r == 0

{-# INLINABLE setNZ #-}
setNZ :: Emu6502 m => Word8 -> m Word8
setNZ r = setN r >> setZ r >> return r

{-# INLINABLE setNZ_ #-}
setNZ_ :: Emu6502 m => Word8 -> m ()
setNZ_ r = setN r >> setZ r

{-# INLINABLE op_ora #-}
op_ora :: Emu6502 m => Word8 -> m ()
op_ora bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA .|. src
    putA newA
    setNZ_ newA

{-# INLINABLE op_and #-}
op_and :: Emu6502 m => Word8 -> m ()
op_and bbb = do
    src <- getData bbb
    getA >>= setNZ . (src .&.) >>= putA

{-# INLINABLE op_xor #-}
op_xor :: Emu6502 m => Word8 -> m ()
op_xor bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA `xor` src
    putA newA
    setNZ newA
    debugStrLn $ "A = " ++ show newA

{-# INLINABLE op_lda #-}
op_lda :: Emu6502 m => Word8 -> m ()
op_lda bbb = do
    getData bbb >>= setNZ >>= putA

{-# INLINABLE op_sta #-}
op_sta :: Emu6502 m => Word8 -> m ()
op_sta bbb = getA >>= putData bbb

{-# INLINABLE op_adc #-}
op_adc :: Emu6502 m => Word8 -> m ()
op_adc bbb = do
    src <- getData bbb
    oldA <- getA
    carry <- getC
    let newA = fromIntegral oldA+fromIntegral src+if carry then 1 else 0 :: Word16
    decimal <- getD
    setZ (i8 newA)
    if decimal
        then do
            let adjustedA = if (oldA .&. 0xf) + (src .&. 0xf) + (if carry then 1 else 0) > 9
                                then newA+6
                                else newA
            setN (i8 adjustedA)
            putV $ (complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` adjustedA) .&. 0x80) /= 0
            let readjustedA = if adjustedA > 0x99 then adjustedA+96 else adjustedA
            putC $ readjustedA > 0xff
            putA $ fromIntegral (readjustedA .&. 0xff)
        else do
            setN (i8 newA)
            putV $ (complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` newA) .&. 0x80) /= 0
            putC $ newA > 0xff
            putA $ fromIntegral (newA .&. 0xff)

{-# INLINABLE op_sbc #-}
op_sbc :: Emu6502 m => Word8 -> m ()
op_sbc bbb = do
    src <- getData bbb
    oldA <- getA
    carry <- getC
    let newA = fromIntegral oldA-fromIntegral src-if carry then 0 else 1 :: Word16
    setNZ $ i8 newA
    putV $ (((i16 oldA `xor` i16 src) .&. 0x80) /= 0) && (((i16 oldA `xor` newA) .&. 0x80) /= 0)
    decimal <- getD
    if decimal
        then do
            debugStrLn $ "Decimal subtract oldA ="++showHex oldA "" ++ " src=" ++ showHex src ""
            debugStrLn $ "unadjusted = " ++ showHex newA ""
            let adjustedA = if iz (oldA .&. 0xf)-(if carry then 0 else 1) < iz (src .&. 0xf)
                                then newA - 6
                                else newA
            let readjustedA = if adjustedA > 0x99
                                then adjustedA-0x60
                                else adjustedA
            putA $ fromIntegral (readjustedA .&. 0xff)
            putC $ readjustedA < 0x100
        else do
            putA $ fromIntegral (newA .&. 0xff)
            putC $ newA < 0x100
    debugStrLn $ "A = " ++ show newA

{-# INLINABLE op_cmp #-}
op_cmp :: Emu6502 m => Word8 -> m ()
op_cmp bbb = do
    src <- getData bbb
    oldA <- getA
    let new = i16 oldA-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

{-# INLINABLE op_asl #-}
op_asl :: Emu6502 m => Word8 -> m ()
op_asl bbb = withData02 bbb False $ \src -> do
    putC $ src .&. 0x80 > 0
    let new = src `shift` 1
    setNZ new

{-# INLINABLE op_rol #-}
op_rol :: Emu6502 m => Word8 -> m ()
op_rol bbb = withData02 bbb False $ \src -> do
    fc <- getC
    putC $ src .&. 0x80 > 0
    let new = (src `shift` 1) + if fc then 1 else 0
    setNZ new
    return new

{-# INLINABLE op_lsr #-}
op_lsr :: Emu6502 m => Word8 -> m ()
op_lsr bbb = withData02 bbb False $ \src -> do
    putC $ src .&. 0x01 > 0
    let new = src `shift` (-1)
    putN False
    setZ new
    return new

{-# INLINABLE op_ror #-}
op_ror :: Emu6502 m => Word8 -> m ()
op_ror bbb = withData02 bbb False $ \src -> do
    fc <- getC
    putC $ src .&. 0x01 > 0
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    setNZ new

{-# INLINABLE op_stx #-}
op_stx :: Emu6502 m => Word8 -> m ()
op_stx bbb = withData02 bbb True $ \_ -> getX

{-# INLINABLE op_ldx #-}
op_ldx :: Emu6502 m => Word8 -> m ()
op_ldx bbb = getData02 bbb True $ \src -> do
    putX src
    setNZ_ src

{-# INLINABLE op_dec #-}
op_dec :: Emu6502 m => Word8 -> m ()
op_dec bbb = withData02 bbb False $ \src -> setNZ (src-1)

{-# INLINABLE op_inc #-}
op_inc :: Emu6502 m => Word8 -> m ()
op_inc bbb = withData02 bbb False $ \src -> setNZ (src+1)

{-# INLINABLE op_bit #-}
op_bit :: Emu6502 m => Word8 -> m ()
op_bit bbb = getData02 bbb False $ \src -> do
    ra <- getA
    setN src
    putV $ src .&. 0x40 > 0
    setZ $ ra .&. src

{-# INLINABLE op_sty #-}
op_sty :: Emu6502 m => Word8 -> m ()
op_sty bbb = withData02 bbb False $ \_ -> getY

{-# INLINABLE op_ldy #-}
op_ldy :: Emu6502 m => Word8 -> m ()
op_ldy bbb = getData02 bbb False $ \src -> do
    putY src
    setNZ_ src

{-# INLINABLE op_cpx #-}
op_cpx :: Emu6502 m => Word8 -> m ()
op_cpx bbb = getData02 bbb False $ \src -> do
    rx <- getX
    let new = i16 rx-i16 src
    setNZ $ i8 new
    putC $ new < 0x100

{-# INLINABLE op_cpy #-}
op_cpy :: Emu6502 m => Word8 -> m ()
op_cpy bbb = getData02 bbb False $ \src -> do
    ry <- getY
    let new = i16 ry-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

{-# INLINABLE ins_txs #-}
ins_txs :: Emu6502 m => m ()
ins_txs = do
    --incPC
    getX >>= putS
    tick 2

{-# INLINABLE ins_transfer #-}
ins_transfer :: Emu6502 m =>
                     m Word8 -> (Word8 -> m ()) ->
                     m ()
ins_transfer getReg putReg = do
    --incPC
    v0 <- getReg
    putReg v0
    setNZ v0
    tick 2

{-# INLINABLE ins_incr #-}
ins_incr :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
ins_incr getReg putReg = do
    --incPC
    v0 <- getReg
    let v1 = v0+1
    setNZ v1
    putReg v1
    tick 2

{-# INLINABLE ins_decr #-}
ins_decr :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
ins_decr getReg putReg = do
    --incPC
    v0 <- getReg
    let v1 = v0-1
    setNZ v1
    putReg v1
    tick 2

{-# INLINABLE ins_brk #-}
ins_brk :: Emu6502 m => m ()
ins_brk = do
    --incPC
    incPC
    putB True
    nmi True

-- Am I using wrong address for IRQ. Should it be 0xfffe for IRQ, 0xfffa for NMI?
{-# INLINABLE irq #-}
irq :: Emu6502 m => m ()
irq = do
    fi <- getI
    if not fi
        then nmi False
        else return ()

{-# INLINABLE push #-}
push :: Emu6502 m => Word8 -> m ()
push v = do
    sp <- getS
    writeMemory (0x100+i16 sp) v
    putS (sp-1)

{-# INLINABLE pull #-}
pull :: Emu6502 m => m Word8
pull = do
    sp <- getS
    let sp' = sp+1
    putS sp'
    readMemory (0x100+i16 sp')

{-# INLINABLE ins_pha #-}
ins_pha ::Emu6502 m => m ()
ins_pha = do
    --incPC
    getA >>= push
    tick 3

{-# INLINABLE ins_php #-}
ins_php :: Emu6502 m => m ()
ins_php = do
    --incPC
    getP >>= push . (.|. 0x30)
    tick 3

{-# INLINABLE ins_plp #-}
ins_plp :: Emu6502 m => m ()
ins_plp = do
    --incPC
    pull >>= putP
    tick 4

{-# INLINABLE ins_pla #-}
ins_pla :: Emu6502 m => m ()
ins_pla = do
    --incPC
    v0 <- pull
    putA v0
    setNZ v0
    tick 4

{-# INLINABLE lo #-}
lo :: Word16 -> Word8
lo = i8

{-# INLINABLE hi #-}
hi :: Word16 -> Word8
hi a = i8 (a `shift` (-8))

{-# INLINABLE nmi #-}
nmi :: Emu6502 m => Bool -> m ()
nmi sw = do
    p0 <- getPC
    push $ hi p0
    push $ lo p0
    putB sw
    getP >>= push . (.|. 0x20) -- always on bit
    putI True
    read16 0xfffe >>= putPC -- irq/brk XXX
    tick 7

{-# INLINABLE ins_rti #-}
ins_rti :: Emu6502 m => m ()
ins_rti = do
    --incPC
    pull >>= putP
    make16 <$> pull <*> pull >>= putPC
    tick 6

-- BBC stuff XXX
{-# INLINABLE ins_jsr #-}
ins_jsr :: Emu6502 m => m ()
ins_jsr = do
    --incPC
    p0 <- getPC
    getPC >>= read16 >>= putPC
    let p2 = p0+1
    push $ hi p2
    push $ lo p2
    tick 6

{-# INLINABLE ins_rts #-}
ins_rts :: Emu6502 m => m ()
ins_rts = do
    --incPC
    make16 <$> pull <*> pull >>= putPC . (+1)
    tick 6

{-# INLINABLE step #-}
step :: Emu6502 m => m ()
step = do
    debugStrLn "------"
    dumpState
    p0 <- getPC
    --if p0 == 0x3781 then debug .= True else return () -- XXX
    if p0 == 0x400 then liftIO $ putStrLn "Started!!!" else return ()
    if p0 == 0x3770 then liftIO $ putStrLn "Passed!!!" else return ()
    debugStrLn $ "pc = " ++ showHex p0 ""
    i <- readMemory p0
    debugStrLn $ "instruction = " ++ showHex i ""
    incPC
    case i of
        0x00 -> ins_brk
        0x08 -> ins_php
        0x10 -> ins_bra getN False
        0x18 -> ins_set putC False
        0x20 -> ins_jsr
        0x28 -> ins_plp
        0x30 -> ins_bra getN True
        0x38 -> ins_set putC True
        0x40 -> ins_rti
        0x48 -> ins_pha
        0x50 -> ins_bra getV False
        0x58 -> ins_set putI False
        0x60 -> ins_rts
        0x68 -> ins_pla
        0x70 -> ins_bra getV True
        0x78 -> ins_set putI True
        0x88 -> ins_decr getY putY
        0x8a -> ins_transfer getX putA
        0x90 -> ins_bra getC False
        0x98 -> ins_transfer getY putA
        0x9a -> ins_txs
        0xa8 -> ins_transfer getA putY
        0xaa -> ins_transfer getA putX
        0xb0 -> ins_bra getC True
        0xb8 -> ins_set putV False
        0xba -> ins_transfer getS putX
        0xc8 -> ins_incr getY putY
        0xca -> ins_decr getX putX
        0xd0 -> ins_bra getZ False
        0xd8 -> ins_set putD False
        0xe8 -> ins_incr getX putX
        0xea -> ins_nop
        0xf0 -> ins_bra getZ True
        0xf8 -> ins_set putD True

        otherwise -> do
            let cc = i .&. 0b11
            debugStrLn $ "cc = " ++ show cc
            case cc of
                0b00 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of
                        0b001 -> op_bit bbb
                        0b010 -> ins_jmp
                        0b011 -> ins_jmp_indirect
                        0b100 -> op_sty bbb
                        0b101 -> op_ldy bbb
                        0b110 -> op_cpy bbb
                        0b111 -> op_cpx bbb

                        otherwise -> illegal i

                0b01 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> op_ora bbb
                        0b001 -> op_and bbb
                        0b010 -> op_xor bbb
                        0b011 -> op_adc bbb
                        0b100 -> op_sta bbb
                        0b101 -> op_lda bbb
                        0b110 -> op_cmp bbb
                        0b111 -> op_sbc bbb

                        otherwise -> illegal i
                0b10 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> op_asl bbb
                        0b001 -> op_rol bbb
                        0b010 -> op_lsr bbb
                        0b011 -> op_ror bbb
                        0b100 -> op_stx bbb
                        0b101 -> op_ldx bbb
                        0b110 -> op_dec bbb
                        0b111 -> op_inc bbb

                otherwise -> illegal i
    dumpState
    return ()
