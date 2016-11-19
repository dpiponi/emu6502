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
    b0 <- readMemory (fromIntegral regPC)
    b1 <- readMemory (fromIntegral regPC+1)
    b2 <- readMemory (fromIntegral regPC+2)
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

{-# INLINABLE read16 #-}
read16 :: Emu6502 m => Word16 -> m Word16
read16 addr = do
    lo <- readMemory addr
    hi <- readMemory (addr+1)
    return $ make16 lo hi

{-# INLINABLE read16zp #-}
read16zp :: Emu6502 m => Word8 -> m Word16
read16zp addr = do
    lo <- readMemory (fromIntegral addr)
    hi <- readMemory (fromIntegral (addr+1))
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

{-
aboutToBrk :: Monad6502 Bool
aboutToBrk = do
    p0 <- getPC
    ins <- readMemory (fromIntegral p0)
    return $ ins == 0x00
-}

{-# INLINABLE putData #-}
putData :: Emu6502 m => Word8 -> Word8 -> m ()
putData bbb src = do
    p0 <- getPC
    case bbb of
        -- (zero page, X)
        0b000 -> do
            offsetX <- getX
            zpAddr <- readMemory (fromIntegral (p0+1))
            addr <- read16zp (zpAddr+offsetX)
            writeMemory addr src
            putPC $ p0+2
            tick 6
        -- zero page
        0b001 -> do
            addr <- readMemory (fromIntegral (p0+1))
            writeMemory (i16 addr) src
            putPC $ p0+2
            tick 3
        -- immediate
        0b010 -> readMemory p0 >>= illegal -- XXX check in caller
        -- absolute
        0b011 -> do
            addr <- read16 (p0+1)
            writeMemory addr src
            putPC $ p0+3
            tick 4
        -- (zero page), Y
        0b100 -> do
            offsetY <- getY
            addr <- readMemory (fromIntegral (p0+1)) >>= read16zp
            --addr <- read16zp zpAddr
            writeMemory (addr+i16 offsetY) src
            putPC $ p0+2
            tick 6
        -- zero page, X
        0b101 -> do
            offsetX <- getX
            zpAddr <- readMemory (p0+1)
            writeMemory (i16 $ zpAddr+offsetX) src
            putPC $ p0+2
            tick 4
        -- absolute, Y
        0b110 -> do
            offsetY <- getY
            baseAddr <- read16 (p0+1)
            writeMemory (baseAddr+i16 offsetY) src
            putPC $ p0+3
            tick 5
        -- absolute, X
        0b111 -> do
            offsetX <- getX
            baseAddr <- read16 (p0+1)
            writeMemory (baseAddr+i16 offsetX) src
            putPC $ p0+3
            tick 5

{-# INLINABLE getData #-}
getData :: Emu6502 m => Word8 -> m Word8
getData bbb = do
    p0 <- getPC
    case bbb of
        -- (zero page, X)
        0b000 -> do
            offsetX <- getX
            zpAddr <- readMemory (p0+1)
            let addrAddr = zpAddr+offsetX
            addr <- read16zp addrAddr
            src <- readMemory addr
            putPC $ p0+2
            tick 6
            return src
        -- zero page
        0b001 -> do
            addr <- readMemory (p0+1)
            src <- readMemory (i16 addr)
            putPC $ p0+2
            tick 3
            return src
        -- immediate
        0b010 -> do
            src <- readMemory (p0+1)
            putPC $ p0+2
            tick 2
            return src
        -- absolute
        0b011 -> do
            src <- read16 (p0+1) >>= readMemory
            putPC $ p0+3
            tick 4
            return src
        -- (zero page), Y
        0b100 -> do
            offsetY <- getY
            addr <- readMemory (p0+1) >>= read16zp
            let newAddr = addr+i16 offsetY
            let carry = (newAddr .&. 0xff00) /= (addr .&. 0xff00)
            src <- readMemory (addr+i16 offsetY)
            putPC (p0+2)
            tick $ if carry then 6 else 5
            return src
        -- zero page, X
        0b101 -> do
            offsetX <- getX
            zpAddr <- readMemory (p0+1)
            src <- readMemory (i16 $ zpAddr+offsetX)
            putPC (p0+2)
            tick 4
            return src
        -- absolute, Y
        0b110 -> do
            offsetY <- getY
            baseAddr <- read16 (p0+1)
            let addr = baseAddr+i16 offsetY
            let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            src <- readMemory addr
            putPC $ p0+3
            tick $ if carry then 5 else 4
            return src
        -- absolute, X
        0b111 -> do
            offsetX <- getX
            baseAddr <- read16 (p0+1)
            let addr = baseAddr+i16 offsetX
            let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            src <- readMemory addr
            putPC $ p0+3
            tick $ if carry then 5 else 4
            return src

{-# INLINABLE ins_bra #-}
ins_bra :: Emu6502 m => m Bool -> Bool -> m ()
ins_bra getFlag value = do
    f <- getFlag
    p0 <- getPC
    let oldP = p0+2
    if value && f || not value && not f
        then do
            debugStrLn "Taking branch"
            offset <- readMemory (p0+1) -- XXX or ^^^
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
    putFlag value
    getPC >>= putPC . (+1)
    tick 2

{-# INLINABLE ins_nop #-}
ins_nop :: Emu6502 m => m ()
ins_nop = addPC 1 >> tick 2

{-# INLINABLE ins_jmp #-}
ins_jmp :: Emu6502 m => m ()
ins_jmp = do
    addr <- getPC >>= read16 . (+1)
    putPC addr
    tick 3

{-# INLINE nonwhite #-}
nonwhite :: Word8 -> String
nonwhite ra | ra < 32 = "()"
nonwhite ra = "'" ++ [BS.w2c ra] ++ "'"

-- XX BBC stuff
{-# INLINABLE ins_jmp_indirect #-}
ins_jmp_indirect :: Emu6502 m => m ()
ins_jmp_indirect = do
    getPC >>= read16 . (+1) >>= read16 >>= putPC
    tick 5

-- Need to separate R/W/RW XXX
{-# INLINABLE withData01 #-}
withData01 :: Emu6502 m =>
              Word8 -> Bool -> Bool ->
              (Word8 -> m Word8) ->
              m ()
withData01 bbb write useY op = case bbb of
    -- immediate
    0b000 -> if write
                then do
                    getPC >>= readMemory >>= illegal -- XXX reread mem. Should check in caller.
                else do
                    p0 <- getPC
                    src <- readMemory (p0+1)
                    putPC $ p0+2
                    op src
                    tick 2
    -- zero page
    0b001 -> do
        p0 <- getPC
        addr <- readMemory (p0+1)
        if write
            then do
                readMemory (i16 addr) >>= op >>= writeMemory (i16 addr)
                tick 5
            else do
                readMemory (i16 addr) >>= op
                tick 3
        putPC $ p0+2
    -- accumulator -- XXX
    0b010 -> do
        p0 <- getPC
        getA >>= op >>= putA
        putPC $ p0+1
        if write
            then tick 2
            else error "Must write back to A"
    -- absolute
    0b011 -> do
        p0 <- getPC
        addr <- read16 (p0+1)
        src <- readMemory addr
        putPC $ p0+3
        dst <- op src
        if write
            then do
                writeMemory addr dst
                tick 6
            else tick 4
    -- zero page, X
    0b101 -> do
        p0 <- getPC
        offsetX <- if useY then getY else getX
        zpAddr <- readMemory (p0+1)
        let addr = zpAddr+offsetX
        src <- readMemory (i16 addr)
        putPC $ p0+2
        dst <- op src
        if write
            then do
                writeMemory (i16 addr) dst
                tick 6
            else tick 4
    -- absolute, X
    0b111 -> do
        p0 <- getPC
        offsetX <- if useY then getY else getX
        baseAddr <- read16 (p0+1)
        let addr = baseAddr+i16 offsetX
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        src <- readMemory addr
        putPC $ p0+3
        dst <- op src
        if write
            then do
                writeMemory addr dst
                tick 7
            else
                tick $ if carry then 5 else 4

    otherwise -> error "Unknown addressing mode"

{-# INLINABLE setN #-}
setN :: Emu6502 m => Word8 -> m ()
setN r = putN $ r >= 0x80

{-# INLINABLE setZ #-}
setZ :: Emu6502 m => Word8 -> m ()
setZ r = putZ $ r == 0

{-# INLINABLE ins_ora #-}
ins_ora :: Emu6502 m => Word8 -> m ()
ins_ora bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA .|. src
    putA newA
    setN newA
    setZ newA
    debugStrLn $ "A = " ++ show newA

{-# INLINABLE ins_and #-}
ins_and :: Emu6502 m => Word8 -> m ()
ins_and bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA .&. src
    putA newA
    setN newA
    setZ newA
    debugStrLn $ "A = " ++ show newA

{-# INLINABLE ins_xor #-}
ins_xor :: Emu6502 m => Word8 -> m ()
ins_xor bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA `xor` src
    putA newA
    setN newA
    setZ newA
    debugStrLn $ "A = " ++ show newA

{-# INLINABLE ins_lda #-}
ins_lda :: Emu6502 m => Word8 -> m ()
ins_lda bbb = do
    debugStrLn $ "LDA instruction with address mode " ++ showHex bbb ""
    newA <- getData bbb
    putA newA
    setN newA
    setZ newA
    debugStrLn $ "A = " ++ show newA

{-# INLINABLE ins_sta #-}
ins_sta :: Emu6502 m => Word8 -> m ()
ins_sta bbb = getA >>= putData bbb

{-# INLINABLE ins_adc #-}
ins_adc :: Emu6502 m => Word8 -> m ()
ins_adc bbb = do
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

{-# INLINABLE ins_sbc #-}
ins_sbc :: Emu6502 m => Word8 -> m ()
ins_sbc bbb = do
    src <- getData bbb
    oldA <- getA
    carry <- getC
    let newA = fromIntegral oldA-fromIntegral src-if carry then 0 else 1 :: Word16
    setN (i8 newA)
    setZ (i8 newA)
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

{-# INLINABLE ins_cmp #-}
ins_cmp :: Emu6502 m => Word8 -> m ()
ins_cmp bbb = do
    src <- getData bbb
    oldA <- getA
    let new = i16 oldA-i16 src
    setN (i8 new)
    putC $ new < 0x100
    setZ (i8 new)

{-# INLINABLE ins_asl #-}
ins_asl :: Emu6502 m => Word8 -> m ()
ins_asl bbb = withData01 bbb True False $ \src -> do
    putC $ src .&. 0x80 > 0
    let new = src `shift` 1
    setN new
    setZ new
    return new

{-# INLINABLE ins_rol #-}
ins_rol :: Emu6502 m => Word8 -> m ()
ins_rol bbb = withData01 bbb True False $ \src -> do
    fc <- getC
    putC $ src .&. 0x80 > 0
    let new = (src `shift` 1) + if fc then 1 else 0
    setN new
    setZ new
    return new

{-# INLINABLE ins_lsr #-}
ins_lsr :: Emu6502 m => Word8 -> m ()
ins_lsr bbb = withData01 bbb True False $ \src -> do
    putC $ src .&. 0x01 > 0
    let new = src `shift` (-1)
    putN False
    setZ new
    return new

{-# INLINABLE ins_ror #-}
ins_ror :: Emu6502 m => Word8 -> m ()
ins_ror bbb = withData01 bbb True False $ \src -> do
    fc <- getC
    putC $ src .&. 0x01 > 0
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    setN new
    setZ new
    return new

{-# INLINABLE ins_stx #-}
ins_stx :: Emu6502 m => Word8 -> m ()
ins_stx bbb = withData01 bbb True True $ \_ -> getX

{-# INLINABLE ins_ldx #-}
ins_ldx :: Emu6502 m => Word8 -> m ()
ins_ldx bbb = withData01 bbb False True $ \src -> do
    putX src
    setN src
    setZ src
    return 0 -- Unused, I hope

{-# INLINABLE ins_dec #-}
ins_dec :: Emu6502 m => Word8 -> m ()
ins_dec bbb = withData01 bbb True False $ \src -> do
    let new = src-1
    setN new
    setZ new
    return new

{-# INLINABLE ins_inc #-}
ins_inc :: Emu6502 m => Word8 -> m ()
ins_inc bbb = withData01 bbb True False $ \src -> do
    let new = src+1
    setN new
    setZ new
    return new

{-# INLINABLE ins_bit #-}
ins_bit :: Emu6502 m => Word8 -> m ()
ins_bit bbb = withData01 bbb False False $ \src -> do
    ra <- getA
    setN src
    putV $ src .&. 0x40 > 0
    setZ $ ra .&. src
    return 0 -- unused

{-# INLINABLE ins_sty #-}
ins_sty :: Emu6502 m => Word8 -> m ()
ins_sty bbb = withData01 bbb True False $ \_ -> getY

{-# INLINABLE ins_ldy #-}
ins_ldy :: Emu6502 m => Word8 -> m ()
ins_ldy bbb = withData01 bbb False False $ \src -> do
    putY src
    setN src
    setZ src
    return 0 -- Unused, I hope

{-# INLINABLE ins_cpx #-}
ins_cpx :: Emu6502 m => Word8 -> m ()
ins_cpx bbb = withData01 bbb False False $ \src -> do
    rx <- getX
    let new = i16 rx-i16 src
    setN (i8 new)
    setZ (i8 new)
    putC $ new < 0x100
    return 0 -- unused

{-# INLINABLE ins_cpy #-}
ins_cpy :: Emu6502 m => Word8 -> m ()
ins_cpy bbb = withData01 bbb False False $ \src -> do
    ry <- getY
    let new = i16 ry-i16 src
    putC $ new < 0x100
    setN (i8 new)
    setZ (i8 new)
    return 0 -- unused

{-# INLINABLE ins_txs #-}
ins_txs :: Emu6502 m => m ()
ins_txs = do
    getX >>= putS
    addPC 1
    tick 2

{-# INLINABLE ins_transfer #-}
ins_transfer :: Emu6502 m =>
                     m Word8 -> (Word8 -> m ()) ->
                     m ()
ins_transfer getReg putReg = do
    v0 <- getReg
    putReg v0
    setN v0
    setZ v0
    addPC 1
    tick 2

{-# INLINABLE ins_incr #-}
ins_incr :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
ins_incr getReg putReg = do
    v0 <- getReg
    let v1 = v0+1
    setN v1
    setZ v1
    putReg v1
    addPC 1
    tick 2

{-# INLINABLE ins_decr #-}
ins_decr :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
ins_decr getReg putReg = do
    v0 <- getReg
    let v1 = v0-1
    setN v1
    setZ v1
    putReg v1
    addPC 1
    tick 2

{-# INLINABLE ins_brk #-}
ins_brk :: Emu6502 m => m ()
ins_brk = do
    addPC 2
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
    getA >>= push
    addPC 1
    tick 3

{-# INLINABLE ins_php #-}
ins_php :: Emu6502 m => m ()
ins_php = do
    getP >>= push . (.|. 0x30)
    addPC 1
    tick 3

{-# INLINABLE ins_plp #-}
ins_plp :: Emu6502 m => m ()
ins_plp = do
    pull >>= putP
    addPC 1
    tick 4

{-# INLINABLE ins_pla #-}
ins_pla :: Emu6502 m => m ()
ins_pla = do
    v0 <- pull
    putA v0
    setN v0
    setZ v0
    addPC 1
    tick 4

{-# INLINABLE nmi #-}
nmi :: Emu6502 m => Bool -> m ()
nmi sw = do
    p0 <- getPC
    push (i8 (p0 `shift` (-8)))
    push (i8 p0)
    putB sw
    getP >>= push . (.|. 0x20) -- always on bit
    putI True
    read16 0xfffe >>= putPC -- irq/brk
    tick 7

{-# INLINABLE ins_rti #-}
ins_rti :: Emu6502 m => m ()
ins_rti = do
    pull >>= putP
    make16 <$> pull <*> pull >>= putPC
    tick 6

-- BBC stuff XXX
{-# INLINABLE ins_jsr #-}
ins_jsr :: Emu6502 m => m ()
ins_jsr = do
    p0 <- getPC
    getPC >>= read16 . (+1) >>= putPC
    let p2 = p0+2
    push (i8 (p2 `shift` (-8)))
    push (i8 p2)
    tick 6

{-# INLINABLE ins_rts #-}
ins_rts :: Emu6502 m => m ()
ins_rts = do
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
                        0b001 -> ins_bit bbb
                        0b010 -> ins_jmp
                        0b011 -> ins_jmp_indirect
                        0b100 -> ins_sty bbb
                        0b101 -> ins_ldy bbb
                        0b110 -> ins_cpy bbb
                        0b111 -> ins_cpx bbb

                        otherwise -> illegal i

                0b01 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> ins_ora bbb
                        0b001 -> ins_and bbb
                        0b010 -> ins_xor bbb
                        0b011 -> ins_adc bbb
                        0b100 -> ins_sta bbb
                        0b101 -> ins_lda bbb
                        0b110 -> ins_cmp bbb
                        0b111 -> ins_sbc bbb

                        otherwise -> illegal i
                0b10 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> ins_asl bbb
                        0b001 -> ins_rol bbb
                        0b010 -> ins_lsr bbb
                        0b011 -> ins_ror bbb
                        0b100 -> ins_stx bbb
                        0b101 -> ins_ldx bbb
                        0b110 -> ins_dec bbb
                        0b111 -> ins_inc bbb

                otherwise -> illegal i
    dumpState
    return ()

