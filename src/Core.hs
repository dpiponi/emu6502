{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE Strict #-}

-- http://nesdev.com/6502_cpu.txt

module Core where

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

data Registers = R {
    _pc :: !Word16,
    _p :: !Word8,
    _a :: !Word8,
    _x :: !Word8,
    _y :: !Word8,
    _s :: !Word8
}

makeLenses ''Registers

data State6502 = S {
    _mem :: IOUArray Int Word8,
    _clock :: !Int64,
    _regs :: !Registers,
    _debug :: !Bool
}

makeLenses ''State6502

writeRAM :: IOUArray Int Word8 -> Int -> Word8 -> StateT State6502 IO ()
writeRAM m addr b = liftIO $ writeArray m addr b

readRAM :: IOUArray Int Word8 -> Int -> StateT State6502 IO Word8
readRAM m addr = liftIO $ readArray m addr

flagC :: Lens' Registers Bool
flagC = p . bitAt 0

flagZ :: Lens' Registers Bool
flagZ = p . bitAt 1

flagI :: Lens' Registers Bool
flagI = p . bitAt 2

flagD :: Lens' Registers Bool
flagD = p . bitAt 3

flagB :: Lens' Registers Bool
flagB = p . bitAt 4

flagV :: Lens' Registers Bool
flagV = p . bitAt 6

flagS :: Lens' Registers Bool
flagS = p . bitAt 7

{-# INLINE getA #-}
getA :: StateT State6502 IO Word8
getA = use (regs . a)

{-# INLINE putA #-}
putA :: Word8 -> StateT State6502 IO ()
putA r = regs . a .= r

{-# INLINE getS #-}
getS :: StateT State6502 IO Word8
getS = use (regs . s)

{-# INLINE putS #-}
putS :: Word8 -> StateT State6502 IO ()
putS r = regs . s .= r

{-# INLINE getX #-}
getX :: StateT State6502 IO Word8
getX = use (regs . x)

{-# INLINE putX #-}
putX :: Word8 -> StateT State6502 IO ()
putX r = regs . x .= r

{-# INLINE getY #-}
getY :: StateT State6502 IO Word8
getY = use (regs . y)

{-# INLINE putY #-}
putY :: Word8 -> StateT State6502 IO ()
putY r = regs . y .= r

{-# INLINE putPC #-}
putPC :: Word16 -> StateT State6502 IO ()
putPC r = regs . pc .= r

{-# INLINE getPC #-}
getPC :: StateT State6502 IO Word16
getPC = use (regs . pc)

debugStr str = do
    d <- use debug
    if d
        then liftIO $ putStr str
        else return ()

debugStrLn str = do
    d <- use debug
    if d
        then liftIO $ putStrLn str
        else return ()

dumpRegisters :: StateT State6502 IO ()
dumpRegisters = do
    tClock <- use clock
    debugStr $ "clock = " ++ show tClock
    regPC <- getPC
    debugStr $ " pc = " ++ showHex regPC ""
    regP <- use (regs . p)
    debugStr $ " flags = " ++ showHex regP ""
    debugStr $ "(S=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
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
    regS <- use (regs . s)
    debugStrLn $ " S = " ++ showHex regS ""

{-# INLINE readMemory #-}
readMemory :: Word16 -> StateT State6502 IO Word8
readMemory addr = do
    -- debugStrLn $ "Reading from addr " ++ showHex addr ""
    -- if addr == 0x8000
    --     then do
    --         c <- liftIO $ getChar
    --         return $ BS.c2w c
    --     else do
    --         m <- use mem
    --         liftIO $ readArray m addr

    m <- use mem
    liftIO $ readArray m (fromIntegral addr)

{-# INLINE writeMemory #-}
writeMemory :: Word16 -> Word8 -> StateT State6502 IO ()
writeMemory addr v = do
    m <- use mem
    liftIO $ writeArray m (fromIntegral addr) v
    -- debugStrLn $ "Writing " ++ showHex v "" ++ " to addr " ++ showHex addr ""
    -- if addr == 0x8000
    --     then do
    --         liftIO $ putChar (BS.w2c v)
    --     else do
    --         m <- use mem
    --         liftIO $ writeArray m addr v

dumpMemory :: StateT State6502 IO ()
dumpMemory = do
    regPC <- getPC
    b0 <- readMemory (fromIntegral regPC)
    b1 <- readMemory (fromIntegral regPC+1)
    b2 <- readMemory (fromIntegral regPC+2)
    debugStr $ "(PC) = "
    debugStr $ showHex b0 "" ++ " "
    debugStr $ showHex b1 "" ++ " "
    debugStrLn $ showHex b2 ""

dumpState :: StateT State6502 IO ()
dumpState = do
    dumpMemory
    dumpRegisters

{-# INLINE read16 #-}
read16 :: Word16 -> StateT State6502 IO Word16
read16 addr = do
    lo <- readMemory addr
    hi <- readMemory (addr+1)
    return $ (i16 hi `shift` 8)+i16 lo

{-# INLINE read16zp #-}
read16zp :: Word8 -> StateT State6502 IO Word16
read16zp addr = do
    lo <- readMemory (fromIntegral addr)
    hi <- readMemory (fromIntegral (addr+1))
    return $ (fromIntegral hi `shift` 8)+fromIntegral lo

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

aboutToBrk :: StateT State6502 IO Bool
aboutToBrk = do
    p0 <- getPC
    ins <- readMemory (fromIntegral p0)
    return $ ins == 0x00

putData :: Word8 -> Word8 -> StateT State6502 IO ()
putData bbb src = do
    p0 <- getPC
    case bbb of
        -- (zero page, X)
        0b000 -> do
            offsetX <- getX
            zpAddr <- readMemory (fromIntegral (p0+1))
            let addrAddr = zpAddr+offsetX
            addr <- read16zp addrAddr
            writeMemory addr src
            putPC $ p0+2
            clock += 6
        -- zero page
        0b001 -> do
            addr <- readMemory (fromIntegral (p0+1))
            writeMemory (i16 addr) src
            putPC $ p0+2
            clock += 3
        -- immediate
        0b010 -> do
            error "Can't store immediate"
        -- absolute
        0b011 -> do
            addr <- read16 (p0+1)
            writeMemory addr src
            putPC $ p0+3
            clock += 4
        -- (zero page), Y
        0b100 -> do
            offsetY <- getY
            zpAddr <- readMemory (fromIntegral (p0+1))
            addr <- read16zp zpAddr
            let newAddr = addr+i16 offsetY
            --let carry = (newAddr .&. 0xff00) /= (addr .&. 0xff00)
            writeMemory (addr+i16 offsetY) src
            putPC $ p0+2
            clock += 6
        -- zero page, X
        0b101 -> do
            offsetX <- getX
            zpAddr <- readMemory (p0+1)
            let addrAddr = zpAddr+offsetX
            writeMemory (i16 addrAddr) src
            putPC $ p0+2
            clock += 4
        -- absolute, Y
        0b110 -> do
            offsetY <- getY
            baseAddr <- read16 (p0+1)
            let addr = baseAddr+i16 offsetY
            --let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            writeMemory addr src
            putPC $ p0+3
            clock += 5
        -- absolute, X
        0b111 -> do
            offsetX <- getX
            baseAddr <- read16 (p0+1)
            let addr = baseAddr+i16 offsetX
            let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            writeMemory addr src
            putPC $ p0+3
            clock += 5

getData :: Word8 -> StateT State6502 IO Word8
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
            clock += 6
            return src
        -- zero page
        0b001 -> do
            addr <- readMemory (p0+1)
            src <- readMemory (i16 addr)
            putPC $ p0+2
            clock += 3
            return src
        -- immediate
        0b010 -> do
            src <- readMemory (p0+1)
            putPC $ p0+2
            clock += 2
            return src
        -- absolute
        0b011 -> do
            --addr <- read16 (p0+1)
            addr <- read16 (p0+1)
            debugStrLn $ "Absolute read from " ++ showHex addr ""
            src <- readMemory addr
            putPC $ p0+3
            clock += 4
            return src
        -- (zero page), Y
        0b100 -> do
            offsetY <- getY
            zpAddr <- readMemory (p0+1)
            addr <- read16zp zpAddr
            let newAddr = addr+i16 offsetY
            let carry = (newAddr .&. 0xff00) /= (addr .&. 0xff00)
            src <- readMemory (addr+i16 offsetY)
            regs . pc .= p0+2
            clock += if carry then 6 else 5
            return src
        -- zero page, X
        0b101 -> do
            offsetX <- getX
            zpAddr <- readMemory (p0+1)
            let addrAddr = zpAddr+offsetX
            src <- readMemory (i16 addrAddr)
            regs . pc .= p0+2
            clock += 4
            return src
        -- absolute, Y
        0b110 -> do
            offsetY <- getY
            baseAddr <- read16 (p0+1)
            let addr = baseAddr+i16 offsetY
            let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            src <- readMemory addr
            regs . pc .= p0+3
            clock += if carry then 5 else 4
            return src
        -- absolute, X
        0b111 -> do
            offsetX <- getX
            baseAddr <- read16 (p0+1)
            let addr = baseAddr+i16 offsetX
            let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            src <- readMemory addr
            regs . pc .= p0+3
            clock += if carry then 5 else 4
            return src

{-# INLINE ins_bra #-}
ins_bra :: Lens' Registers Bool -> Bool -> StateT State6502 IO ()
ins_bra flag value = do
    f <- use (regs . flag)
    p0 <- getPC
    let oldP = p0+2
    if value && f || not value && not f
        then do
            debugStrLn "Taking branch"
            offset <- readMemory (p0+1) -- XXX or ^^^
            let newP = if offset < 0x80 then oldP+i16 offset else oldP+i16 offset-0x100
            clock += if newP .&. 0xff00 == oldP .&. 0xff00 then 3 else 4
            regs . pc .= newP
        else do
            debugStrLn "Not taking branch"
            clock += 2
            regs . pc .= oldP

{-# INLINE ins_set #-}
ins_set :: Lens' Registers Bool -> Bool -> StateT State6502 IO ()
ins_set flag value = do
    p0 <- getPC
    regs . flag .= value
    regs . pc .= p0+1
    clock += 2

{-# INLINE ins_nop #-}
ins_nop :: StateT State6502 IO ()
ins_nop = do
    regs . pc += 1
    clock += 2

{-# INLINE ins_jmp #-}
ins_jmp :: StateT State6502 IO ()
ins_jmp = do
    p0 <- getPC
    addr <- read16 (p0+1)
    regs . pc .= addr
    clock += 3

nonwhite ra | ra < 32 = "()"
nonwhite ra = "'" ++ [BS.w2c ra] ++ "'"

{-# INLINE ins_jmp_indirect #-}
ins_jmp_indirect :: StateT State6502 IO ()
ins_jmp_indirect = do
    p0 <- getPC
    addrAddr <- read16 (p0+1)
    debugStrLn $ "Indirect jump (" ++ showHex addrAddr "" ++ ")"
    case addrAddr of
        0x20e -> do -- WRCHV
            ra <- getA
            --liftIO $ putChar (BS.w2c ra)
            liftIO $ putStrLn $ "WRCHV " ++ show ra ++ " " ++ nonwhite ra
            ins_rts
        otherwise -> do    
            addr <- read16 addrAddr
            regs . pc .= addr
            clock += 5

-- Need to separate R/W/RW XXX
withData01 :: Word8 -> Bool -> Bool ->
              (Word8 -> StateT State6502 IO Word8) ->
              StateT State6502 IO ()
withData01 bbb write useY op = case bbb of
    -- immediate
    0b000 -> do
        p0 <- getPC
        src <- readMemory (p0+1)
        regs . pc .= p0+2
        op src
        if write
            then error "Can't write immediate"
            else clock += 2
    -- zero page
    0b001 -> do
        p0 <- getPC
        addr <- readMemory (p0+1)
        src <- readMemory (i16 addr)
        regs . pc .= p0+2
        dst <- op src
        if write
            then do
                writeMemory (i16 addr) dst
                clock += 5
            else
                clock += 3
    -- accumulator -- XXX
    0b010 -> do
        p0 <- getPC
        src <- getA
        putPC $ p0+1
        dst <- op src
        putA dst
        if write
            then clock += 2
            else error "Must write back to A"
    -- absolute
    0b011 -> do
        p0 <- getPC
        addr <- read16 (p0+1)
        src <- readMemory addr
        regs . pc .= p0+3
        dst <- op src
        if write
            then do
                writeMemory addr dst
                clock += 6
            else clock += 4
    -- zero page, X
    0b101 -> do
        p0 <- getPC
        offsetX <- if useY then getY else getX
        zpAddr <- readMemory (p0+1)
        let addr = zpAddr+offsetX
        src <- readMemory (i16 addr)
        regs . pc .= p0+2
        dst <- op src
        if write
            then do
                writeMemory (i16 addr) dst
                clock += 6
            else clock += 4
    -- absolute, X
    0b111 -> do
        p0 <- getPC
        offsetX <- if useY then getY else getX
        baseAddr <- read16 (p0+1)
        let addr = baseAddr+i16 offsetX
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        src <- readMemory addr
        regs . pc .= p0+3
        dst <- op src
        if write
            then do
                writeMemory addr dst
                clock += 7
            else
                clock += if carry then 5 else 4

    otherwise -> error "Unknown addressing mode"

{-# INLINE setS #-}
setS :: Word8 -> StateT State6502 IO ()
setS r = regs . flagS .= (r >= 0x80)

{-# INLINE setZ #-}
setZ :: Word8 -> StateT State6502 IO ()
setZ r = regs . flagZ .= (r == 0)

{-# INLINE ins_ora #-}
ins_ora :: Word8 -> StateT State6502 IO ()
ins_ora bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA .|. src
    putA newA
    setS newA
    setZ newA
    debugStrLn $ "A = " ++ show newA

{-# INLINE ins_and #-}
ins_and :: Word8 -> StateT State6502 IO ()
ins_and bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA .&. src
    putA newA
    setS newA
    setZ newA
    debugStrLn $ "A = " ++ show newA

{-# INLINE ins_xor #-}
ins_xor :: Word8 -> StateT State6502 IO ()
ins_xor bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA `xor` src
    putA newA
    setS newA
    setZ newA
    debugStrLn $ "A = " ++ show newA

{-# INLINE ins_lda #-}
ins_lda :: Word8 -> StateT State6502 IO ()
ins_lda bbb = do
    debugStrLn $ "LDA instruction with address mode " ++ showHex bbb ""
    newA <- getData bbb
    putA newA
    setS newA
    setZ newA
    debugStrLn $ "A = " ++ show newA

{-# INLINE ins_sta #-}
ins_sta :: Word8 -> StateT State6502 IO ()
ins_sta bbb = do
    oldA <- getA
    putData bbb oldA

{-# INLINE ins_adc #-}
ins_adc :: Word8 -> StateT State6502 IO ()
ins_adc bbb = do
    src <- getData bbb
    oldA <- getA
    carry <- use (regs . flagC)
    let newA = fromIntegral oldA+fromIntegral src+if carry then 1 else 0 :: Word16
    decimal <- use (regs . flagD)
    setZ (i8 newA)
    if decimal
        then do
            let adjustedA = if (oldA .&. 0xf) + (src .&. 0xf) + (if carry then 1 else 0) > 9
                                then newA+6
                                else newA
            setS (i8 adjustedA)
            regs . flagV .= ((complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` adjustedA) .&. 0x80) /= 0)
            let readjustedA = if adjustedA > 0x99 then adjustedA+96 else adjustedA
            regs . flagC .= (readjustedA > 0xff)
            putA $ fromIntegral (readjustedA .&. 0xff)
        else do
            setS (i8 newA)
            regs . flagV .= ((complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` newA) .&. 0x80) /= 0)
            regs . flagC .= (newA > 0xff)
            putA $ fromIntegral (newA .&. 0xff)

{-# INLINE ins_sbc #-}
ins_sbc :: Word8 -> StateT State6502 IO ()
ins_sbc bbb = do
    src <- getData bbb
    oldA <- getA
    carry <- use (regs . flagC)
    let newA = fromIntegral oldA-fromIntegral src-if carry then 0 else 1 :: Word16
    setS (i8 newA)
    setZ (i8 newA)
    regs . flagV .= ((((i16 oldA `xor` i16 src) .&. 0x80) /= 0) && (((i16 oldA `xor` newA) .&. 0x80) /= 0))
    decimal <- use (regs . flagD)
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
            regs . flagC .= (readjustedA < 0x100)
        else do
            putA $ fromIntegral (newA .&. 0xff)
            regs . flagC .= (newA < 0x100)
    debugStrLn $ "A = " ++ show newA

ins_cmp :: Word8 -> StateT State6502 IO ()
ins_cmp bbb = do
    src <- getData bbb
    oldA <- getA
    let new = i16 oldA-i16 src
    debugStrLn $ "Comparing " ++ showHex oldA "" ++ " to " ++ showHex src ""
    setS (i8 new)
    regs . flagC .= (new < 0x100)
    setZ (i8 new)

{-# INLINE ins_asl #-}
ins_asl :: Word8 -> StateT State6502 IO ()
ins_asl bbb = withData01 bbb True False $ \src -> do
    regs . flagC .= (src .&. 0x80 > 0)
    let new = src `shift` 1
    setS new
    setZ new
    return new

{-# INLINE ins_rol #-}
ins_rol :: Word8 -> StateT State6502 IO ()
ins_rol bbb = withData01 bbb True False $ \src -> do
    fc <- use (regs . flagC)
    regs . flagC .= (src .&. 0x80 > 0)
    let new = (src `shift` 1) + if fc then 1 else 0
    setS new
    setZ new
    return new

{-# INLINE ins_lsr #-}
ins_lsr :: Word8 -> StateT State6502 IO ()
ins_lsr bbb = withData01 bbb True False $ \src -> do
    regs . flagC .= (src .&. 0x01 > 0)
    let new = src `shift` (-1)
    regs . flagS .= False
    setZ new
    return new

{-# INLINE ins_ror #-}
ins_ror :: Word8 -> StateT State6502 IO ()
ins_ror bbb = withData01 bbb True False $ \src -> do
    fc <- use (regs . flagC)
    regs . flagC .= (src .&. 0x01 > 0)
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    setS new
    setZ new
    return new

{-# INLINE ins_stx #-}
ins_stx :: Word8 -> StateT State6502 IO ()
ins_stx bbb = withData01 bbb True True $ \_ -> getX

{-# INLINE ins_ldx #-}
ins_ldx :: Word8 -> StateT State6502 IO ()
ins_ldx bbb = withData01 bbb False True $ \src -> do
    putX src
    setS src
    setZ src
    return 0 -- Unused, I hope

{-# INLINE ins_dec #-}
ins_dec :: Word8 -> StateT State6502 IO ()
ins_dec bbb = withData01 bbb True False $ \src -> do
    let new = src-1
    setS new
    setZ new
    return new

{-# INLINE ins_inc #-}
ins_inc :: Word8 -> StateT State6502 IO ()
ins_inc bbb = withData01 bbb True False $ \src -> do
    let new = src+1
    setS new
    setZ new
    return new

{-# INLINE ins_bit #-}
ins_bit :: Word8 -> StateT State6502 IO ()
ins_bit bbb = withData01 bbb False False $ \src -> do
    ra <- getA
    setS src
    regs . flagV .= (src .&. 0x40 > 0)
    setZ (ra .&. src)
    return 0 -- unused

{-# INLINE ins_sty #-}
ins_sty :: Word8 -> StateT State6502 IO ()
ins_sty bbb = withData01 bbb True False $ \src -> do
    new <- getY
    return new

{-# INLINE ins_ldy #-}
ins_ldy :: Word8 -> StateT State6502 IO ()
ins_ldy bbb = withData01 bbb False False $ \src -> do
    putY src
    setS src
    setZ src
    return 0 -- Unused, I hope

{-# INLINE ins_cpx #-}
ins_cpx :: Word8 -> StateT State6502 IO ()
ins_cpx bbb = withData01 bbb False False $ \src -> do
    rx <- getX
    let new = i16 rx-i16 src
    setS (i8 new)
    setZ (i8 new)
    regs . flagC .= (new < 0x100)
    return 0 -- unused

{-# INLINE ins_cpy #-}
ins_cpy :: Word8 -> StateT State6502 IO ()
ins_cpy bbb = withData01 bbb False False $ \src -> do
    ry <- getY
    let new = i16 ry-i16 src
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagC .= (new < 0x100)
    regs . flagZ .= (new .&. 0xff == 0)
    return 0 -- unused

{-# INLINE ins_txs #-}
ins_txs :: StateT State6502 IO ()
ins_txs = do
    v0 <- getX
    putS v0
    regs . pc += 1
    clock += 2

{-# INLINE ins_transfer_flag #-}
ins_transfer_flag :: Lens' Registers Word8 -> Lens' Registers Word8 ->
                     StateT State6502 IO ()
ins_transfer_flag vsrc vdst = do
    v0 <- use (regs . vsrc)
    regs . vdst .= v0
    setS v0
    setZ v0
    regs . pc += 1
    clock += 2

{-# INLINE ins_incr #-}
ins_incr :: Lens' Registers Word8 -> StateT State6502 IO ()
ins_incr v = do
    v0 <- use (regs . v)
    let v1 = v0+1
    setS v1
    setZ v1
    regs . v .= v1
    regs . pc += 1
    clock += 2

{-# INLINE ins_decr #-}
ins_decr :: Lens' Registers Word8 -> StateT State6502 IO ()
ins_decr v = do
    v0 <- use (regs . v)
    let v1 = v0-1
    setS v1
    setZ v1
    regs . v .= v1
    regs . pc += 1
    clock += 2

{-# INLINE ins_brk #-}
ins_brk :: StateT State6502 IO ()
ins_brk = do
    regs . pc += 2
    regs . flagB .= True
    nmi True

-- Am I using wrong address for IRQ. Should it be 0xfffe for IRQ, 0xfffa for NMI?
irq :: StateT State6502 IO ()
irq = do
    fi <- use (regs . flagI)
    if not fi
        then nmi False
        else return ()

{-# INLINE push #-}
push :: Word8 -> StateT State6502 IO ()
push v = do
    sp <- use (regs . s)
    writeMemory (0x100+i16 sp) v
    regs . s -= 1

{-# INLINE pull #-}
pull :: StateT State6502 IO Word8
pull = do
    regs . s += 1
    sp <- use (regs . s)
    readMemory (0x100+i16 sp)

{-# INLINE ins_push #-}
ins_push :: Lens' Registers Word8 -> StateT State6502 IO ()
ins_push v = do
    v0 <- use (regs . v)
    push v0
    regs . pc += 1
    clock += 3

{-# INLINE ins_php #-}
ins_php :: StateT State6502 IO ()
ins_php = do
    v0 <- use (regs . p)
    push (v0 .|. 0x30)
    regs . pc += 1
    clock += 3

{-# INLINE ins_pull #-}
ins_pull :: Lens' Registers Word8 -> StateT State6502 IO ()
ins_pull v = do
    v0 <- pull
    regs . v .= v0
    regs . pc += 1
    clock += 4

{-# INLINE ins_pla #-}
ins_pla :: StateT State6502 IO ()
ins_pla = do
    v0 <- pull
    putA v0
    setS v0
    setZ v0
    regs . pc += 1
    clock += 4

nmi :: Bool -> StateT State6502 IO ()
nmi sw = do
    p0 <- getPC
    push (i8 (p0 `shift` (-8)))
    push (i8 p0)
    regs . flagB .= sw
    s0 <- use (regs . p)
    push (s0 .|. 0x20) -- always on bit
    regs . flagI .= True
    addr <- read16 0xfffe -- irq/brk
    regs . pc .= addr
    clock += 7
    let bbc = False
    if bbc && sw
        then do -- BRKV BBC stuff XXX
            writeMemory 0xfd (i8 p0)
            writeMemory 0xfe (i8 (p0 `shift` (-8)))
            regs . pc .= 0xb402
        else return ()

{-# INLINE ins_rti #-}
ins_rti :: StateT State6502 IO ()
ins_rti = do
    s0 <- pull
    regs . p .= s0
    lo <- pull
    hi <- pull
    let newPc = i16 lo+(i16 hi `shift` 8)
    regs . pc .= newPc
    clock += 6

-- BBC stuff XXX
{-# INLINE ins_jsr #-}
ins_jsr :: StateT State6502 IO ()
ins_jsr = do
    p0 <- getPC
    addr <- read16 (p0+1)
    if addr >= 0xc000
        then do
            case addr of
                0xffe3 -> do -- OSASCI
                            ra <- getA
                            --liftIO $ putChar (BS.w2c ra)
                            liftIO $ putStrLn $ "OSASCI " ++ show ra ++ " " ++ nonwhite ra
                            regs . pc += 3

                0xffe7 -> do -- OSNEWL
                    --liftIO $ putStrLn ""
                    liftIO $ putStrLn "OSNEWL"
                    regs . pc += 3
                    putA 0x0d

                0xfff1 -> do -- OSWORD
                    rA <- getA
                    case rA of
                        0x00 -> do -- Read line
                            lo <- getX
                            hi <- getY
                            let blockAddr = i16 lo+(i16 hi `shift` 8)
                            sAddr <- read16 blockAddr
                            line <- liftIO $ getLine
                            let n = length line
                            forM_ [0..n-1] $ \i -> do
                                writeMemory (sAddr+fromIntegral i) (BS.c2w (line!!i))
                            writeMemory (sAddr+i16 n) 13
                            regs . flagC .= False
                            putY $ fromIntegral n+1
                            regs . pc += 3

                        otherwise -> do
                            error "Unknown OSWORD call"
                0xfff4 -> do -- OSBYTE
                    rA <- getA
                    case rA of
                        0x7e -> do -- acknowledge escape
                            putX 0xff
                            regs . pc += 3
                        0x83 -> do -- read OSHWM
                            putX 0x00
                            putY 0x40
                            regs . pc += 3
                        0x84 -> do -- read HIMEM
                            putX 0x00
                            putY 0x70
                            regs . pc += 3
                        0xda -> do -- read length of VDU queue
                            putX 0x00
                            regs . pc += 3
                        otherwise -> do
                            error "Unknown OSBYTE call"
                otherwise -> do
                    liftIO $ putStrLn $ "Call to 0x" ++ showHex addr ""
                    error "It's over"
        else do
            let p2 = p0+2
            push (i8 (p2 `shift` (-8)))
            push (i8 p2)
            putPC addr
            clock += 6

{-# INLINE ins_rts #-}
ins_rts :: StateT State6502 IO ()
ins_rts = do
    lo <- pull
    hi <- pull
    let addr = i16 lo+(i16 hi `shift` 8)+1
    putPC addr
    clock += 6

step :: StateT State6502 IO ()
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
        0x10 -> ins_bra flagS False
        0x18 -> ins_set flagC False
        0x20 -> ins_jsr
        0x28 -> ins_pull p
        0x30 -> ins_bra flagS True
        0x38 -> ins_set flagC True
        0x40 -> ins_rti
        0x48 -> ins_push a
        0x50 -> ins_bra flagV False
        0x58 -> ins_set flagI False
        0x60 -> ins_rts
        0x68 -> ins_pla
        0x70 -> ins_bra flagV True
        0x78 -> ins_set flagI True
        0x88 -> ins_decr y
        0x8a -> ins_transfer_flag x a
        0x90 -> ins_bra flagC False
        0x98 -> ins_transfer_flag y a
        0x9a -> ins_txs
        0xa8 -> ins_transfer_flag a y
        0xaa -> ins_transfer_flag a x
        0xb0 -> ins_bra flagC True
        0xb8 -> ins_set flagV False
        0xba -> ins_transfer_flag s x
        0xc8 -> ins_incr y
        0xca -> ins_decr x
        0xd0 -> ins_bra flagZ False
        0xd8 -> ins_set flagD False
        0xe8 -> ins_incr x
        0xea -> ins_nop
        0xf0 -> ins_bra flagZ True
        0xf8 -> ins_set flagD True

        otherwise -> do
            let cc = i .&. 0b11
            debugStrLn $ "cc = " ++ show cc
            case cc of
                0b00 -> do
                    debugStrLn "cc=0b00 instruction"
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

                        otherwise -> do
                            error "bbb=0 error!"

                0b01 -> do
                    debugStrLn "0b01 instruction"
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

                        otherwise -> error "Unknown aaa"
                0b10 -> do
                    debugStrLn "0b10 instruction"
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

                otherwise -> do
                    debug .= True
                    dumpState
                    error "Unknown instruction class (cc)"
    dumpState
    return ()

