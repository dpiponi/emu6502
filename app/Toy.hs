{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Core
import Control.Concurrent (threadDelay)
import Binary
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import SDL.Vect
import SDL.Video.Renderer
import Data.Word
import System.Random
import Control.Monad
import Control.Monad.State
import qualified SDL
import Control.Lens
import TIAColors
import Data.Array.Unboxed
import Numeric
import Data.Array.IO
import Data.Bits
import Data.Bits.Lens
import Stella

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (160, 192)

stellaWsync :: (MonadIO m, MonadState Stella m) => Word8 -> m ()
stellaWsync _ = do
    hpos' <- use hpos
    stellaIdle (228-fromIntegral hpos')

stellaVsync :: (MonadIO m, MonadState Stella m) => Word8 -> m ()
stellaVsync v = do
    return ()

stellaVblank :: (MonadIO m, MonadState Stella m) => Word8 -> m ()
stellaVblank v = do
    vold <- use vblank
    liftIO $ putStrLn $ show vold ++ " -> " ++ show v
    if (vold .&. 0b00000010 /= 0) && (v .&. 0b00000010 == 0)
        then do
            liftIO $ print "VBLANK"
            hpos .= 0
            vpos .= 0
        else return ()
    vblank .= v

-- player0
{-# INLINABLE player0 #-}
player0 :: (MonadIO m, MonadState Stella m) => Int -> m Bool
player0 i = do
    hpos' <- use hpos
    pos0' <- use pos0
    let o = hpos'-pos0' :: CInt
    if o >= 0 && o < 8
        then do
            grp0' <- use grp0
            --when (o == 7) $ pos0 .= 9999
            return $ (grp0' `shift` (fromIntegral o-7)) .&. 1 /= 0
        else return False

{-# INLINABLE player1 #-}
player1 :: (MonadIO m, MonadState Stella m) => Int -> m Bool
player1 i = do
    hpos' <- use hpos
    pos1' <- use pos1
    let o = hpos'-pos1' :: CInt
    if o >= 0 && o < 8
        then do
            grp1' <- use grp1
            --when (o == 7) $ pos1 .= 9999
            return $ (grp1' `shift` (fromIntegral o-7)) .&. 1 /= 0
        else return False

picy :: CInt
picy = 40
picx :: CInt
picx = 68

{-# INLINE stellaIdle #-}
stellaIdle :: (MonadIO m, MonadState Stella m) => Int -> m ()
stellaIdle 0 = return ()
stellaIdle n = do
    hpos' <- use hpos
    vpos' <- use vpos
    when (vpos' >= picy && vpos' < picy+192 && hpos' >= picx) $ do
        surface <- use tvSurface
        ptr <- liftIO $ surfacePixels surface
        let ptr' = castPtr ptr :: Ptr Word32
        let x = hpos'-picx
        let y = vpos'-picy
        let i = screenWidth*y+x
        -- The final colour composite!
        c <- use colubk
        pb <- playfield (fromIntegral $ x `shift` (-2))
        pc <- use colupf -- XXX
        let c' = if pb then pc else c
        p0 <- player0 (fromIntegral hpos')
        pc0 <- use colup0 -- XXX
        let c'' = if p0 then pc0 else c'
        p1 <- player1 (fromIntegral hpos')
        pc1 <- use colup1 -- XXX
        let c''' = if p1 then pc1 else c''
        liftIO $ pokeElemOff ptr' (fromIntegral i) (lut!(c''' `shift` (-1)))
    hpos += 1
    hpos' <- use hpos
    when (hpos' >= picx+160) $ do
        hpos .= 0
        vpos += 1
        vpos' <- use vpos
        when (vpos' >= picy+192) $ vpos .= 0
    stellaIdle (n-1)

renderFrame :: (MonadIO m, MonadState Stella m) => m ()
renderFrame = do
    surface <- use tvSurface
    ptr <- liftIO $ surfacePixels surface
    let ptr' = castPtr ptr :: Ptr Word32
    liftIO $ lockSurface surface
    forM_ [0..screenHeight-1] $ \row -> do
        forM_ [0..screenWidth-1] $ \col -> do
            let i = screenWidth*row+col
            liftIO $ pokeElemOff ptr' (fromIntegral i) (fromIntegral col)
            return ()
    liftIO $ unlockSurface surface

data Registers = R {
    _pc :: !Word16,
    _p :: !Word8,
    _a :: !Word8,
    _x :: !Word8,
    _y :: !Word8,
    _s :: !Word8
}

makeLenses ''Registers

{-# INLINE flagC #-}
flagC :: Lens' Registers Bool
flagC = p . bitAt 0

{-# INLINE flagZ #-}
flagZ :: Lens' Registers Bool
flagZ = p . bitAt 1

{-# INLINE flagI #-}
flagI :: Lens' Registers Bool
flagI = p . bitAt 2

{-# INLINE flagD #-}
flagD :: Lens' Registers Bool
flagD = p . bitAt 3

{-# INLINE flagB #-}
flagB :: Lens' Registers Bool
flagB = p . bitAt 4

{-# INLINE flagV #-}
flagV :: Lens' Registers Bool
flagV = p . bitAt 6

{-# INLINE flagN #-}
flagN :: Lens' Registers Bool
flagN = p . bitAt 7

data StateAtari = S {
    _mem :: IOUArray Int Word8,
    _clock :: !Int,
    _regs :: !Registers,
    _debug :: !Bool,
    _stella :: Stella
}

makeLenses ''StateAtari

newtype MonadAtari a = M { unM :: StateT StateAtari IO a }
    deriving (Functor, Applicative, Monad, MonadState StateAtari, MonadIO)

{-# SPECIALIZE player0 :: Int -> StateT Stella IO Bool #-}
{-# SPECIALIZE player1 :: Int -> StateT Stella IO Bool #-}

vsync_addr :: Word16
vsync_addr = 0x00
vblank_addr :: Word16
vblank_addr = 0x01
wsync_addr :: Word16
wsync_addr = 0x02
colubk_addr :: Word16
colubk_addr = 0x09
pf0_addr :: Word16
pf0_addr = 0x0d
pf1_addr :: Word16
pf1_addr = 0x0e
pf2_addr :: Word16
pf2_addr = 0x0f

usingStella m = do
    stella' <- use stella
    stella'' <- flip execStateT stella' m
    stella .= stella''

instance Emu6502 MonadAtari where
    {-# INLINE readMemory #-}
    readMemory addr =
        if addr >= 0x00 && addr < 0x80
            then do
                --liftIO $ putStrLn $ "reading TIA 0x" ++ showHex addr ""
                return 0
            else if addr >= 0x80 && addr < 0x100 || addr >= 0x180 && addr < 0x200
                    then do
                        m <- use mem
                        liftIO $ readArray m (fromIntegral addr .&. 0xff)
                    else if addr >= 0x280 && addr < 0x298
                        then do
                            liftIO $ putStrLn $ "reading PIA 0x" ++ showHex addr ""
                            return 0
                        else if addr >= 0xf000
                            then do
                                m <- use mem
                                liftIO $ readArray m (fromIntegral addr)
                            else do
                                error $ "Mystery read from " ++ showHex addr ""


    {-# INLINE writeMemory #-}
    writeMemory addr v =
        if addr >= 0x00 && addr < 0x80
            then do --liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""
                case addr of
                   0x00 -> usingStella $ stellaVsync v
                   0x01 -> usingStella $ stellaVblank v
                   0x02 -> do
                    --liftIO $ print $ "Doing vblank " ++ showHex v ""
                    usingStella $ stellaWsync v
                   0x06 -> usingStella $ colup0 .= v
                   0x07 -> usingStella $ colup1 .= v
                   0x08 -> usingStella $ colupf .= v
                   0x09 -> usingStella $ colubk .= v
                   0x0a -> usingStella $ ctrlpf .= v
                   0x0d -> usingStella $ pf0 .= v
                   0x0e -> usingStella $ pf1 .= v
                   0x0f -> usingStella $ pf2 .= v
                   0x10 -> usingStella $ do
                    hpos' <- use hpos
                    pos0 .= hpos'
                   0x11 -> usingStella $ do
                    liftIO $ putStrLn "Writing 0x11!!!!!!!!!!!!!!"
                    hpos' <- use hpos
                    pos1 .= hpos'
                   0x1b -> usingStella $ grp0 .= v
                   0x1c -> usingStella $ grp1 .= v
                   otherwise -> do
                    --liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""
                    return ()
            else if addr >= 0x80 && addr < 0x100 || addr >= 0x180 && addr < 0x200
                    then do
                        m <- use mem
                        liftIO $ writeArray m (fromIntegral addr .&. 0xff) v
                    else if addr >= 0x280 && addr < 0x298
                            then liftIO $ putStrLn $ "writing PIA 0x" ++ showHex addr ""
                            else if addr >= 0xf000
                                then do
                                    m <- use mem
                                    liftIO $ writeArray m (fromIntegral addr) v
                                else do
                                    return ()
                                    --liftIO $ print $ "Mystery write to " ++ showHex addr ""

    {-# INLINE getPC #-}
    getPC = use (regs . pc)
    {-# INLINE tick #-}
    tick n = do
        clock += n
        usingStella $ stellaIdle (3*n)
    {-# INLINE putC #-}
    putC b = regs . flagC .= b
    {-# INLINE getC #-}
    getC = use (regs . flagC)
    {-# INLINE putZ #-}
    putZ b = regs . flagZ .= b
    {-# INLINE getZ #-}
    getZ = use (regs . flagZ)
    {-# INLINE putI #-}
    putI b = regs . flagI .= b
    {-# INLINE getI #-}
    getI = use (regs . flagI)
    {-# INLINE putD #-}
    putD b = regs . flagD .= b
    {-# INLINE getD #-}
    getD = use (regs . flagD)
    {-# INLINE putB #-}
    putB b = regs . flagB .= b
    {-# INLINE getB #-}
    getB = use (regs . flagB)
    {-# INLINE putV #-}
    putV b = regs . flagV .= b
    {-# INLINE getV #-}
    getV = use (regs . flagV)
    {-# INLINE putN #-}
    putN b = regs . flagN .= b
    {-# INLINE getN #-}
    getN = use (regs . flagN)
    {-# INLINE getA #-}
    getA = use (regs . a)
    {-# INLINE putA #-}
    putA r = regs . a .= r
    {-# INLINE getS #-}
    getS = use (regs . s)
    {-# INLINE putS #-}
    putS r = regs . s .= r
    {-# INLINE getX #-}
    getX = use (regs . x)
    {-# INLINE putX #-}
    putX r = regs . x .= r
    {-# INLINE getP #-}
    getP = use (regs . p)
    {-# INLINE putP #-}
    putP r = regs . p .= r
    {-# INLINE getY #-}
    getY = use (regs . y)
    {-# INLINE putY #-}
    putY r = regs . y .= r
    {-# INLINE putPC #-}
    putPC r = regs . pc .= r
    {-# INLINE addPC #-}
    addPC n = regs . pc += fromIntegral n

    {-# INLINE debugStr #-}
    debugStr str = do
        d <- use debug
        if d
            then liftIO $ putStr str
            else return ()

    {-# INLINE debugStrLn #-}
    debugStrLn str = do
        d <- use debug
        if d
            then liftIO $ putStrLn str
            else return ()

    {-# INLINE illegal #-}
    illegal i = error $ "Illegal opcode 0x" ++ showHex i ""

scale :: CInt
scale = 4

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 (scale*screenWidth) (scale*screenHeight) }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window
  --renderer <- getRenderer window

  -- helloWorld <- SDL.loadBMP "cat.bmp"
  -- 160x192
  helloWorld <- createRGBSurface (V2 screenWidth screenHeight) RGB888

  memory <- newArray (0, 0xffff) 0 :: IO (IOUArray Int Word8)
--  readBinary memory "kernel_01.bin" 0xf000
  --readBinary memory "kernel_13.bin" 0xf000
  --readBinary memory "Breakout.bin" 0xf000
  readBinary memory "Yar.bin" 0xf000
  --readBinary memory "Galaxian.bin" 0xf000
  --readBinary memory "skiing.bin" 0xf000
  --readBinary memory "kernel21.bin" 0xf000
  --readBinary memory "kernel22.bin" 0xf000
  --readBinary memory "adventure.rom" 0xf000

  let stella = Stella 0 0 0 helloWorld 0 0 0 0 0 0 0 0 0 0 0 9999 9999 0 0
  let state = S { _mem = memory,  _clock = 0, _regs = R 0xf000 0 0 0 0 0xff,
                    _debug = False,
                    _stella = stella}

  let loop n = do
        events <- liftIO $ SDL.pollEvents

        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        liftIO $ lockSurface screenSurface
        unM $ times 10000 step
        liftIO $ unlockSurface screenSurface
{-
        liftIO $ lockSurface screenSurface
        vblank .= 0
        vsync .= 2
        forM_ [0..2] $ \_ -> stellaWsync
        vsync .= 0
        forM_ [0..36] $ \_ -> stellaWsync
        forM_ [0..191] $ \i -> do
            colubk .= fromIntegral i
            stellaIdle 150
            colubk .= fromIntegral (191-i)
            stellaWsync
        vblank .= 0b01000010
        forM_ [0..29] $ \i -> do
            stellaWsync
        liftIO $ unlockSurface screenSurface
        -}

        liftIO $ SDL.surfaceBlitScaled helloWorld Nothing screenSurface (Just (Rectangle (P (V2 0 0)) (V2 (screenWidth*scale) (screenHeight*scale))))
        liftIO $ SDL.updateWindowSurface window
        unless quit $ loop (n+1)

  flip runStateT state $ loop 0

  SDL.destroyWindow window
  SDL.freeSurface helloWorld
  SDL.quit
