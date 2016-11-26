{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Core
import Control.Concurrent (threadDelay)
import Binary
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import SDL.Vect
import SDL.Video.Renderer
import SDL.Event
import SDL.Input.Keyboard
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
import System.Console.CmdArgs hiding ((+=))

data Args = Args { file :: String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin" }

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

scale :: CInt
scale = 4

isPressed :: InputMotion -> Bool
isPressed Pressed = True
isPressed Released = False

main :: IO ()
main = do
  args <- cmdArgs clargs
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
  --readBinary memory "Yar.bin" 0xf000
  --readBinary memory "Galaxian.bin" 0xf000
  --readBinary memory "skiing.bin" 0xf000
  --readBinary memory "kernel21.bin" 0xf000
  --readBinary memory "kernel22.bin" 0xf000
  --readBinary memory "adventure.rom" 0xf000
  --readBinary memory "combat.bin" 0xf000
  --readBinary memory "joustpong.bin" 0xf000
  --readBinary memory "exp.bin" 0xf000
  readBinary memory (file args) 0xf000

  let stella = Stella {
      _hpos = 0,
      _vpos = 0,
      _tvSurface = helloWorld,
      _vblank = 0,
      _vsync = 0,
      _wsync = 0,
      _colubk = 0,
      _colupf = 0,
      _pf0 = 0,
      _pf1 = 0,
      _pf2 = 0,
      _ctrlpf = 0,
      _colup0 = 0,
      _colup1 = 0,
      _ppos0 = 9999,
      _ppos1 = 9999,
      _grp0 = 0,
      _grp1 = 0,
      _swcha = 0,
      _swchb = 0b00001011,
      _enam0 = 0,
      _enam1 = 0,
      _hmp0 = 0,
      _hmp1 = 0,
      _nusiz0 = 0,
      _nusiz1 = 0,
      _inpt4 = 0,
      _cxm0p = 0, _cxm1p = 0, _cxp0fb = 0, _cxp1fb = 0, _cxm0fb = 0, _cxm1fb = 0, _cxblpf = 0, _cxppmm = 0,
      _enabl = 0,
      _mpos0 = 0, _mpos1 = 0,
      _bpos = 0,
      _resmp0 = 0,
      _resmp1 = 0,
      _resbl = 0,
      _hmm0 = 0,
      _hmm1 = 0,
      _hmbl = 0,
      _inpt5 = 0,
      _intim = 0,
      _subtimer = 0,
      _interval = 0
  }
  let state = S { _mem = memory,  _clock = 0, _regs = R 0xf000 0 0 0 0 0xff,
                    _debug = False,
                    _stella = stella}

  let loop n = do
        liftIO $ lockSurface screenSurface
        forM_ [0..9] $ \_ -> do
            events <- liftIO $ SDL.pollEvents

            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

            forM_ events $ \event ->
                case eventPayload event of
                    KeyboardEvent (KeyboardEventData win motion rep sym) -> do
                        let pressed = isPressed motion
                        --liftIO $ print sym
                        case keysymScancode sym of
                            SDL.ScancodeUp -> usingStella $ swcha . bitAt 4 .= not pressed
                            SDL.ScancodeDown -> usingStella $ swcha . bitAt 5 .= not pressed
                            SDL.ScancodeLeft -> usingStella $ swcha . bitAt 6 .= not pressed
                            SDL.ScancodeRight -> usingStella $ swcha . bitAt 7 .= not pressed
                            SDL.ScancodeC -> usingStella $ swchb . bitAt 1 .= not pressed
                            SDL.ScancodeV -> usingStella $ swchb . bitAt 0 .= not pressed
                            SDL.ScancodeSpace -> usingStella $ do
                                latch <- use (vblank . bitAt 6)
                                liftIO $ putStrLn $ "Latch = " ++ show (latch, pressed)
                                case (latch, pressed) of
                                    (False, _) -> inpt4 . bitAt 7 .= not pressed
                                    (True, False) -> return ()
                                    (True, True) -> inpt4 . bitAt 7 .= False
                    otherwise -> return ()

        unM $ times 10000 step

        liftIO $ unlockSurface screenSurface

        liftIO $ SDL.surfaceBlitScaled helloWorld Nothing screenSurface (Just (Rectangle (P (V2 0 0)) (V2 (screenWidth*scale) (screenHeight*scale))))
        liftIO $ SDL.updateWindowSurface window
        unless False $ loop (n+1) -- XXX

  flip runStateT state $ loop 0

  SDL.destroyWindow window
  SDL.freeSurface helloWorld
  SDL.quit
