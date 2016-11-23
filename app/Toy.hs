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

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

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
  --readBinary memory "Yar.bin" 0xf000
  --readBinary memory "Galaxian.bin" 0xf000
  --readBinary memory "skiing.bin" 0xf000
  --readBinary memory "kernel21.bin" 0xf000
  --readBinary memory "kernel22.bin" 0xf000
  readBinary memory "adventure.rom" 0xf000

  let stella = Stella 0 0 0 helloWorld 0 0 0 0 0 0 0 0 0 0 0 9999 9999 0 0 0b00001011
  let state = S { _mem = memory,  _clock = 0, _regs = R 0xf000 0 0 0 0 0xff,
                    _debug = False,
                    _stella = stella}

  let loop n = do
        events <- liftIO $ SDL.pollEvents

        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        forM_ events $ \event ->
            case eventPayload event of
                KeyboardEvent (KeyboardEventData win motion rep sym) -> do
                    case keysymScancode sym of
                        SDL.ScancodeC -> case motion of
                            Pressed -> usingStella $ swchb . bitAt 1 .= False
                            Released -> usingStella $ swchb .bitAt 1 .= True
                        SDL.ScancodeV -> case motion of
                            Pressed -> usingStella $ swchb . bitAt 0 .= False
                            Released -> usingStella $ swchb .bitAt 0 .= True
                otherwise -> return ()

        liftIO $ lockSurface screenSurface
        unM $ times 10000 step
        liftIO $ unlockSurface screenSurface

        liftIO $ SDL.surfaceBlitScaled helloWorld Nothing screenSurface (Just (Rectangle (P (V2 0 0)) (V2 (screenWidth*scale) (screenHeight*scale))))
        liftIO $ SDL.updateWindowSurface window
        unless quit $ loop (n+1)

  flip runStateT state $ loop 0

  SDL.destroyWindow window
  SDL.freeSurface helloWorld
  SDL.quit
