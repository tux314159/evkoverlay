{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import App
import System.Evdev.KeyCode
import qualified Raylib.Types as Ray
import qualified Raylib.Util.Colors as Col

jumpKey1 :: Key
jumpKey1 = makeKey KeyC "jump" (4, 3) (2, 1)

jumpKey2 :: Key
jumpKey2 = makeKey KeySpace "jump" (4, 2) (2, 1)

dashKey :: Key
dashKey = makeKey KeyX "dash" (2, 2) (2, 1)

demoKey :: Key
demoKey = makeKey KeyLeftalt "demo" (2, 3) (2, 1)

grabKey :: Key
grabKey = makeKey KeyZ "grab" (0, 2) (2, 2)

leftKey :: Key
leftKey = makeKey KeyLeft "<" (0, 0) (2, 2)

upKey :: Key
upKey = makeKey KeyUp "^" (2, 0) (2, 1)

downKey :: Key
downKey = makeKey KeyDown "v" (2, 1) (2, 1)

rightKey :: Key
rightKey = makeKey KeyRight ">" (4, 0) (2, 2)

defaultAppConfig :: Env
defaultAppConfig =
  Env
    { configInputFile = "/dev/input/by-path/platform-i8042-serio-0-event-kbd",
      configWindowSize = (7, 5),
      configWindowScaling = 40,
      configWindowFps = 144,
      configKeyDisplay = [jumpKey1, jumpKey2, dashKey, demoKey, grabKey, leftKey, upKey, downKey, rightKey],
      configKeyReleasedColor = Ray.Color 70 70 70 200,
      configKeyPressedColor = Col.blue,
      configFontSize = 20,
      -- don't touch unless you know what these are
      configSizeofTimeT = 8,
      configSizeofSusecondsT = 8
    }

main :: IO ()
main = app defaultAppConfig
