{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module App
  ( Key (..),
    Env (..),
    app,
    makeKey,
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Set as S
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import qualified Raylib.Types as Ray
import qualified Raylib.Util.Colors as Col
import System.Evdev.Event
import System.Evdev.KeyCode
import System.IO

data Key = Key {keyKey :: KeyCode, keyLabel :: String, keyPosition :: Ray.Vector2, keySize :: Ray.Vector2}

instance Eq Key where
  k1 == k2 = keyKey k1 == keyKey k2

newtype AppState = AppState {_keysPressed :: S.Set KeyCode}

makeLenses ''AppState

data Env = Env
  { configInputFile :: String,
    configWindowSize :: (Int, Int),
    configWindowScaling :: Int,
    configWindowFps :: Int,
    configKeyDisplay :: [Key],
    configKeyReleasedColor :: Ray.Color,
    configKeyPressedColor :: Ray.Color,
    configFontSize :: Int,
    configSizeofTimeT :: Int,
    configSizeofSusecondsT :: Int
  }

data Event = Event
  { evTimeSecs :: Int,
    evTimeUsecs :: Int,
    evType :: Int,
    evCode :: Int,
    evValue :: Int
  }
  deriving (Show)

withOpenDevice :: (MonadIO m) => String -> (Handle -> m a) -> m a
withOpenDevice inputFile f = do
  dev <- liftIO $ openBinaryFile inputFile ReadMode
  ret <- f dev
  liftIO $ hClose dev
  pure ret

drawKey :: (MonadReader Env m, MonadIO m) => Key -> Bool -> m ()
drawKey key keyPressed = do
  Env
    { configKeyReleasedColor = relCol,
      configKeyPressedColor = pressCol,
      configWindowScaling = scale,
      configFontSize = fontsize
    } <-
    ask
  let Ray.Vector2 x_ y_ = keyPosition key
      Ray.Vector2 w_ h_ = keySize key
      (x, y) = (scale * round x_, scale * round y_)
      (w, h) = (scale * round w_, scale * round h_)
      midX = x + w `div` 2
      midY = y + h `div` 2

  labelWidth <- liftIO $ measureText (keyLabel key) fontsize

  liftIO $ do
    drawRectangle x y w h (if keyPressed then pressCol else relCol)
    drawRectangleLines x y w h Col.white
    drawText (keyLabel key) (midX - labelWidth `div` 2) (midY - fontsize `div` 2) fontsize Col.black

drawFrame :: (MonadReader Env m, MonadIO m) => AppState -> m ()
drawFrame appState = do
  Env {configKeyDisplay = keys} <- ask
  liftIO beginDrawing
  liftIO $ clearBackground Col.blank
  mapM_ (\k -> drawKey k $ S.member (keyKey k) (appState ^. keysPressed)) keys
  liftIO endDrawing

appLoop :: (MonadReader Env m, MonadState AppState m, MonadIO m) => Handle -> m ()
appLoop device = do
  Env
    { configSizeofTimeT = timeTSize,
      configSizeofSusecondsT = susecondsTSize
    } <-
    ask
  input <- fmap getKeyEvent . readEvent timeTSize susecondsTSize $ device
  get >>= drawFrame
  case input of
    Just (KeyEvent KeyDepress k) -> keysPressed %= S.insert k
    Just (KeyEvent KeyRelease k) -> keysPressed %= S.delete k
    _ -> pure ()

  whenM (not <$> liftIO windowShouldClose) $ appLoop device

makeKey :: KeyCode -> String -> (Int, Int) -> (Int, Int) -> Key
makeKey code label (x, y) (w, h) =
  Key
    { keyKey = code,
      keyLabel = label,
      keyPosition = Ray.Vector2 (fromIntegral x) (fromIntegral y),
      keySize = Ray.Vector2 (fromIntegral w) (fromIntegral h)
    }

app :: Env -> IO ()
app config = do
  let Env
        { configInputFile = inputFile,
          configWindowSize = (winw, winh),
          configWindowScaling = scale
        } = config
  setConfigFlags [Ray.WindowTransparent, Ray.WindowUndecorated]
  window <- initWindow (scale * winw) (scale * winh) "hello"
  setTargetFPS (configWindowFps config)
  void . withOpenDevice inputFile $ \dev ->
    runReaderT
      (runStateT (appLoop dev) $ AppState S.empty)
      config
  closeWindow $ Just window
