{-# LANGUAGE CPP #-}

module System.Evdev.KeyCode
  ( KeyCode (..),
    fromKeyCode,
  )
where

#include <linux/input-event-codes.h>
data KeyCode = KeyEsc | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9 | Key0 | KeyMinus | KeyEqual | KeyBackspace | KeyTab | KeyQ | KeyW | KeyE | KeyR | KeyT | KeyY | KeyU | KeyI | KeyO | KeyP | KeyLeftbrace | KeyRightbrace | KeyEnter | KeyLeftctrl | KeyA | KeyS | KeyD | KeyF | KeyG | KeyH | KeyJ | KeyK | KeyL | KeySemicolon | KeyApostrophe | KeyGrave | KeyLeftshift | KeyBackslash | KeyZ | KeyX | KeyC | KeyV | KeyB | KeyN | KeyM | KeyComma | KeyDot | KeySlash | KeyRightshift | KeyLeftalt | KeySpace | KeyUp | KeyLeft | KeyRight | KeyDown deriving (Show, Eq, Ord)

-- What the fuck

fromKeyCode :: (Integral a) => a -> Maybe KeyCode
fromKeyCode KEY_ESC = Just KeyEsc
fromKeyCode KEY_1 = Just Key1
fromKeyCode KEY_2 = Just Key2
fromKeyCode KEY_3 = Just Key3
fromKeyCode KEY_4 = Just Key4
fromKeyCode KEY_5 = Just Key5
fromKeyCode KEY_6 = Just Key6
fromKeyCode KEY_7 = Just Key7
fromKeyCode KEY_8 = Just Key8
fromKeyCode KEY_9 = Just Key9
fromKeyCode KEY_0 = Just Key0
fromKeyCode KEY_MINUS = Just KeyMinus
fromKeyCode KEY_EQUAL = Just KeyEqual
fromKeyCode KEY_BACKSPACE = Just KeyBackspace
fromKeyCode KEY_TAB = Just KeyTab
fromKeyCode KEY_Q = Just KeyQ
fromKeyCode KEY_W = Just KeyW
fromKeyCode KEY_E = Just KeyW
fromKeyCode KEY_R = Just KeyR
fromKeyCode KEY_T = Just KeyT
fromKeyCode KEY_Y = Just KeyY
fromKeyCode KEY_U = Just KeyU
fromKeyCode KEY_I = Just KeyI
fromKeyCode KEY_O = Just KeyO
fromKeyCode KEY_P = Just KeyP
fromKeyCode KEY_LEFTBRACE = Just KeyLeftbrace
fromKeyCode KEY_RIGHTBRACE = Just KeyRightbrace
fromKeyCode KEY_ENTER = Just KeyEnter
fromKeyCode KEY_LEFTCTRL = Just KeyLeftctrl
fromKeyCode KEY_A = Just KeyA
fromKeyCode KEY_S = Just KeyS
fromKeyCode KEY_D = Just KeyD
fromKeyCode KEY_F = Just KeyF
fromKeyCode KEY_G = Just KeyG
fromKeyCode KEY_H = Just KeyH
fromKeyCode KEY_J = Just KeyJ
fromKeyCode KEY_K = Just KeyK
fromKeyCode KEY_L = Just KeyL
fromKeyCode KEY_SEMICOLON = Just KeySemicolon
fromKeyCode KEY_APOSTROPHE = Just KeyApostrophe
fromKeyCode KEY_GRAVE = Just KeyGrave
fromKeyCode KEY_LEFTSHIFT = Just KeyLeftshift
fromKeyCode KEY_BACKSLASH = Just KeyBackslash
fromKeyCode KEY_Z = Just KeyZ
fromKeyCode KEY_X = Just KeyX
fromKeyCode KEY_C = Just KeyC
fromKeyCode KEY_V = Just KeyV
fromKeyCode KEY_B = Just KeyB
fromKeyCode KEY_N = Just KeyN
fromKeyCode KEY_M = Just KeyM
fromKeyCode KEY_COMMA = Just KeyComma
fromKeyCode KEY_DOT = Just KeyDot
fromKeyCode KEY_SLASH = Just KeySlash
fromKeyCode KEY_RIGHTSHIFT = Just KeyRightshift
fromKeyCode KEY_LEFTALT = Just KeyLeftalt
fromKeyCode KEY_SPACE = Just KeySpace
fromKeyCode KEY_UP = Just KeyUp
fromKeyCode KEY_LEFT = Just KeyLeft
fromKeyCode KEY_RIGHT = Just KeyRight
fromKeyCode KEY_DOWN = Just KeyDown
fromKeyCode _ = Nothing
