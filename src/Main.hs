module Main where

import Data.Array.Base
import Data.Char
import qualified Data.Set as S
import qualified Data.ByteString as BS
import System.IO
import System.Random (newStdGen)
import Numeric (showHex)
import Data.Maybe

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color (black, white)
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Pure.Game

import Chip8.State (VMState(..), create, nextInstruction, showDisplay)
import Chip8.Opcodes (runInstruction)

-- | Runs the next instruction on the VM state and returns the resulting state
step :: VMState  -- ^ The starting state
     -> VMState  -- ^ The stepped through state
step s@VMState { pc = pc, memory = memory, delayTimer = delayTimer } =
    case waitForKeypress s of
        Nothing -> runInstruction s' op
        Just _ -> s
  where
    op = nextInstruction s
    delayTimer' = if delayTimer > 0 then delayTimer - 1 else delayTimer
    s' = s { pc = pc + 2, delayTimer = delayTimer' }

-- | Generates a Gloss picture to represent the current state's display
drawScreen :: VMState -> Picture
drawScreen s@VMState { display = d } = color white $
    pictures [
        translate 200 160 $
        scale 0.25 0.25 $
        text $ "del:" ++ show (delayTimer s),

        translate (-40) 160 $
        scale 0.25 0.25 $
        text $ "pc:" ++ showHex (pc s) "",

        translate (-320) 160 $
        scale 0.25 0.25 $
        text $ "op:" ++ showHex (nextInstruction s) "",

        translate (-320) 140 $
        scale 1 (-1) $
        pictures
        [translate x' y' pixel
            | x <- [0,1..63]
            , y <- [0,1..31]
            , let x' = fromIntegral (x * 10)
            , let y' = fromIntegral (y * 10)
            , d ! (x, y)]]
  where
    pixel = rectangleSolid 10 10

-- | Handles keyboard input by adding/removing pressed keys from the state
handleInput :: Event -> VMState -> VMState
handleInput (EventKey (Char c) ks _ _) s@VMState { pressed = pressed, v = v } =
    if isHexDigit c
    then case waitForKeypress s of
        Nothing -> s { pressed = pressed' }
        Just x -> s { pressed = pressed'
                    , v = v // [(x, fromIntegral c')]  -- Set VX to the key
                    , waitForKeypress = Nothing }      -- Remove the wait flag
    else s
  where
    c' = digitToInt c
    pressed' = case ks of
        Down -> S.insert c' pressed
        Up -> S.delete c' pressed

handleInput _ s = s

-- | Runs the programs with real(tm) graphics!
run :: VMState -> IO ()
run state =
    play
        window       -- Window info
        black        -- Background colour
        100          -- Steps per second (100Hz)
        state        -- Starting state
        drawScreen   -- Display generating function
        handleInput  -- Input handling function
        step'        -- State stepping function
  where
    step' _ = step
    window = InWindow "CHIP-8" (660, 380) (10, 10)

main :: IO ()
main = do
    program <- BS.readFile "./roms/BRIX"
    randGen <- newStdGen
    run $ create (BS.unpack program) randGen