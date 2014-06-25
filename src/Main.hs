module Main where

import Data.Array.Base
import qualified Data.ByteString as BS
import System.IO
import System.Random (newStdGen)
import Numeric (showHex)

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color (black, white)
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Pure.Game

import Chip8.State (VMState(..), create, nextInstruction, showDisplay)
import Chip8.Opcodes (runInstruction)

-- | Runs the next instruction on the VM state and returns the resulting state
step :: VMState  -- ^ The starting state
     -> VMState  -- ^ The stepped through state
step s@VMState { pc = pc, memory = memory } =
    runInstruction s' op
  where
    op = nextInstruction s
    s' = s { pc = pc + 2 }

-- | Steps through a program for each input
stepLoop :: VMState -> IO ()
stepLoop s@VMState { pc = pc, v = v, i = i, stack = stack} = do
    putStr "PC: "
    putStrLn $ showHex pc ""

    putStr "Instruction: "
    putStrLn $ showHex (nextInstruction s) ""

    putStr "V: "
    print v

    putStr "I: "
    print i

    putStr "Stack: "
    print stack

    putStr $ showDisplay s

    putStr "Press enter to step"
    hFlush stdout
    getLine
    stepLoop $ step s

-- | Runs a program until it loops indefinitely
run :: VMState -> IO ()
run s@VMState { pc = pc } = do
    -- Print the current display
    putStr $ showDisplay s
    hFlush stdout

    -- Run the next instruction
    let s'@VMState { pc = pc' } = step s

    -- If the program counter hasn't changed, then
    if pc == pc'
    then return ()
    else run s'

drawScreen :: VMState -> Picture
drawScreen s@VMState { display = d } =
    scale 1 (-1) $
    translate (-320) (-160) $
    pictures [ color white (translate x' y' pixel)
        | x <- [0,1..63]
        , y <- [0,1..31]
        , let x' = fromIntegral (x * 10)
        , let y' = fromIntegral (y * 10)
        , d ! (x, y) ]
  where
    pixel = rectangleSolid 10 10

-- | Runs the programs with real(tm) graphics!
runGraphical :: VMState -> IO ()
runGraphical state =
    play window black 100 state drawScreen handleInput step'
  where
    handleInput _ s = s
    step' _ = step
    window = InWindow "CHIP-8" (660, 340) (10, 10)

main :: IO ()
main = do
    program <- BS.readFile "./roms/LOGO"
    randGen <- newStdGen
    let vm = create (BS.unpack program) randGen
    runGraphical vm