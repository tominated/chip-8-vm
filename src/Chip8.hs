module Chip8 where

import Data.Word
import Data.Bits
import Data.Bool
import Data.Array.Unboxed
import Data.Array.Base
import qualified Data.ByteString as BS
import System.IO
import System.Random
import Numeric (showHex)

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

main :: IO ()
main = do
    program <- BS.readFile "../roms/LOGO"
    randGen <- newStdGen
    let vm = create (BS.unpack program) randGen
    stepLoop vm