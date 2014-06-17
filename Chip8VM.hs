module Chip8VM
( step
) where

import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Array.Base

data VMState = VMState { memory :: UArray Int Word8 -- VM Memory
                       , pc :: Word8                -- Program counter
                       , i :: Word16                -- 16-bit register
                       , v :: UArray Int Word8      -- 8-bit registers
                       } deriving (Show)

loadProgram :: [Word8] -> VMState
loadProgram p = VMState { memory = listArray (0, 4095) p
                        , pc = 0x200
                        , i = 0x0
                        , v = listArray (0, 16) [] }

runInstruction :: VMState -> Word8 -> Word8 -> VMState
runInstruction s 0x1000 ops = s
runInstruction s 0x3000 ops = s
runInstruction s 0x6000 ops = s
runInstruction s 0x7000 ops = s
runInstruction s 0xA000 ops = s
runInstruction s 0xC000 ops = s
runInstruction s 0xD000 ops = s

step :: VMState -> VMState
step s = runInstruction nextState opcode instruction
  where instruction = (((memory s) ! (pc s)) shiftL 8) + ((memory s) ! ((pc s) + 1))
        opcode = instruction .&. 0xF000
        nextState = s { pc = (pc s) + (2 :: Word8) }

main :: IO ()
main = do
  putStrLn "Hello, world!"