module Chip8VM
( loadProgram
, step
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
                        , pc = 0x200 -- CHIP-8 programs start here in memory
                        , i = 0x0
                        , v = listArray (0, 16) [] }

runInstruction :: VMState -> Word8 -> Word8 -> VMState

-- '1nnn' - Jump to location nnn
runInstruction s 0x1000 ops =
  s { pc = ops .&. 0x0FFF}

-- '3xkk' - Skip to next instruction if Vx == kk
runInstruction s 0x3000 ops =
  if vx == kk then s { pc = (pc s) + (2 :: Word8) } else s
  where x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
        kk = ops .&. 0x00FF
        vx = (v s) ! x

-- '6xkk' - Set Vx = kk
runInstruction s 0x6000 ops = s { v = v' }
  where x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
        kk = ops .&. 0x00FF
        v' = (v s) // [(x, kk)]

-- '7xkk' - Set Vx = Vx + kk
runInstruction s 0x7000 ops = s { v = v' }
  where x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
        vx = (v s) ! x
        kk = ops .&. 0x00FF
        v' = (v s) // [(x, vx + kk)]

-- 'Annn' - Set I = nnn
runInstruction s 0xA000 ops = s { i = fromIntegral nnn }
  where nnn = ops .&. 0x0FFF

-- 'Cxkk' - Set Vx = random byte AND kk
runInstruction s 0xC000 ops = s { v = v' }
  where x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
        kk = ops .&. 0x00FF
        rand = 255 -- Totally random, right? Still unsure how I should do this
        v' = (v s) // [(x, rand .&. kk)]

-- 'Dxyn' - Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision
runInstruction s 0xD000 ops = s

step :: VMState -> VMState
step s = runInstruction nextState opcode instruction
  where instruction = (shiftL ((memory s) ! (fromIntegral (pc s))) 8) + ((memory s) ! ((fromIntegral (pc s)) + 1))
        opcode = instruction .&. 0xF000
        nextState = s { pc = (pc s) + (2 :: Word8) }

main :: IO ()
main = do
  putStrLn "Hello, world!"