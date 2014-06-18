module Chip8VM
( createVM
, step
, runInstruction
) where

import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Array.Base
import qualified Data.ByteString as BS
import System.IO

data VMState = VMState { memory :: UArray Int Word8 -- VM Memory
                       , pc :: Int                  -- Program counter
                       , i :: Word16                -- 16-bit register
                       , v :: UArray Int Word8      -- 8-bit registers
                       } deriving (Show)

createVM :: [Word8] -> VMState
createVM p = VMState { memory = listArray (0x0, 0xFFF) memContents
                     , pc = 0x200 -- CHIP-8 programs start here in memory
                     , i = 0x0
                     , v = listArray (0, 16) [] }
  where memContents = (replicate 0x200 (0x0 :: Word8)) ++ p

runInstruction :: VMState -> Word8 -> Word8 -> VMState

-- '1nnn' - Jump to location nnn
runInstruction s 0x1000 ops =
  s { pc = fromIntegral $ ops .&. 0x0FFF}

-- '3xkk' - Skip to next instruction if Vx == kk
runInstruction s 0x3000 ops =
  if vx == kk then s { pc = (pc s) + 2 } else s
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

-- This is broken. 'instruction' wraps around at 255 which isn't wanted
step :: VMState -> VMState
step s = runInstruction nextState opcode instruction
  where instruction = (shiftL ((memory s) ! (fromIntegral (pc s))) 8) + ((memory s) ! ((fromIntegral (pc s)) + 1))
        opcode = instruction .&. 0xF000
        nextState = s { pc = (pc s) + 2 }

stepLoop :: VMState -> IO ()
stepLoop s = do
  putStr "PC: "
  print $ pc s

  putStr "V: "
  print $ v s

  putStr "I: "
  print $ i s

  putStr "Press enter to step"
  hFlush stdout
  getLine
  stepLoop $ step s

main :: IO ()
main = do
  program <- BS.readFile "roms/MAZE"
  let vm = createVM $ BS.unpack program
  stepLoop vm