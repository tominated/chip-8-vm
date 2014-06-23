module Chip8VM
    (
      createVM
    , step
    , runInstruction
    , getSprite
    ) where

import Data.Word
import Data.Bits
import Data.Bool
import Data.Array.Unboxed
import Data.Array.Base
import qualified Data.ByteString as BS
import System.IO
import System.Random
import Numeric (showHex)

-- | Represents the state of a CHIP-8 VM at any given time
data VMState = VMState
    { memory :: UArray Word16 Word8            -- ^ VM Memory
    , pc :: Word16                             -- ^ Program counter
    , i :: Word16                              -- ^ 16-bit register
    , v :: UArray Word16 Word8                 -- ^ 8-bit registers
    , stack :: [Word16]                        -- ^ The call stack
    , display :: UArray (Word16, Word16) Bool  -- ^ Simulates a b/w display
    , randGen :: StdGen                        -- ^ Generator for random nums
    } deriving (Show)

-- | Creates a new VM state for a given program ROM
createVM :: [Word8]  -- ^ A byte array of the ROM to load
         -> StdGen   -- ^ A random number generator to use
         -> VMState  -- ^ A VM with the ROM in memory
createVM p g = VMState { memory = listArray (0x0, 0xFFF) memContents
                       , pc = 0x200 -- CHIP-8 programs start here in memory
                       , i = 0x0
                       , v = listArray (0x0, 0xF) []
                       , stack = []
                       , display = listArray ((0,0),(63,31)) (repeat False)
                       , randGen = g }
  where
    memContents = (replicate 0x200 (0x0 :: Word8)) ++ p

nextInstruction :: VMState  -- ^ The starting state
                -> Word     -- ^ The next instruction to run
nextInstruction VMState { pc = pc, memory = memory } =
    (shiftL b1 8) + b2
  where
    b1 = fromIntegral $ memory ! pc       -- First byte in instruction
    b2 = fromIntegral $ memory ! (pc + 1) -- Second byte in instruction

-- | Runs a cpu instruction on VM state and returns the resulting state
runInstruction :: VMState  -- ^ The starting state
               -> Word     -- ^ The instruction opcode
               -> Word     -- ^ The instruction operands
               -> VMState  -- ^ The manipulated state

-- '0000' - Multiple instructions
runInstruction s 0x0000 ops = runOp ops
  where
    -- Clear screen
    runOp 0x00E0 = s { display = listArray ((0,0),(63,31)) (repeat False) }

    -- Return from subroutine
    runOp 0x00EE = s { pc = pc', stack = stack' }
      where
        (pc' : stack') = stack s

-- '1nnn' - Jump to location nnn
runInstruction s 0x1000 ops =
    s { pc = nnn }
  where
    nnn = fromIntegral $ ops .&. 0x0FFF

-- '2nnn' - Call subroutine at location nnn
runInstruction s 0x2000 ops =
    s { pc = nnn, stack = stack' }
  where
    nnn = fromIntegral $ ops .&. 0x0FFF
    stack' = (pc s) : (stack s)

-- '3xkk' - Skip to next instruction if Vx == kk
runInstruction s 0x3000 ops = if vx == kk
                              then s { pc = (pc s) + 2 }
                              else s
  where
    x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
    kk = ops .&. 0x00FF
    vx = fromIntegral $ (v s) ! x

-- '4xkk' - Skip next instruction if Vx != kk
runInstruction s 0x4000 ops = if vx /= kk
                              then s { pc = (pc s) + 2 }
                              else s
  where
    x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
    kk = ops .&. 0x00FF
    vx = fromIntegral $ (v s) ! x

-- '6xkk' - Set Vx = kk
runInstruction s 0x6000 ops = s { v = v' }
  where
    x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
    kk = fromIntegral $ ops .&. 0x00FF
    v' = (v s) // [(x, kk)]

-- '7xkk' - Set Vx = Vx + kk
runInstruction s 0x7000 ops = s { v = v' }
  where
    x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
    vx = (v s) ! x
    kk = fromIntegral $ ops .&. 0x00FF
    v' = (v s) // [(x, vx + kk)]

-- 'Annn' - Set I = nnn
runInstruction s 0xA000 ops = s { i = fromIntegral nnn }
  where
    nnn = ops .&. 0x0FFF

-- 'Cxkk' - Set Vx = random byte AND kk
runInstruction s 0xC000 ops = s { v = v', randGen = randGen' }
  where
    x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
    kk = fromIntegral $ ops .&. 0x00FF
    (rand, randGen') = randomR (0x0, 0xFF) (randGen s)
    v' = (v s) // [(x, rand .&. kk)]

-- 'Dxyn' - Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision
runInstruction s 0xD000 ops = s' { v = (v s) // [(0xF, collision')] }
  where
    x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
    y = fromIntegral $ shiftR (ops .&. 0x00F0) $ fromIntegral 4
    n = fromIntegral $ ops .&. 0x000F
    vx = fromIntegral $ (v s) ! x
    vy = fromIntegral $ (v s) ! y
    (s', collision) = drawSprite s vx vy (i s) n
    collision' = if collision then 1 else 0

-- Naive implementation
runInstruction s 0xF000 ops = s { i = (i s) + vx }
  where
    x = fromIntegral $ shiftR (ops .&. 0x0F00) $ fromIntegral 8
    vx = fromIntegral $ (v s) ! x

runInstruction s _ _ = s

-- | Gets a sprite from a memory location and returns it's pixel coordinates
getSprite :: VMState             -- ^ The VM state
          -> Word16              -- ^ The memory address of the sprite
          -> Word                -- ^ The byte length of the sprite in memory
          -> [(Word16, Word16)]  -- ^ Pixel coordinates representing the sprite
getSprite s addr n =
    [(fromIntegral x - 4, fromIntegral y) -- I have no idea why -4 is needed.
        | y <- range (0, n)
        , x <- [7,6..0]
        , let line = addr + fromIntegral y
        , (shiftR ((memory s) ! line) x) .&. 1 == 1]

-- | Draws a sprite on the display and finds if there is a collision
drawSprite :: VMState          -- ^ The VM state
           -> Word16           -- ^ X coordinate to draw from
           -> Word16           -- ^ Y coordinate to draw from
           -> Word16           -- ^ The memory address of the sprite
           -> Word             -- ^ The byte length of the sprite in memory
           -> (VMState, Bool)  -- ^ The new state and if a collision occured
drawSprite s x y addr n = (s { display = (display s) // display' }, collision)
  where
    addOffset (sx, sy) = (sx + x, sy + y)
    inBounds (x, y) = (x >= 0 && x < 64) && (y >= 0 && y < 32)
    sprite = filter inBounds $ map addOffset $ getSprite s addr n
    collides _ True = True
    collides coord _ = not (boolXor ((display s) ! coord) True)
    collision = foldr collides False sprite
    display' = map (\coord -> (coord, True)) sprite

-- | Performs a XOR bitwise operation on two boolean values
boolXor :: Bool -> Bool -> Bool
boolXor True True = False
boolXor False False = False
boolXor True False = True
boolXor False True = True

-- | Runs the next instruction on the VM state and returns the resulting state
step :: VMState  -- ^ The starting state
     -> VMState  -- ^ The stepped through state
step s@VMState { pc = pc, memory = memory } = runInstruction s' op instr
  where
    b1 = fromIntegral $ memory ! pc :: Word -- First byte in instruction
    b2 = fromIntegral $ memory ! (pc + 1) :: Word -- Second byte in instruction
    instr = (shiftL b1 8) + b2
    op = instr .&. 0xF000
    s' = s { pc = pc + 2 }

-- | Returns a string representation of a VM state's display
getDisplay :: VMState  -- ^ The VM state
           -> String   -- ^ A string showing the display contents using ascii
getDisplay s =
    unlines [unwords [toPixel ((display s) ! (x, y)) | x <- [0..63]] | y <- [0..31]]
  where
    toPixel True = "█"
    toPixel False = " "

-- | Steps through a program for each input
stepLoop :: VMState -> IO ()
stepLoop s = do
    putStr "PC: "
    putStrLn $ showHex (pc s) ""

    putStr "Instruction: "
    putStrLn $ showHex (nextInstruction s) ""

    putStr "V: "
    print $ v s

    putStr "I: "
    print $ i s

    putStr "Stack: "
    print $ stack s

    putStr $ getDisplay s

    putStr "Press enter to step"
    hFlush stdout
    getLine
    stepLoop $ step s

main :: IO ()
main = do
    program <- BS.readFile "roms/LOGO"
    randGen <- newStdGen
    let vm = createVM (BS.unpack program) randGen
    stepLoop vm