module Chip8.State
    (
      VMState(..)
    , create
    , nextInstruction
    , showDisplay
    ) where

import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Array.Base
import System.Random

-- | Represents the state of a CHIP-8 VM at any given time
data VMState = VMState
    { memory :: UArray Word Word           -- ^ VM Memory
    , pc :: Word                           -- ^ Program counter
    , i :: Word                            -- ^ 16-bit register
    , v :: UArray Word Word                -- ^ 8-bit registers
    , stack :: [Word]                      -- ^ The call stack
    , display :: UArray (Word, Word) Bool  -- ^ Simulates a b/w display
    , randGen :: StdGen                    -- ^ Generator for random nums
    } deriving (Show)

-- | Creates a new VM state for a given program ROM
create :: [Word8]    -- ^ A byte array of the ROM to load
         -> StdGen   -- ^ A random number generator to use
         -> VMState  -- ^ A VM with the ROM in memory
create p g = VMState { memory = listArray (0x0, 0xFFF) memContents
                     , pc = 0x200 -- CHIP-8 programs start here in memory
                     , i = 0x0
                     , v = listArray (0x0, 0xF) []
                     , stack = []
                     , display = listArray ((0,0),(63,31)) (repeat False)
                     , randGen = g }
  where
    memContents = replicate 0x200 0x0 ++ map fromIntegral p

-- | Retrieves the next CPU instruction for the a vm
nextInstruction :: VMState  -- ^ The starting state
                -> Word     -- ^ The next instruction to run
nextInstruction VMState { pc = pc, memory = memory } =
    shiftL b1 8 + b2
  where
    b1 = fromIntegral $ memory ! pc       -- First byte in instruction
    b2 = fromIntegral $ memory ! (pc + 1) -- Second byte in instruction

-- | Returns a string representation of a VM state's display
showDisplay :: VMState  -- ^ The VM state
            -> String   -- ^ A string showing the display contents using ascii
showDisplay s =
    unlines [unwords [toPixel (display s ! (x, y)) | x <- [0..63]] | y <- [0..31]]
  where
    toPixel True = "█"
    toPixel False = " "