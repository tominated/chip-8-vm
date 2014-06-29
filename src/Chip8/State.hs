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
import qualified Data.Set as S
import System.Random
import Data.Maybe

-- | Represents the state of a CHIP-8 VM at any given time
data VMState = VMState
    { memory :: UArray Word Word           -- ^ VM Memory
    , pc :: Word                           -- ^ Program counter
    , i :: Word                            -- ^ 16-bit register
    , v :: UArray Word Word                -- ^ 8-bit registers
    , stack :: [Word]                      -- ^ The call stack
    , display :: UArray (Word, Word) Bool  -- ^ Simulates a b/w display
    , pressed :: S.Set Int                 -- ^ Set of currently pressed keys
    , delayTimer :: Word                   -- ^ The delay timer wait value
    , waitForKeypress :: Maybe Word        -- ^ If set, store next keypress in this register
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
                     , pressed = S.empty
                     , delayTimer = 0
                     , waitForKeypress = Nothing
                     , randGen = g }
  where
    reservedMem = font ++ replicate (0x200 - length font) 0x0
    memContents = reservedMem ++ map fromIntegral p

-- | The Chip-8 font represented as sprites
font :: [Word]
font = [ 0xF0, 0x90, 0x90, 0x90, 0xF0,   -- 0
         0x20, 0x60, 0x20, 0x20, 0x70,   -- 1
         0xF0, 0x10, 0xF0, 0x80, 0xF0,   -- 2
         0xF0, 0x10, 0xF0, 0x10, 0xF0,   -- 3
         0x90, 0x90, 0xF0, 0x10, 0x10,   -- 4
         0xF0, 0x80, 0xF0, 0x10, 0xF0,   -- 5
         0xF0, 0x80, 0xF0, 0x90, 0xF0,   -- 6
         0xF0, 0x10, 0x20, 0x40, 0x40,   -- 7
         0xF0, 0x90, 0xF0, 0x90, 0xF0,   -- 8
         0xF0, 0x90, 0xF0, 0x10, 0xF0,   -- 9
         0xF0, 0x90, 0xF0, 0x90, 0x90,   -- A
         0xE0, 0x90, 0xE0, 0x90, 0xE0,   -- B
         0xF0, 0x80, 0x80, 0x80, 0xF0,   -- C
         0xE0, 0x90, 0x90, 0x90, 0xE0,   -- D
         0xF0, 0x80, 0xF0, 0x80, 0xF0,   -- E
         0xF0, 0x80, 0xF0, 0x80, 0x80 ]  -- F

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