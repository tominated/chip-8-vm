module Chip8.Opcodes
    (
      runInstruction
    ) where

import Data.Word
import Data.Bits
import Data.Bool
import Data.Array.Unboxed
import Data.Array.Base
import System.Random

import Chip8.State (VMState(..))

runInstruction :: VMState  -- ^ Initial CPU state
               -> Word     -- ^ Full CPU instruction
               -> VMState  -- ^ Resulting CPU state
runInstruction s operands = op s operands
  where
    op = case (operands .&. 0xF000) of
      0x0000 -> case (operands .&. 0xF) of
        0x0000 -> op00E0
        0x000E -> op00EE
      0x1000 -> op1NNN
      0x2000 -> op2NNN
      0x3000 -> op3XKK
      0x4000 -> op4XKK
      0x6000 -> op6XKK
      0x7000 -> op7XKK
      0xA000 -> opANNN
      0xC000 -> opCXKK
      0xD000 -> opDXYN
      0xF000 -> opFX1E

-- | Get the NNN value from an instruction
iNNN :: Word -> Word
iNNN = (.&.) 0x0FFF

-- | Get the KK value from an instruction
iKK :: Word -> Word
iKK = (.&.) 0x00FF

-- | Get the N value from an instruction
iN :: Word -> Word
iN = (.&.) 0x000F

-- | Get the X value from an instruction
iX :: Word -> Word
iX op = shiftR (op .&. 0x0F00) 8

-- | Get the Y value from an instruction
iY :: Word -> Word
iY op = shiftR (op .&. 0x00F0) 4

-- | Clear the display
op00E0 :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op00E0 s op =
    s

-- | Return from subroutine
op00EE :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op00EE s op =
    s { pc = pc', stack = stack' }
  where
    (pc' : stack') = stack s

-- | Jump to location NNN
op1NNN :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op1NNN s op =
    s { pc = iNNN op }

-- | Call subroutine at location NNN
op2NNN :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op2NNN s@VMState { pc = pc, stack = stack } op =
    s { pc = iNNN op, stack = stack' }
  where
    stack' = pc : stack

-- | Skip next instruction if VX == KK
op3XKK :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op3XKK s@VMState { pc = pc, v = v } op =
    if vx == (iKK op)
    then s { pc = pc + 2 }
    else s
  where
    vx = v ! (iX op)

-- | Skip next instruction if VX != KK
op4XKK :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op4XKK s@VMState { pc = pc, v = v } op =
    if vx /= (iKK op)
    then s { pc = pc + 2 }
    else s
  where
    vx = v ! (iX op)

-- | Set VX = KK
op6XKK :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op6XKK s@VMState { v = v } op =
    s { v = v' }
  where
    v' = v // [(iX op, iKK op)]

-- | Set VX = VX + KK
op7XKK :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op7XKK s@VMState { v = v } op =
    s { v = v' }
  where
    vx = v ! (iX op)
    v' = v // [(iX op, vx + (iKK op))]

-- | Set i = NNN
opANNN :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opANNN s op =
    s { i = iNNN op }

-- | Set VX = (random byte) & KK
opCXKK :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opCXKK s@VMState { v = v, randGen = randGen } op =
    s { v = v', randGen = randGen' }
  where
    (r, randGen') = randomR (0x0, 0xFF) randGen
    v' = v // [(iX op, r .&. (iKK op))]

-- | Paint N byte long sprite starting at location I at (VX, VY)
--   Also set VF = collision
opDXYN :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opDXYN s@VMState { v = v, i = i } op =
    s' { v = v' }
  where
    vx = v ! (iX op)
    vy = v ! (iY op)
    (collision, s') = drawSprite s vx vy i (iN op)
    collision' = if collision then 1 else 0
    v' = v // [(0xF, collision')]

-- | Set op = I + VX
opFX1E :: VMState  -- ^ Initial CPU state
       -> Word     -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX1E s@VMState { v = v } op =
    s { i = (i s) + vx }
  where
    vx = v ! (iX op)

-- | Gets a sprite from a memory location and returns it's pixel coordinates
getSprite :: VMState         -- ^ The VM state
          -> Word            -- ^ The memory address of the sprite
          -> Word            -- ^ The byte length of the sprite in memory
          -> [(Word, Word)]  -- ^ Pixel coordinates representing the sprite
getSprite s addr n =
    [(fromIntegral x, fromIntegral y)
        | y <- range (0, n - 1)
        , x <- [0,1..7]
        , let shift = 7 - x -- This prevents the sprite from being flipped
        , (shiftR ((memory s) ! (addr + y)) shift) .&. 1 == 1]

-- | Draws a sprite on the display and finds if there is a collision
drawSprite :: VMState          -- ^ The VM state
           -> Word             -- ^ X coordinate to draw from
           -> Word             -- ^ Y coordinate to draw from
           -> Word             -- ^ The memory address of the sprite
           -> Word             -- ^ The byte length of the sprite in memory
           -> (Bool, VMState)  -- ^ The new state and if a collision occured
drawSprite s@VMState { display = display } x y addr n =
    (collision, s { display = display // display' })
  where
    -- Adds the x,y offset to the relative pixel co-ordinate
    addOffset (sx, sy) = (sx + x, sy + y)
    -- Checks if the pixel co-ordinate is within the display bounds
    inBounds (x, y) = (x >= 0 && x < 64) && (y >= 0 && y < 32)
    -- Gets the sprite's relative pixels, adds offsets and filters out of bounds
    sprite = filter inBounds $ map addOffset $ getSprite s addr n
    -- Checks if any sprite pixels collide with existing displayed pixels
    collision = any (\c -> not (boolXor (display ! c) True)) sprite
    -- Generates the list of changed array values for the display
    display' = map (\coord -> (coord, True)) sprite

-- | Performs a XOR bitwise operation on two boolean values
boolXor :: Bool -> Bool -> Bool
boolXor True True = False
boolXor False False = False
boolXor True False = True
boolXor False True = True