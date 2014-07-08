module Chip8.Opcodes
    (
      runInstruction
    ) where

import Data.Word
import Data.Bits
import Data.Array.Unboxed
import qualified Data.Set as S
import System.Random
import Numeric (showHex)

import Chip8.State (VMState(..))

runInstruction :: VMState  -- ^ Initial CPU state
               -> Word16   -- ^ Full CPU instruction
               -> VMState  -- ^ Resulting CPU state
runInstruction s operands = op s operands
  where
    op = case operands .&. 0xF000 of
        0x0000 -> case operands .&. 0xF of
            0x0 -> op00E0
            0xE -> op00EE
            _ -> error $ showHex operands ""
        0x1000 -> op1NNN
        0x2000 -> op2NNN
        0x3000 -> op3XKK
        0x4000 -> op4XKK
        0x6000 -> op6XKK
        0x7000 -> op7XKK
        0x8000 -> case operands .&. 0xF of
            0x0 -> op8XY0
            0x1 -> op8XY1
            0x2 -> op8XY2
            0x3 -> op8XY3
            0x4 -> op8XY4
            0x5 -> op8XY5
            0x6 -> op8XY6
            0x7 -> op8XY7
            0xE -> op8XYE
            _ -> error $ showHex operands ""
        0x9000 -> op9XY0
        0xA000 -> opANNN
        0xB000 -> opBNNN
        0xC000 -> opCXKK
        0xD000 -> opDXYN
        0xE000 -> case operands .&. 0xFF of
            0x9E -> opEX9E
            0xA1 -> opEXA1
            _ -> error $ showHex operands ""
        0xF000 -> case operands .&. 0xFF of
            0x07 -> opFX07
            0x0A -> opFX0A
            0x15 -> opFX15
            0x18 -> opFX18
            0x1E -> opFX1E
            0x29 -> opFX29
            0x33 -> opFX33
            0x55 -> opFX55
            0x65 -> opFX65
            _ -> error $ showHex operands ""
        _ -> error $ showHex operands ""

-- | Get the NNN value from an instruction
iNNN :: Word16 -> Word16
iNNN = fromIntegral . (0xFFF .&.)

-- | Get the KK value from an instruction
iKK :: Word16 -> Word8
iKK = fromIntegral . (0xFF .&.)

-- | Get the N value from an instruction
iN :: Word16 -> Word8
iN = fromIntegral . (0xF .&.)

-- | Get the X value from an instruction
iX :: Word16 -> Word8
iX = fromIntegral . flip shiftR 8 . (0xF00 .&.)

-- | Get the Y value from an instruction
iY :: Word16 -> Word8
iY = fromIntegral . flip shiftR 4 . (0xF0 .&.)

-- | Clear the display
op00E0 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op00E0 s _ =
    s { display = listArray ((0,0),(63,31)) (repeat False) }

-- | Return from subroutine
op00EE :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op00EE s@VMState { stack = stack } _ =
    s { pc = pc', stack = stack' }
  where
    (pc' : stack') = stack

-- | Jump to location NNN
op1NNN :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op1NNN s op =
    s { pc = iNNN op }

-- | Call subroutine at location NNN
op2NNN :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op2NNN s@VMState { pc = pc, stack = stack } op =
    s { pc = iNNN op, stack = pc : stack }

-- | Skip next instruction if VX == KK
op3XKK :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op3XKK s@VMState { pc = pc, v = v } op
  | vx == iKK op = s { pc = pc + 2 }
  | otherwise = s
  where
    vx = v ! iX op

-- | Skip next instruction if VX != KK
op4XKK :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op4XKK s@VMState { pc = pc, v = v } op
  | vx /= iKK op = s { pc = pc + 2 }
  | otherwise = s
  where
    vx = v ! iX op

-- | Skip next instruction if VX == VY
op5XY0 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op5XY0 s@VMState { pc = pc, v = v } op
  | vx == vy = s { pc = pc + 2 }
  | otherwise = s
  where
    vx = v ! iX op
    vy = v ! iY op

-- | Set VX = KK
op6XKK :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op6XKK s@VMState { v = v } op =
    s { v = v // [(iX op, iKK op)] }

-- | Set VX = VX + KK
op7XKK :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op7XKK s@VMState { v = v } op =
    s { v = v // [(x, vx + iKK op)] }
  where
    x = iX op
    vx = v ! x

-- | Set VX to VY
op8XY0 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op8XY0 s@VMState { v = v } op =
    s { v = v // [(iX op, v ! iY op)] }

-- | Set VX to VX OR VY
op8XY1 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op8XY1 s@VMState { v = v } op =
    s { v = v // [(x, vx .|. vy)] }
  where
    x = iX op
    vx = v ! x
    vy = v ! iY op

-- | Set VX to VX AND VY
op8XY2 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op8XY2 s@VMState { v = v } op =
    s { v = v // [(x, vx .&. vy)] }
  where
    x = iX op
    vx = v ! x
    vy = v ! iY op

-- | Set VX to VX XOR XY
op8XY3 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op8XY3 s@VMState { v = v } op =
    s { v = v // [(x, vx `xor` vy)] }
  where
    x = iX op
    vx = v ! x
    vy = v ! iY op

-- | Add VY to VX. Set VF to 1 when there is a carry, or 0 if not
op8XY4 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op8XY4 s@VMState { v = v } op =
    s { v = v // [(x, result .&. 0xFF), (0xF, carry)] }
  where
    x = iX op
    vx = v ! x
    vy = v ! iY op
    result = vx + vy
    carry = boolToFlag $ result > 255

-- | Set VX to VX - VY. Set VF to 0 when there is a borrow, or 1 if not
op8XY5 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op8XY5 s@VMState { v = v } op =
    s { v = v // [(x, result), (0xF, borrow)] }
  where
    x = iX op
    vx = v ! x
    vy = v ! iY op
    result = vx - vy
    borrow = boolToFlag $ vx > vy

-- | Shift VX right by 1.
--   VF is set to the least significant bit before the shift
op8XY6 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op8XY6 s@VMState { v = v } op =
    s { v = v // [(x, result), (0xF, leastSignificant)] }
  where
    x = iX op
    vx = v ! x
    leastSignificant = vx .&. 0x1
    result = shiftR vx 1

-- | Set VX to VY - VX. Set VF to 0 when there is a borrow, or 1 if not
op8XY7 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op8XY7 s@VMState { v = v } op =
    s { v = v // [(x, result), (0xF, borrow)] }
  where
    x = iX op
    vx = v ! x
    vy = v ! iY op
    result = vy - vx
    borrow = boolToFlag $ vy > vx

-- | Shift VX left by 1
--   VF is set to the most significant bit before the shift
op8XYE :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op8XYE s@VMState { v = v } op =
    s { v = v // [(x, result), (0xF, mostSignificant)] }
  where
    x = iX op
    vx = v ! x
    mostSignificant = shiftR vx 7
    result = shiftL vx 1 .&. 0xFF

-- | Skip next instruction if VX != VY
op9XY0 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
op9XY0 s@VMState { pc = pc, v = v } op
    | vx /= vy = s { pc = pc + 2 }
    | otherwise = s
  where
    vx = v ! iX op
    vy = v ! iY op

-- | Set i = NNN
opANNN :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opANNN s op =
    s { i = iNNN op }

-- | Jump to location NNN + V0
opBNNN :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opBNNN s@VMState { pc = pc, v = v } op =
    s { pc = iNNN op + v0 }
  where
    v0 = fromIntegral $ v ! 0x0

-- | Set VX = (random byte) & KK
opCXKK :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opCXKK s@VMState { v = v, randGen = randGen } op =
    s { v = v', randGen = randGen' }
  where
    (r, randGen') = randomR (0x0, 0xFF) randGen
    v' = v // [(iX op, r .&. iKK op)]

-- | Paint N byte long sprite starting at location I at (VX, VY)
--   Also set VF = collision
opDXYN :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opDXYN s@VMState { v = v, i = i } op =
    s' { v = v // [(0xF, collision')] }
  where
    vx = v ! iX op
    vy = v ! iY op
    (collision, s') = drawSprite s vx vy i (iN op)
    collision' = boolToFlag collision

-- | Skip next instruction if key VX is pressed
opEX9E :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opEX9E s@VMState { pc = pc, v = v, pressed = pressed } op
  | pressed' = s { pc = pc + 2 }
  | otherwise = s
  where
    vx = v ! iX op
    pressed' = S.member (fromIntegral vx) pressed

-- | Skip next instruction if key VX is up
opEXA1 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opEXA1 s@VMState { pc = pc, v = v, pressed = pressed } op
  | up = s { pc = pc + 2 }
  | otherwise = s
  where
    vx = v ! iX op
    up = S.notMember (fromIntegral vx) pressed

-- | Set VX to the value of the delay timer
opFX07 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX07 s@VMState { v = v, delayTimer = delayTimer } op =
    s { v = v // [(iX op, fromIntegral delayTimer)] }

-- | Wait for keypress then store in VX
opFX0A :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX0A s@VMState { v = v } op =
    s { waitForKeypress = Just (iX op) }

-- | Set delay timer to VX
opFX15 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX15 s@VMState { v = v } op =
    s { delayTimer = fromIntegral $ v ! iX op }

-- | Set sound timer to VX
opFX18 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX18 s op = s

-- | Set op = I + VX
opFX1E :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX1E s@VMState { v = v, i = i } op =
    s { i = i + fromIntegral  (v ! iX op) }

-- | Set I to the location of the sprite for the character in VX
opFX29 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX29 s@VMState { v = v } op =
    s { i = fromIntegral (v ! iX op) * 5 } -- Char sprites are 5 bytes long and start at 0

-- | Store BCD of VX at I, I+1 and I+2
opFX33 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX33 s@VMState { v = v, i = i, memory = memory } op =
    s { memory = memory // bcd }
  where
    vx = v ! iX op
    ones = vx `mod` 10
    tens = (vx `div` 10) `mod` 10
    hundreds = vx `div` 100
    bcd = [(i, hundreds), (i + 1, tens), (i + 2, ones)]

-- | Store V0 to VX in memory starting at address I
opFX55 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX55 s@VMState { pc = pc, v = v, i = i, memory = memory } op =
    s { memory = memory // memory' }
  where
    x = fromIntegral $ iX op
    memory' = map (\n -> (i + fromIntegral n, v ! n)) (range (0, x))

-- | Store memory in V0 to VX starting from I
opFX65 :: VMState  -- ^ Initial CPU state
       -> Word16   -- ^ Full CPU instruction
       -> VMState  -- ^ Resulting CPU state
opFX65 s@VMState { v = v, i = i, memory = memory } op =
    s { v = v // v' }
  where
    x = iX op
    v' = map (\ n -> (n, memory ! (i + fromIntegral n))) (range (0, x))

-- | Gets a sprite from a memory location and returns it's pixel coordinates
getSprite :: VMState           -- ^ The VM state
          -> Word16            -- ^ The memory address of the sprite
          -> Word8             -- ^ The byte length of the sprite in memory
          -> [(Word8, Word8)]  -- ^ Pixel coordinates representing the sprite
getSprite s@VMState { memory = memory } addr n =
    [(fromIntegral x, fromIntegral y)
        | y <- range (0, n - 1)
        , x <- [0,1..7]
        , let shift = 7 - x -- This prevents the sprite from being flipped
        , shiftR (memory ! (addr + fromIntegral y)) shift .&. 1 == 1]

-- | Draws a sprite on the display and finds if there is a collision
drawSprite :: VMState          -- ^ The VM state
           -> Word8            -- ^ X coordinate to draw from
           -> Word8            -- ^ Y coordinate to draw from
           -> Word16           -- ^ The memory address of the sprite
           -> Word8            -- ^ The byte length of the sprite in memory
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
    -- Folding func to turn sprite in to updated pixels and flag on collision
    folder (coll, display') coord
      | coll = (coll, (coord, pixel):display')
      | not coll = (not pixel, (coord, pixel):display')
      where
        pixel = boolXor (display ! coord) True
    -- Actually fold over the sprite coordinates
    (collision, display') = foldl folder (False, []) sprite

-- | Converts a boolean value to a 1 or 0 Word8 flag
boolToFlag :: Bool -> Word8
boolToFlag True = 1
boolToFlag False = 0

-- | Performs a XOR bitwise operation on two boolean values
boolXor :: Bool -> Bool -> Bool
boolXor True True = False
boolXor False False = False
boolXor True False = True
boolXor False True = True