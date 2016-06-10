module Lib (somefunc) where

import Data.Word (Word8)
import qualified Data.Map.Strict as M

-- Probably don't need the opcode in the record.
-- Try using a couple Word8 Maps using the opcode as key
data Instr = Instr {
  asm :: String, -- Assembly language representation
  bytes :: Word8, -- Length in memory
  cycles :: Word8 -- Number of cycles to run
  }

-- Instruction Map indexed by the opcode
type ByteMap = M.Map Word8 Instr

-- Normal instructions, separate from the extended set
--
-- d8, d16 are immediate data
-- a8 is unsigned data which is added to 0xFF00
-- a16 is a 16 bit address
-- r8 is signed data added to PC
--
-- flags are (Z)zero, (N)subtract, (H)half carry, and (C)carry
-- they are (-)unaffected, (z|n|h|c) dependently affected,
--          (1) set, or (0) cleared
opcodes :: ByteMap                                      -- affected flags
opcodes = M.fromList [(0x00, Instr "nop"          1  4) -- Z N H C
                     ,(0x01, Instr "ld BC,d16"    3 12)
                     ,(0x02, Instr "ld (BC),A"    1  8)
                     ,(0x03, Instr "inc BC"       1  8)
                     ,(0x04, Instr "inc B"        1  4) -- z 0 h -
                     ,(0x05, Instr "dec B"        1  4) -- z 1 h -
                     ,(0x06, Instr "ld B,d8"      2  8)
                     ,(0x07, Instr "rlca"         1  4) -- 0 0 0 c
                     ,(0x08, Instr "ld (a16),SP"  3 20)
                     ,(0x09, Instr "add HL,BC"    1  8) -- - 0 h c
                     ,(0x0A, Instr "ld A,(BC)"    1  8)
                     ,(0x0B, Instr "dec BC"       1  8)
                     ,(0x0C, Instr "inc C"        1  4) -- z 0 h -
                     ,(0x0D, Instr "dec C"        1  4) -- z 1 h -
                     ,(0x0E, Instr "ld C,d8"      2  8)
                     ,(0x0F, Instr "rrca"         1  4) -- 0 0 0 c
                     ,(0x10, Instr "stop 0"       2  4)
                     ,(0x11, Instr "ld DE,d16"    3 12)
                     ,(0x12, Instr "ld (DE),A"    1  8)
                     ,(0x13, Instr "inc DE"       1  8)
                     ,(0x14, Instr "inc D"        1  4) -- z 0 h -
                     ,(0x15, Instr "dec D"        1  4) -- z 1 h -
                     ,(0x16, Instr "ld D,d8"      2  8)
                     ,(0x17, Instr "rla"          1  4) -- 0 0 0 c
                     ,(0x18, Instr "jr r8"        2 12)
                     ,(0x19, Instr "add HL,DE"    1  8) -- - 0 h c
                     ,(0x1A, Instr "ld A,(DE)"    1  8)
                     ,(0x1B, Instr "dec DE"       1  8)
                     ,(0x1C, Instr "inc E"        1  4) -- z 0 h -
                     ,(0x1D, Instr "dec E"        1  4) -- z 1 h -
                     ,(0x1E, Instr "ld E,d8"      2  8)
                     ,(0x1F, Instr "rra"          1  4) -- 0 0 0 c
                     ,(0x20, Instr "jr nz,r8"     2  8) -- 8 or 12 cycles
                     ,(0x21, Instr "ld HL,d16"    3 12)
                     ,(0x22, Instr "ld (HL+),A"   1  8)
                     ,(0x23, Instr "inc HL"       1  8)
                     ,(0x24, Instr "inc H"        1  4) -- z 0 h -
                     ,(0x25, Instr "dec H"        1  4) -- z 1 h -
                     ,(0x26, Instr "ld H,d8"      2  8)
                     ,(0x27, Instr "daa"          1  4) -- z - 0 c
                     ,(0x28, Instr "jr z,r8"      2  8) -- 8 or 12 cycles
                     ,(0x29, Instr "add HL,HL"    1  8) -- - 0 h c
                     ,(0x2A, Instr "ld A,(HL+)"   1  8)
                     ,(0x2B, Instr "dec HL"       1  8)
                     ,(0x2C, Instr "inc L"        1  4) -- z 0 h -
                     ,(0x2D, Instr "dec L"        1  4) -- z 1 h -
                     ,(0x2E, Instr "ld L,d8"      2  8)
                     ,(0x2F, Instr "cpl"          1  4) -- - 1 1 -
                     ]

somefunc :: IO ()
somefunc = putStrLn "Hello, Game Boy!"
