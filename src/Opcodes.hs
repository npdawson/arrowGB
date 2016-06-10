module Opcodes where

import qualified Data.Map.Strict as M
import Data.Word (Word8)

data Instr = Instr {
  asm :: String, -- Disassembly
  bytes :: Word8, -- Length in memory
  cycles :: Word8 -- Number of cycles to run
  }

-- Instruction Map indexed by the opcode
type ByteMap = M.Map Word8 Instr

-- Normal instructions, separate from the extended set
-- http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
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
                     ,(0x20, Instr "jr nz,r8"     2  8) -- 12 cycles if taken
                     ,(0x21, Instr "ld HL,d16"    3 12)
                     ,(0x22, Instr "ld (HL+),A"   1  8)
                     ,(0x23, Instr "inc HL"       1  8)
                     ,(0x24, Instr "inc H"        1  4) -- z 0 h -
                     ,(0x25, Instr "dec H"        1  4) -- z 1 h -
                     ,(0x26, Instr "ld H,d8"      2  8)
                     ,(0x27, Instr "daa"          1  4) -- z - 0 c
                     ,(0x28, Instr "jr z,r8"      2  8) -- 12 cycles if taken
                     ,(0x29, Instr "add HL,HL"    1  8) -- - 0 h c
                     ,(0x2A, Instr "ld A,(HL+)"   1  8)
                     ,(0x2B, Instr "dec HL"       1  8)
                     ,(0x2C, Instr "inc L"        1  4) -- z 0 h -
                     ,(0x2D, Instr "dec L"        1  4) -- z 1 h -
                     ,(0x2E, Instr "ld L,d8"      2  8)
                     ,(0x2F, Instr "cpl"          1  4) -- - 1 1 -
                     ,(0x30, Instr "jr nc,r8"     2  8) -- 12 cycles if taken
                     ,(0x31, Instr "ld SP,d16"    3 12)
                     ,(0x32, Instr "ld (HL-),A"   1  8)
                     ,(0x33, Instr "inc SP"       1  8)
                     ,(0x34, Instr "inc (HL)"     1 12) -- z 0 h -
                     ,(0x35, Instr "dec (HL)"     1 12) -- z 1 h -
                     ,(0x36, Instr "ld (HL),d8"   2 12)
                     ,(0x37, Instr "scf"          1  4) -- - 0 0 1
                     ,(0x38, Instr "jr c,r8"      2  8) -- 12 cycles if taken
                     ,(0x39, Instr "add HL,SP"    1  8) -- - 0 h c
                     ,(0x3A, Instr "ld A,(HL-)"   1  8)
                     ,(0x3B, Instr "dec SP"       2  8)
                     ,(0x3C, Instr "inc A"        1  4) -- z 0 h -
                     ,(0x3D, Instr "dec A"        1  4) -- z 1 h -
                     ,(0x3E, Instr "ld A,d8"      2  8)
                     ,(0x3F, Instr "ccf"          1  4) -- - 0 0 c
                     ,(0x40, Instr "ld B,B"       1  4)
                     ,(0x41, Instr "ld B,C"       1  4)
                     ,(0x42, Instr "ld B,D"       1  4)
                     ,(0x43, Instr "ld B,E"       1  4)
                     ,(0x44, Instr "ld B,H"       1  4)
                     ,(0x45, Instr "ld B,L"       1  4)
                     ,(0x46, Instr "ld B,(HL)"    1  8)
                     ,(0x47, Instr "ld B,A"       1  4)
                     ,(0x48, Instr "ld C,B"       1  4)
                     ,(0x49, Instr "ld C,C"       1  4)
                     ,(0x4A, Instr "ld C,D"       1  4)
                     ,(0x4B, Instr "ld C,E"       1  4)
                     ,(0x4C, Instr "ld C,H"       1  4)
                     ,(0x4D, Instr "ld C,L"       1  4)
                     ,(0x4E, Instr "ld C,(HL)"    1  8)
                     ,(0x4F, Instr "ld C,A"       1  4)
                     ,(0x50, Instr "ld D,B"       1  4)
                     ,(0x51, Instr "ld D,C"       1  4)
                     ,(0x52, Instr "ld D,D"       1  4)
                     ,(0x53, Instr "ld D,E"       1  4)
                     ,(0x54, Instr "ld D,H"       1  4)
                     ,(0x55, Instr "ld D,L"       1  4)
                     ,(0x56, Instr "ld D,(HL)"    1  8)
                     ,(0x57, Instr "ld D,A"       1  4)
                     ,(0x58, Instr "ld E,B"       1  4)
                     ,(0x59, Instr "ld E,C"       1  4)
                     ,(0x5A, Instr "ld E,D"       1  4)
                     ,(0x5B, Instr "ld E,E"       1  4)
                     ,(0x5C, Instr "ld E,H"       1  4)
                     ,(0x5D, Instr "ld E,L"       1  4)
                     ,(0x5E, Instr "ld E,(HL)"    1  8)
                     ,(0x5F, Instr "ld E,A"       1  4)
                     ,(0x60, Instr "ld H,B"       1  4)
                     ,(0x61, Instr "ld H,C"       1  4)
                     ,(0x62, Instr "ld H,D"       1  4)
                     ,(0x63, Instr "ld H,E"       1  4)
                     ,(0x64, Instr "ld H,H"       1  4)
                     ,(0x65, Instr "ld H,L"       1  4)
                     ,(0x66, Instr "ld H,(HL)"    1  8)
                     ,(0x67, Instr "ld H,A"       1  4)
                     ,(0x68, Instr "ld L,B"       1  4)
                     ,(0x69, Instr "ld L,C"       1  4)
                     ,(0x6A, Instr "ld L,D"       1  4)
                     ,(0x6B, Instr "ld L,E"       1  4)
                     ,(0x6C, Instr "ld L,H"       1  4)
                     ,(0x6D, Instr "ld L,L"       1  4)
                     ,(0x6E, Instr "ld L,(HL)"    1  8)
                     ,(0x6F, Instr "ld L,A"       1  4)
                     ,(0x70, Instr "ld (HL),B"    1  8)
                     ,(0x71, Instr "ld (HL),C"    1  8)
                     ,(0x72, Instr "ld (HL),D"    1  8)
                     ,(0x73, Instr "ld (HL),E"    1  8)
                     ,(0x74, Instr "ld (HL),H"    1  8)
                     ,(0x75, Instr "ld (HL),L"    1  8)
                     ,(0x76, Instr "halt"         1  4)
                     ,(0x77, Instr "ld (HL),A"    1  8)
                     ,(0x78, Instr "ld A,B"       1  4)
                     ,(0x79, Instr "ld A,C"       1  4)
                     ,(0x7A, Instr "ld A,D"       1  4)
                     ,(0x7B, Instr "ld A,E"       1  4)
                     ,(0x7C, Instr "ld A,H"       1  4)
                     ,(0x7D, Instr "ld A,L"       1  4)
                     ,(0x7E, Instr "ld A,(HL)"    1  8)
                     ,(0x7F, Instr "ld A,A"       1  4)
                     ,(0x80, Instr "add A,B"      1  4) -- z 0 h c
                     ,(0x81, Instr "add A,C"      1  4) -- z 0 h c
                     ,(0x82, Instr "add A,D"      1  4) -- z 0 h c
                     ,(0x83, Instr "add A,E"      1  4) -- z 0 h c
                     ,(0x84, Instr "add A,H"      1  4) -- z 0 h c
                     ,(0x85, Instr "add A,L"      1  4) -- z 0 h c
                     ,(0x86, Instr "add A,(HL)"   1  8) -- z 0 h c
                     ,(0x87, Instr "add A,A"      1  4) -- z 0 h c
                     ,(0x88, Instr "adc A,B"      1  4) -- z 0 h c
                     ,(0x89, Instr "adc A,C"      1  4) -- z 0 h c
                     ,(0x8A, Instr "adc A,D"      1  4) -- z 0 h c
                     ,(0x8B, Instr "adc A,E"      1  4) -- z 0 h c
                     ,(0x8C, Instr "adc A,H"      1  4) -- z 0 h c
                     ,(0x8D, Instr "adc A,L"      1  4) -- z 0 h c
                     ,(0x8E, Instr "adc A,(HL)"   1  8) -- z 0 h c
                     ,(0x8F, Instr "adc A,A"      1  4) -- z 0 h c
                     ,(0x90, Instr "sub B"        1  4) -- z 1 h c
                     ,(0x91, Instr "sub C"        1  4) -- z 1 h c
                     ,(0x92, Instr "sub D"        1  4) -- z 1 h c
                     ,(0x93, Instr "sub E"        1  4) -- z 1 h c
                     ,(0x94, Instr "sub H"        1  4) -- z 1 h c
                     ,(0x95, Instr "sub L"        1  4) -- z 1 h c
                     ,(0x96, Instr "sub (HL)"     1  8) -- z 1 h c
                     ,(0x97, Instr "sub A"        1  4) -- z 1 h c
                     ,(0x98, Instr "sbc A,B"      1  4) -- z 1 h c
                     ,(0x99, Instr "sbc A,C"      1  4) -- z 1 h c
                     ,(0x9A, Instr "sbc A,D"      1  4) -- z 1 h c
                     ,(0x9B, Instr "sbc A,E"      1  4) -- z 1 h c
                     ,(0x9C, Instr "sbc A,H"      1  4) -- z 1 h c
                     ,(0x9D, Instr "sbc A,L"      1  4) -- z 1 h c
                     ,(0x9E, Instr "sbc A,(HL)"   1  8) -- z 1 h c
                     ,(0x9F, Instr "sbc A,A"      1  4) -- z 1 h c
                     ,(0xA0, Instr "and B"        1  4) -- z 0 1 0
                     ,(0xA1, Instr "and C"        1  4) -- z 0 1 0
                     ,(0xA2, Instr "and D"        1  4) -- z 0 1 0
                     ,(0xA3, Instr "and E"        1  4) -- z 0 1 0
                     ,(0xA4, Instr "and H"        1  4) -- z 0 1 0
                     ,(0xA5, Instr "and L"        1  4) -- z 0 1 0
                     ,(0xA6, Instr "and (HL)"     1  8) -- z 0 1 0
                     ,(0xA7, Instr "and A"        1  4) -- z 0 1 0
                     ,(0xA8, Instr "xor B"        1  4) -- z 0 0 0
                     ,(0xA9, Instr "xor C"        1  4) -- z 0 0 0
                     ,(0xAA, Instr "xor D"        1  4) -- z 0 0 0
                     ,(0xAB, Instr "xor E"        1  4) -- z 0 0 0
                     ,(0xAC, Instr "xor H"        1  4) -- z 0 0 0
                     ,(0xAD, Instr "xor L"        1  4) -- z 0 0 0
                     ,(0xAE, Instr "xor (HL)"     1  8) -- z 0 0 0
                     ,(0xAF, Instr "xor A"        1  4) -- z 0 0 0
                     ,(0xB0, Instr "or B"         1  4) -- z 0 0 0
                     ,(0xB1, Instr "or C"         1  4) -- z 0 0 0
                     ,(0xB2, Instr "or D"         1  4) -- z 0 0 0
                     ,(0xB3, Instr "or E"         1  4) -- z 0 0 0
                     ,(0xB4, Instr "or H"         1  4) -- z 0 0 0
                     ,(0xB5, Instr "or L"         1  4) -- z 0 0 0
                     ,(0xB6, Instr "or (HL)"      1  8) -- z 0 0 0
                     ,(0xB7, Instr "or A"         1  4) -- z 0 0 0
                     ,(0xB8, Instr "cp B"         1  4) -- z 1 h c
                     ,(0xB9, Instr "cp C"         1  4) -- z 1 h c
                     ,(0xBA, Instr "cp D"         1  4) -- z 1 h c
                     ,(0xBB, Instr "cp E"         1  4) -- z 1 h c
                     ,(0xBC, Instr "cp H"         1  4) -- z 1 h c
                     ,(0xBD, Instr "cp L"         1  4) -- z 1 h c
                     ,(0xBE, Instr "cp (HL)"      1  8) -- z 1 h c
                     ,(0xBF, Instr "cp A"         1  4) -- z 1 h c
                     ,(0xC0, Instr "ret nz"       1  8) -- 20 cycles if taken
                     ,(0xC1, Instr "pop BC"       1 12)
                     ,(0xC2, Instr "jp nz,a16"    3 12) -- 16 cycles if taken
                     ,(0xC3, Instr "jp a16"       3 16)
                     ,(0xC4, Instr "call nz,a16"  3 12) -- 24 cycles if taken
                     ,(0xC5, Instr "push BC"      1 16)
                     ,(0xC6, Instr "add A,d8"     2  8) -- z 0 h c
                     ,(0xC7, Instr "rst 00H"      1 16)
                     ,(0xC8, Instr "ret z"        1  8) -- 20 cycles if taken
                     ,(0xC9, Instr "ret"          1 16)
                     ,(0xCA, Instr "jp z,a16"     3 12) -- 16 cycles if taken
                     ,(0xCB, Instr "prefix 0xCB"  1  4)
                     ,(0xCC, Instr "call z,a16"   3 12) -- 24 cycles if taken
                     ,(0xCD, Instr "call a16"     3 24)
                     ,(0xCE, Instr "adc A,d8"     2  8) -- z 0 h c
                     ,(0xCF, Instr "rst 08H"      1 16)
                     ,(0xD0, Instr "ret nc"       1  8) -- 20 cycles if taken
                     ,(0xD1, Instr "pop DE"       1 12)
                     ,(0xD2, Instr "jp nc,a16"    3 12) -- 16 cycles if taken
                     -- no 0xD3
                     ,(0xD4, Instr "call nc,a16"  3 12) -- 24 cycles if taken
                     ,(0xD5, Instr "push DE"      1 16)
                     ,(0xD6, Instr "sub d8"       2  8) -- z 1 h c
                     ,(0xD7, Instr "rst 10H"      1 16)
                     ,(0xD8, Instr "ret c"        1  8) -- 20 cycles if taken
                     ,(0xD9, Instr "reti"         1 16)
                     ,(0xDA, Instr "jp c,a16"     3 12) -- 16 cycles if taken
                     -- no 0xDB
                     ,(0xDC, Instr "call c,a16"   3 12) -- 24 cycles if taken
                     -- no 0xDD
                     ,(0xDE, Instr "sbc A,d8"     2  8) -- z 1 h c
                     ,(0xDF, Instr "rst 18H"      1 16)
                     ,(0xE0, Instr "ldh (a8),A"   2 12)
                     ,(0xE1, Instr "pop HL"       1 12)
                     ,(0xE2, Instr "ld (C),A"     2  8)
                     -- no 0xE3
                     -- no 0xE4
                     ,(0xE5, Instr "push HL"      1 16)
                     ,(0xE6, Instr "and d8"       2  8) -- z 0 1 0
                     ,(0xE7, Instr "rst 20H"      1 16)
                     ,(0xE8, Instr "add SP,r8"    2 16) -- 0 0 h c
                     ,(0xE9, Instr "jp (HL)"      1  4)
                     ,(0xEA, Instr "ld (a16),A"   3 16)
                     -- no 0xEB
                     -- no 0xEC
                     -- no 0xED
                     ,(0xEE, Instr "xor d8"       2  8) -- z 0 0 0
                     ,(0xEF, Instr "rst 28H"      1 16)
                     ,(0xF0, Instr "ldh A,(a8)"   2 12)
                     ,(0xF1, Instr "pop AF"       1 12) -- z n h c
                     ,(0xF2, Instr "ld A,(C)"     2  8)
                     ,(0xF3, Instr "di"           1  4)
                     -- no 0xF4
                     ,(0xF5, Instr "push AF"      1 16)
                     ,(0xF6, Instr "or d8"        2  8) -- z 0 0 0
                     ,(0xF7, Instr "rst 30H"      1 16)
                     ,(0xF8, Instr "ld HL,SP+r8"  2 12) -- 0 0 h c
                     ,(0xF9, Instr "ld SP,HL"     1  8)
                     ,(0xFA, Instr "ld A,(a16)"   3 16)
                     ,(0xFB, Instr "ei"           1  4)
                     -- no 0xFC
                     -- no 0xFD
                     ,(0xFE, Instr "cp d8"        2  8) -- z 1 h c
                     ,(0xFF, Instr "rst 38H"      1 16)
                     ]

-- 2nd set of opcodes prefixed with 0xCB
prefixOps :: ByteMap
prefixOps = M.fromList [(0x00, Instr "rlc B"   2 8) -- z 0 0 c
                       ,(0x11, Instr "rl C"    2 8) -- z 0 0 c
                       ,(0x4F, Instr "bit 1,A" 2 8) -- z 0 1 -
                       ,(0x7C, Instr "bit 7,H" 2 8) -- z 0 1 -
                     ]
