{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}
module Mem (memory) where

import           Control.Monad.ST
import           Control.Wire
import qualified Data.ByteString             as B
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Word
import           Prelude                     hiding (id, (.))

import           Types

type Ram = forall s. ST s (M.MVector s Word8)

-- 8K of working ram (for non-color GB)
wram :: Ram
wram = M.replicate (0xFC00 - 0xC000) 0

-- 8K of video ram
vram :: Ram
vram = M.replicate (8 * 1024) 0

-- external ram (cartridge)
extram :: Ram
extram = M.replicate (8 * 1024) 0

-- sprite table
spriteTable :: Ram
spriteTable = M.replicate 0xA0 0

-- probably need to initialize this with specific data
ioRegisters :: Ram
ioRegisters = M.replicate 0x80 0

-- high ram + interrupts enable register (1 byte at end)
hram :: Ram
hram = M.replicate 0x80 0

memory :: B.ByteString -> Wire s () (ST s') (AddrBus, DataBus, Control) DataBus
memory rom = mkGen_ $ \(AddrBus a, DataBus byte, ctrl) -> do
  let addr = fromIntegral a
  wram' <- wram
  vram' <- vram
  cartram <- extram
  sprites <- spriteTable
  ioreg <- ioRegisters
  hram' <- hram
  case ctrl of
    ReadMem -> do
      out <- case addr of
        _ | addr `elem` [0x0..0x7FFF]    -> return $ B.index rom addr
          | addr `elem` [0x8000..0x9FFF] -> M.read vram' (addr - 0x8000)
          | addr `elem` [0xA000..0xBFFF] -> M.read cartram (addr - 0xA000)
          | addr `elem` [0xC000..0xFDFF] -> M.read wram' (addr - 0xC000)
          | addr `elem` [0xFE00..0xFE9F] -> M.read sprites (addr - 0xFE00)
          | addr `elem` [0xFEA0..0xFEFF] -> return 0 -- unusable, error?
          | addr `elem` [0xFF00..0xFF7F] -> M.read ioreg (addr - 0xFF00)
          | addr `elem` [0xFF80..0xFFFF] -> M.read hram' (addr - 0xFF80)
          | otherwise -> return 0
      return (Right (DataBus out))
    WriteMem -> do
      case addr of
        _ | addr `elem` [0x8000..0x9FFF] -> M.write vram' (addr - 0x8000) byte
          | addr `elem` [0xA000..0xBFFF] -> M.write cartram (addr - 0xA000) byte
            -- TODO handle mirror addresses in both ranges
          | addr `elem` [0xC000..0xDFFF] -> M.write wram' (addr - 0xC000) byte
          | addr `elem` [0xE000..0xFDFF] -> do -- ram mirror
              M.write wram' (addr - 0xC000) byte
              M.write wram' (addr - 0xF000) byte
          | addr `elem` [0xFE00..0xFE9F] -> M.write sprites (addr - 0xFE00) byte
          | addr `elem` [0xFF00..0xFF7F] -> M.write ioreg (addr - 0xFF00) byte
          | addr `elem` [0xFF80..0xFFFF] -> M.write hram' (addr - 0xFF80) byte
          | otherwise -> return ()
      return (Left ())

-- what value to inhibit with?

-- how to handle switching banks?

-- should load these up into variables for each START, LENGTH, END
-- memory map:
-- 0000-3FFF	16KB ROM bank 00	     cartridge, fixed bank
-- 4000-7FFF	16KB ROM Bank 01~NN	     cartridge, switchable bank via MBC
-- 8000-9FFF	8KB Video RAM (VRAM)	     Switchable bank 0/1 in CGB mode
-- A000-BFFF	8KB External RAM	     cartridge, switchable bank if any
-- C000-CFFF	4KB Work RAM (WRAM) bank 0
-- D000-DFFF	4KB Work RAM bank 1~N	     Switchable bank 1~7 in CGB mode
-- E000-FDFF	Mirror of C000~DDFF (ECHO)   Typically not used
-- FE00-FE9F	Sprite attribute table (OAM)
-- FEA0-FEFF	Not Usable
-- FF00-FF7F	I/O Registers
-- FF80-FFFE	High RAM (HRAM)
-- FFFF-FFFF	Interrupts Enable Register
