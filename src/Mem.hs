{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Mem (memory) where

import Prelude hiding ((.), id)
import Control.Wire
import Control.Monad.ST
import Data.Word
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.ByteString as B

import Types

type Ram = forall s. ST s (M.MVector s Word8)

-- 8K of working ram (for non-color GB)
wram :: Ram
wram = M.replicate (8 * 1024) 0

-- 8K of video ram
vram :: Ram
vram = M.replicate (8 * 1024) 0

memory :: B.ByteString -> Wire s () (ST s') (AddrBus, DataBus, Control) DataBus
memory rom = mkGen_ $ \(AddrBus a, DataBus byte, ctrl) -> do
  let addr = fromIntegral a
  wram' <- wram
  vram' <- vram
  case ctrl of
    ReadMem -> do
      out <- case addr of
        _ | addr `elem` [0x0..0x7FFF]    -> return $ B.index rom addr
          | addr `elem` [0x8000..0x9FFF] -> M.read vram' addr -- TODO calc offset!
          | addr `elem` [0xC000..0xDFFF] -> M.read wram' addr -- TODO calc offset!
          | otherwise -> return 0 -- delete? theoretically unreachable once complete
      return (Right (DataBus out))
    WriteMem -> do
      M.write wram' addr byte
      return (Left ())

-- what value do I want to inhibit with?

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
