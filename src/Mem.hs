{-# LANGUAGE Arrows #-}
module Mem where

import Prelude hiding ((.), id)
import Control.Wire
import Control.Monad.ST
import Data.Word
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as B

import Types

ram :: ST s (M.MVector s Word8)
ram = M.replicate (64 * 1024) 0

memory :: B.ByteString -> Wire s e (ST s') (AddrBus, DataBus, Control) DataBus
memory rom = mkGen_ $ \(AddrBus a, DataBus byte, ctrl) -> do
  ram' <- ram
  let addr = fromIntegral a
  out <- case ctrl of
           ReadMem -> M.read ram' addr
           WriteMem -> do M.write ram' addr byte; return byte
  return (Right (DataBus out))

-- cartridge in a separate module?
-- maybe at least give it its own arrow

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
