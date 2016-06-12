{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}
module Mem (memory) where

import           Control.Monad
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Wire                hiding (when)
import qualified Data.ByteString             as B
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed         as V
import           Data.Word
import           Prelude                     hiding (id, (.))

import           Types

type Ram = forall m. PrimMonad m => m (M.MVector (PrimState m) Word8)

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

memory :: PrimMonad m =>
          B.ByteString ->
          Wire s () m (AddrBus, DataBus, Control) DataBus
memory rom = mem' $ V.replicate 0xFFFF (0 :: Word8)
  where mem' :: PrimMonad m =>
                V.Vector Word8 ->
                Wire s () m (AddrBus, DataBus, Control) DataBus
        mem' ram = mkGenN $ \(AddrBus a, DataBus byte, ctrl) -> do
          let addr = fromIntegral a
          ram' <- V.thaw ram
          case ctrl of
            ReadMem -> do
              out <- case memmap addr of
                ROM x -> return $ B.index rom x
                _ -> M.read ram' addr
                -- CartRAM x -> M.read ram x
                -- WRAM x -> M.read ram' x
                -- Sprites x -> M.read sprites x
                -- IOReg x -> M.read ioreg x
                -- HiRAM x -> M.read hram' x
                -- None -> return 0 -- error?
              ram'' <- V.freeze ram'
              return (Right (DataBus out), mem' ram'')
            WriteMem -> do
              case memmap addr of -- TODO handle bank switching when writing rom
                ROM _ -> return () -- TODO
                _ -> do M.write ram' addr byte
                        if addr < 0xDE00 then
                          M.write ram' (addr + 0x2000) byte
                          else if addr >= 0xE000 then
                            M.write ram' (addr - 0x2000) byte
                            else return ()
                -- CartRAM x -> M.write cartram x byte
                -- WRAM x -> do
                --   M.write wram' x byte
                -- Sprites x -> M.write sprites x byte
                -- IOReg x -> M.write ioreg x byte
                -- HiRAM x -> M.write hram' x byte
                -- None -> return ()
              ram'' <- V.freeze ram'
              return (Left (), mem' ram'')

memmap :: Int -> Area
memmap addr
  | addr `elem` [0x0000..0x7FFF] = ROM addr
  | addr `elem` [0x8000..0x9FFF] = VRAM (addr - 0x8000)
  | addr `elem` [0xA000..0xBFFF] = CartRAM (addr - 0xA000)
  | addr `elem` [0xC000..0xFDFF] = WRAM (addr - 0xC000)
  | addr `elem` [0xFE00..0xFE9F] = Sprites (addr - 0xFE00)
  | addr `elem` [0xFF00..0xFF7F] = IOReg (addr - 0xFF00)
  | addr `elem` [0xFF80..0xFFFF] = HiRAM (addr - 0xFF00)
  | otherwise = None

data Area = ROM Int
          | WRAM Int
          | VRAM Int
          | CartRAM Int
          | Sprites Int
          | IOReg Int
          | HiRAM Int
          | None

-- what value to inhibit with?

-- how to handle switching banks?
-- depending on which MBC the "cartridge" uses, the banks are switched by
--    "writing" to specific address ranges on the cart

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
