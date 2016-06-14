{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}
module Mem ( memory
           , readMem) where

import           Control.Monad               (when)
import           Control.Monad.ST
import           Control.Wire                hiding (when)
import qualified Data.ByteString             as B
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Word
import           Prelude                     hiding (id, (.))

import           Types

memory :: B.ByteString ->
          MemWire s e m
memory rom = mem' $ V.replicate 0x8000 (0 :: Word8)
  where mem' ram = mkSFN $ \(AddrBus a, DataBus byte, ctrl) -> runST $ do
          let addr = fromIntegral a
          ram' <- V.thaw ram
          out <- case ctrl of
            ReadMem ->
              case memmap addr of
                ROM -> return $ B.index rom addr
                _ -> M.read ram' (addr - 0x8000)
            WriteMem -> do
              case memmap addr of
                ROM -> return () -- TODO handle bank switching when writing to rom
                _ -> do let x = addr - 0x8000
                        M.write ram' x byte
                        when (x < 0x5E00) $
                          M.write ram' (x + 0x2000) byte
                        when (x >= 0x6000) $
                          M.write ram' (x - 0x2000) byte
              return 0
          ram'' <- V.freeze ram'
          return (DataBus out, mem' ram'')

readMem :: Monad m =>
           s ->
           MemWire s e m ->
           Word16 ->
           m Word8
readMem dt mem index = do
  (Right (DataBus byte), _) <- stepWire mem dt (Right (AddrBus index,
                                                       DataBus 0,
                                                       ReadMem))
  return byte

memmap :: Int -> Area
memmap addr
  | addr `elem` [0x0000..0x7FFF] = ROM
  | addr `elem` [0x8000..0x9FFF] = VRAM
  | addr `elem` [0xA000..0xBFFF] = CartRAM
  | addr `elem` [0xC000..0xFDFF] = WRAM
  | addr `elem` [0xFE00..0xFE9F] = Sprites
  | addr `elem` [0xFF00..0xFF7F] = IOReg
  | addr `elem` [0xFF80..0xFFFF] = HiRAM
  | otherwise = None

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
