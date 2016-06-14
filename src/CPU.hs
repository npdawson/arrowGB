module CPU where

import Control.Monad.ST
import Control.Wire
import qualified Data.ByteString as B
import Data.Map.Strict
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word

import Mem
import Opcodes
import Types

cpu :: (HasTime t s) => B.ByteString -> Wire s e m DataBus Int
cpu rom = cpu' m $ V.replicate 0x10C (0 :: Word8)
  where m = memory rom
        cpu' mem regs = mkSF $ \dt _ -> do
          let pcLo = regs V.! 0x109
          let pcHi = regs V.! 0x108
          let pc = fromIntegral pcHi * 0x100 + fromIntegral pcLo
          -- fetch
          op <- readMem dt mem pc
          opLo <- readMem dt mem (pc+1)
          opHi <- readMem dt mem (pc+2)
          -- decode
          let instr = opcodes ! fromIntegral op
          let operand = case bytes instr of
                       2 -> fromIntegral opLo
                       3 -> fromIntegral opHi * 0x100 + fromIntegral opLo
                       _ -> 0
          -- execute
          (regs', mem') <- execute op operand instr regs mem
          return (cycles instr, cpu' mem' regs')

execute :: Monad m =>
           Word8 -> Int ->
           Instr ->
           V.Vector Word8 ->
           MemWire s e m ->
           m (V.Vector Word8, MemWire s e m)
execute op operand instr regs mem = do
  return (regs, mem)



-- TODO write functions to handle different opcode types
-- ld :: Operand -> Operand -> ...
-- Operand = Register | Address8 | Address 16...

-- how to handle internal cpu state?
-- I want the registers to be in here
-- Should each pass through the arrow be 1 machine/clock cycle or 1 instruction
-- keep track of current instruction?
-- What Control bits do i need to pass out?
