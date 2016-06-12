{-# LANGUAGE Arrows #-}
module CPU where

import Control.Wire
import qualified Data.ByteString as B
import Data.Map.Strict
import Data.Word

import Mem
import Opcodes
import Types

cpu :: (HasTime t s, Monad m) => CPU -> B.ByteString -> Wire s e m DataBus ()
cpu cs rom = let mem = memory rom in mkSF $ \dt _ -> do
  let currPC = pc cs
  -- fetch
  (Right (DataBus op), mem) <- stepWire mem dt
                               (Right (AddrBus currPC, DataBus 0x00, ReadMem))
  (Right (DataBus opLo), mem) <- stepWire mem dt
                                   (Right (AddrBus (currPC + 1), DataBus 0x00,
                                            ReadMem))
  (Right (DataBus opHi), mem) <- stepWire mem dt
                                    (Right (AddrBus (currPC + 2), DataBus 0x00,
                                            ReadMem))
  -- decode
  let instr = opcodes ! op
  let operand = case bytes instr of
               1 -> 0 :: Word16
               2 -> opLo
               3 -> opHi * 0x100 + opLo
  -- execute
  cs' <- case op of
           0x00 -> return cs
           0x01 -> return CPU (a cs) (f cs)
  return $ cpu cs' mem

initCPU = CPU 0 0 0 0 0 0 0x100 0xFFFE

data CPU = CPU {
      a :: Word8
    , f :: Word8
    , b :: Word8
    , c :: Word8
    , d :: Word8
    , e :: Word8
    , pc :: Word16
    , sp :: Word16
    }

-- TODO write functions to handle different opcode types
-- ld :: Operand -> Operand -> ...
-- Operand = Register | Address8 | Address 16...

-- how to handle internal cpu state?
-- I want the registers to be in here
-- Should each pass through the arrow be 1 machine/clock cycle or 1 instruction
-- keep track of current instruction?
-- What Control bits do i need to pass out?
