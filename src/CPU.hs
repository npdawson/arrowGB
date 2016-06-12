{-# LANGUAGE Arrows #-}
module CPU where

import FRP.Netwire

import Opcodes
import Types

cpu :: (HasTime t s, Monad m) => Wire s e m DataBus (AddrBus, DataBus, Control)
cpu = proc (DataBus byte) -> returnA -< (AddrBus 1, DataBus byte, ReadMem)

-- TODO write functions to handle different opcode types
-- ld :: Operand -> Operand -> ...
-- Operand = Register | Address8 | Address 16...

-- how to handle internal cpu state?
-- I want the registers to be in here
-- Should each pass through the arrow be 1 machine/clock cycle or 1 instruction
-- keep track of current instruction?
-- What Control bits do i need to pass out?
