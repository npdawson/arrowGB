{-# LANGUAGE Arrows #-}
module CPU where

import FRP.Yampa

import Opcodes
import Types

cpu :: SF DataBus (AddrBus, DataBus, Control)
cpu = proc (DataBus byte) -> returnA -< (AddrBus 1, DataBus byte, ReadMem)

-- how to handle internal cpu state?
-- I want the registers to be in here
-- Should each pass through the arrow be 1 machine/clock cycle or 1 instruction
-- keep track of current instruction?
-- What Control bits do i need to pass out?
