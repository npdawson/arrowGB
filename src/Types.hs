module Types where

import Control.Wire
import Data.Word

newtype DataBus = DataBus Word8
newtype AddrBus = AddrBus Word16

data Control = ReadMem
             | WriteMem
             -- | RWMem

data Area = ROM
          | VRAM
          | CartRAM
          | WRAM
          | Sprites
          | IOReg
          | HiRAM
          | None

type MemWire s e m = Wire s e m (AddrBus, DataBus, Control) DataBus
