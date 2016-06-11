module Types where

import Data.Word

newtype DataBus = DataBus Word8
newtype AddrBus = AddrBus Word16

data Control = ReadMem
             | WriteMem
             -- | RWMem
