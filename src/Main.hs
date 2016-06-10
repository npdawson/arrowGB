module Main where

import Data.Word

main :: IO ()
main = putStrLn "Hello, world!"

-- Probably don't need the opcode in the record.
-- Try using a couple Word8 Maps using the opcode as key
data Instr = Instr {
  opcode :: Word8, -- Single byte opcode
  asm :: String, -- Assembly language representation
  bytes :: Word8, -- Length in memory
  cycles :: Word8 -- Number of cycles to run
  }
