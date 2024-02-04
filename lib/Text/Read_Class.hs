-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
-- Temporary Read class
module Text.Read_Class(
  ReadS,
  Read(..),
  ) where
import Primitives

type String = [Char]  -- avoid importing Data.Char

type ReadS a = String -> [(a, String)]

class Read a where
  readsPrec    :: Int -> ReadS a
--  readList     :: ReadS [a]
