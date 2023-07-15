-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.List_Type(module Data.List_Type) where
data List a = Nil | (:) a (List a)
