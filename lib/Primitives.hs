-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Primitives(module Primitives) where

primIntAdd  = primitive "+"
primIntSub  = primitive "-"
primIntMul  = primitive "*"
primIntQuot = primitive "quot"
primIntRem  = primitive "rem"
primIntSubR = primitive "subtract"

primIntEQ   = primitive "=="
primIntNE   = primitive "/="
primIntLT   = primitive "<"
primIntLE   = primitive "<="
primIntGT   = primitive ">"
primIntGE   = primitive ">="

primCharEQ  = primitive "=="
primCharNE  = primitive "/="
primCharLT  = primitive "<"
primCharLE  = primitive "<="
primCharGT  = primitive ">"
primCharGE  = primitive ">="

primError  = primitive "error"

primFix    = primitive "Y"

primChr x = x
primOrd x = x

primUnsafeCoerce x = x

--data List a = Nil | (:) a (List a)

primBind          = primitive "IO.>>="
primThen          = primitive "IO.>>"
primReturn        = primitive "IO.return"
primHPutChar      = primitive "IO.putChar"
primHGetChar      = primitive "IO.getChar"
primOpenFile      = primitive "IO.open"
primIsNullHandle  = primitive "IO.isNullHandle"
primHSerialize    = primitive "IO.serialize"
primHDeserialize  = primitive "IO.deserialize"
primHClose        = primitive "IO.close"
primStdin         = primitive "IO.stdin"
primStdout        = primitive "IO.stdout"
primStderr        = primitive "IO.stderr"
primGetArgs       = primitive "IO.getArgs"
primPerformIO     = primitive "IO.performIO"
