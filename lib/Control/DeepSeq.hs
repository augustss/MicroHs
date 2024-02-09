module Control.DeepSeq(module Control.DeepSeq) where
import Primitives
import Prelude

rnf :: forall a . a -> ()
rnf = primRnfErr

deepseq :: forall a b . a -> b -> b
deepseq a b = rnf a `seq` b

force :: forall a . a -> a
force x = rnf x `seq` x

rnfNoErr :: forall a . a -> ()
rnfNoErr = primRnfNoErr
