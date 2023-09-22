module Control.DeepSeq(module Control.DeepSeq) where
import Primitives --Yhiding(rnf)
import Prelude

rnf :: forall a . --YNFData a =>
                  a -> ()
rnf = primRnf

deepseq :: forall a b . --YNFData a =>
                        a -> b -> b
deepseq a b = rnf a `seq` b

force :: forall a . --YNFData a =>
                    a -> a
force x = rnf x `seq` x
