module MhsEval (
    MhsContext,
    withMhsContext,
    run,
    compiledWithGhc
) where

compiledWithGhc :: Bool
compiledWithGhc = False

data MhsContext
type MhsCombCode = String

withMhsContext :: (MhsContext -> IO a) -> IO a
withMhsContext = err

run :: MhsContext -> MhsCombCode -> IO ()
run = err

err :: a
err = error "only supported with GHC"