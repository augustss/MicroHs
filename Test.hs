module Test(module Test) where
import Prelude
import MicroHs.Parse

main = do
  putStrLn "Start"
  let
    fn = "t.hs"
  file <- readFile fn
  putStrLn $ "File read: " ++ fn ++ "\n"
  putStrLn file
  let
    p = parseDie pTop fn file
  putStrLn (showEModule p)

showEModule :: EModule -> String
showEModule am =
  case am of
    EModule i es ds -> "module " ++ i

showExpr :: Expr -> String
showExpr ae =
  case ae of
    EVar v -> "(EVar " ++ v ++ ")"
    EApp _ _ -> "EApp"
    ELam _ _ -> "ELam"
    EInt i -> "(EInt " ++ showInt i ++ ")"
    EChar _ -> "EChar"
    EStr _ -> "EStr"
    ECase _ _ -> "ECase"
    ELet _ _ -> "ELet"
    ETuple _ -> "ETuple"
    EList _ -> "EList"
    EDo _ _ -> "EDo"
    EPrim _ -> "EPrim"
    ESectL _ _ -> "ESectL"
    ESectR _ _ -> "ESectR"
    EIf _ _ _ -> "EIf"
    ECompr _ _ -> "ECompr"
