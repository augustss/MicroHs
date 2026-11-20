module Error107(main) where
import Error107A

instance Show T where show _ = "xxx"

main :: IO ()
main = print T
