module Display where
import Data.List(intercalate) 
import Text.Printf(printf)

class (Show a) => Display a where
    display :: a -> String

displayList :: Display a => [a] -> IO ()
displayList list = printf . (++ "\n") . intercalate "\n" . map (("-" ++) . display) $ list

