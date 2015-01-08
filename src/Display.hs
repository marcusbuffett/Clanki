module Display where
import Data.List(intercalate) 
import Text.Printf(printf)

class (Show a) => Display a where
    display :: a -> String

displayList list = do
    printf . (++ "\n") . intercalate "\n" . map (("-" ++) . display) $ list

