module Display where

class (Show a) => Display a where
    display :: a -> String

