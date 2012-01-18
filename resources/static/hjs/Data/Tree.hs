module Data.Tree where
  
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: Forest a   -- ^ zero or more child trees
    }
  deriving (Show, Read, Eq)
    
type Forest a = [Tree a]
    