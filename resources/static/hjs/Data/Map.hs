module Data.Map where
  
type Map k v = [(k,v)]

empty :: Map k v
empty = []

lookup :: Eq k => k -> Map k v -> Maybe v
lookup _ [] = Nothing
lookup k ((k', v):xs) | k == k'   = Just v
                      | otherwise = lookup k xs

insert :: k -> v -> Map k v -> Map k v
insert k v = (:) (k,v)

assocs = id

fromList = id