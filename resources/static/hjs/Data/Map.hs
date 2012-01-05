module Data.Map where

type Map k a = [(k, a)]

lookup :: Eq k => k -> Map k a -> Maybe a
lookup _ []            = Nothing
lookup k ((k', a):xs)  | k == k'    = Just a
                       | otherwise  = lookup k xs

insert :: k -> a -> Map k a -> Map k a
insert k a = (:) (k, a)

assocs :: Map k a -> [(k, a)]
assocs = id

empty :: Map k a
empty = []
