{-# LANGUAGE PatternGuards #-}

module MultiSet
    ( MultiSet
    , singleton
    , delete
    , toList
    , fromList
    ) where

import qualified Data.Map.Strict as M

newtype MultiSet a = MultiSet (M.Map a Int)

instance Ord a => Semigroup (MultiSet a) where
    MultiSet xs <> MultiSet ys = MultiSet $ M.unionWith (+) xs ys

instance Ord a => Monoid (MultiSet a) where
    mempty = MultiSet mempty

singleton :: Ord a => a -> MultiSet a
singleton x = MultiSet $ M.singleton x 1

delete :: Ord a => a -> MultiSet a -> Maybe (MultiSet a)
delete x (MultiSet xs)
  | Just n <- M.lookup x xs =
      case n of 
        1 -> Just $ MultiSet $ M.delete x xs 
        _ -> Just $ MultiSet $ M.insert x (n-1) xs 
  | otherwise = Nothing

toList :: MultiSet a -> [a]
toList (MultiSet xs) =
    concat [ replicate n x | (x,n) <- M.toList xs ]

fromList :: Ord a => [a] -> MultiSet a
fromList = foldMap singleton
