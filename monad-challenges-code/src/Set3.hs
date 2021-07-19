{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import           MCPrelude




allPairs :: [a] -> [b] -> [(a, b)]
allPairs xs       []       = []
allPairs []       xs       = []
allPairs [x     ] (y : ys) = (x, y) : allPairs [x] ys
allPairs (x : xs) ys       = allPairs [x] ys ++ allPairs xs ys


data Card = Card Int String

instance Show Card where
  show (Card n s) = show n ++ s

allCards :: [Int] -> [String] -> [Card]
allCards _       []          = []
allCards []      _           = []
allCards [cRank] (s : suits) = Card cRank s : allCards [cRank] suits
allCards (cRank : cRanks) suits =
  allCards [cRank] suits ++ allCards cRanks suits

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f _        []       = []
allCombs f []       _        = []
allCombs f [x     ] (y : ys) = f x y : allCombs f [x] ys
allCombs f (x : xs) ys       = allCombs f [x] ys ++ allCombs f xs ys

allPairs2 = allCombs (,)
allCards2 = allCombs Card

-- FIXME THIS IS INCOMPLETE AND INCORRECT
allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f [] _  _  = []
allCombs3 f _  [] _  = []
allCombs3 f _  _  [] = []
-- allCombs3 f [x] [y] (z : zs) = f x y z : allCombs3 f [x] [y] zs
allCombs3 f [x] (y : ys) (z : zs) =
  allCombs3 f [x] ys (z : zs)
    ++ allCombs3 f [x] (y : ys) zs
    ++ allCombs3 f [x] ys       zs

allCombs3 f (x : xs) ys zs = allCombs3 f [x] ys zs ++ allCombs3 f xs ys zs

combStep :: [a -> b] -> [a] -> [b]
combStep []       xs = []
combStep (f : fs) xs = map f xs ++ combStep fs xs
-- combStep fs xs = foldr (\f -> (++) (map f xs)) [] fs

allCombs_ :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs_ f xs = combStep (combStep [f] xs)

allCombs3_ :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3_ f xs ys = combStep (combStep (combStep [f] xs) ys)
