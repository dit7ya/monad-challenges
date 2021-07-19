{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import           MCPrelude


data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  -- show :: (Maybe a) -> String
  show Nothing  = "Nothing"
  show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
  -- (==) :: Maybe a -> Maybe a -> Bool
  (==) (Just a) (Just b) = a == b
  (==) Nothing  Nothing  = True
  (==) (Just _) Nothing  = False
  (==) Nothing  (Just _) = False

headMay :: [a] -> Maybe a
headMay []       = Nothing
headMay (x : xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay []       = Nothing
tailMay (x : xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay el [] = Nothing
lookupMay el ((k, v) : xs) | k == el   = Just v
                           | otherwise = lookupMay el xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x : xs) | null xs   = Just x
                    | x > y     = Just x
                    | otherwise = Just y
  where (Just y) = maximumMay xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x : xs) | null xs   = Just x
                    | x < y     = Just x
                    | otherwise = Just y
  where (Just y) = maximumMay xs



queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s = case lookupMay s gd of
  Nothing   -> Nothing
  (Just xs) -> case tailMay xs of
    Nothing    -> Nothing
    (Just nxs) -> case maximumMay nxs of
      Nothing    -> Nothing
      (Just max) -> case divMay (fromIntegral max) (fromIntegral y) of
        Nothing     -> Nothing
        (Just quot) -> Just quot
        where (Just y) = headMay xs


chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing  = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd s = link (link (link y tailMay) maximumMay)
                        (\x -> divMay (fromIntegral x) (fromIntegral z))
 where
  y        = lookupMay s gd
  (Just z) = link y headMay


addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries sals p1 p2 = case lookupMay p1 sals of
  Nothing   -> Nothing
  (Just s1) -> case lookupMay p2 sals of
    Nothing   -> Nothing
    (Just s2) -> Just (s1 + s2)

mkMaybe :: a -> Maybe a
mkMaybe = Just

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb = link ma (\x -> link mb (mkMaybe . f x))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 sals p1 p2 = yLink (+) (lookupMay p1 sals) (lookupMay p2 sals)


tailProd :: Num a => [a] -> Maybe a
tailProd []       = Nothing
tailProd [x     ] = Just 1
tailProd (x : xs) = Just $ product xs


tailSum :: Num a => [a] -> Maybe a
tailSum []       = Nothing
tailSum [x     ] = Just 1
tailSum (x : xs) = Just $ sum xs

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f Nothing  = Nothing
transMaybe f (Just a) = Just (f a)

tailSum2 xs = transMaybe sum (tailMay xs)
tailProd2 xs = transMaybe product (tailMay xs)

tailMax xs = transMaybe maximumMay (tailMay xs)
tailMin xs = transMaybe minimumMay (tailMay xs)

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing         = Nothing
combine (Just Nothing ) = Nothing
combine (Just (Just a)) = Just a
