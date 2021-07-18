{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import           MCPrelude

fiveRands :: [Integer]
fiveRands = [fst a, fst b, fst c, fst d, fst e] where
  a = rand (mkSeed 1)
  b = rand (snd a)
  c = rand (snd b)
  d = rand (snd c)
  e = rand (snd d)

type Gen a = Seed -> (a, Seed)

randLetter :: Gen Char
randLetter seed = (ltr, newSeed) where
  a       = rand seed
  ltr     = toLetter . fst $ a
  newSeed = snd a

randString3 :: String
randString3 = [fst a, fst b, fst c]
 where
  a = randLetter (mkSeed 1)
  b = randLetter (snd a)
  c = randLetter (snd b)

randEven :: Gen Integer
-- randEven seed = (2 * fst a, snd a) where a = rand seed
randEven = generalA (2 *) rand

randOdd :: Gen Integer
-- randOdd seed = (fst a + 1, snd a) where a = randEven seed
randOdd = generalA (+ 1) randEven

randTen :: Gen Integer
-- randTen seed = (10 * fst a, snd a) where a = rand seed
randTen = generalA (10 *) rand

-- generalA :: (a -> b) -> (Seed -> (a, Seed)) -> (Seed -> (b, Seed))
generalA :: (a -> b) -> Gen a -> Gen b
generalA g f x = ((g . (fst . f)) x, (snd . f) x)


randPair :: Gen (Char, Integer)
randPair seed = ((ltr, int), newSeed) where
  a       = randLetter seed
  ltr     = fst a
  b       = rand (snd a)
  int     = fst b
  newSeed = snd b


generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair f g x = (((fst . f) x, fst (g (snd (f x)))), snd (g (snd (f x))))

randPair_ = generalPair randLetter rand


generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB cons f g x =
  (cons ((fst . f) x) (fst (g (snd (f x)))), snd (g (snd (f x))))


generalPair2 = generalB (,)


repRandom :: [Gen a] -> Gen [a]
repRandom [] seed = ([], seed)
repRandom (f : fs) seed =
  (fst (f seed) : fst (repRandom fs (snd (f seed))), snd (f seed))

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo f g seed = g (fst (f seed)) (snd (f seed))
-- genTwo f g seed = uncurry g (f seed)

mkGen :: a -> Gen a
mkGen x seed = (x, seed)
