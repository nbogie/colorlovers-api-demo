{-# LANGUAGE  UndecidableInstances
            , FlexibleInstances
            , OverlappingInstances #-}
import System.Random
data Animal = Dog| Cat | Lion deriving (Show, Enum, Bounded)

{-
instance Random Animal where
  randomR (lo,hi) g = pickOne [lo .. hi] g
  random = randomR (minBound, maxBound)
-}

instance (Enum t, Bounded t) => Random t where
  randomR (lo,hi) g = pickOne [lo .. hi] g
  random = randomR (minBound, maxBound)

pickOne :: (RandomGen g) => [a] -> g -> (a, g)
pickOne xs gen = (xs !! i, g')
  where (i, g') = randomR (0, length xs - 1) gen
instance Random (Int,Int) where
  randomR ((x1,y1),(x2,y2)) g = let (i1, g') =  randomR (x1,x2) g
                                    (i2, g'') = randomR (y1,y2) g'
                                in ((i1,i2), g'')
  random = randomR (minBound, maxBound)
demo :: IO ()
demo = do
  gen <- getStdGen
  print $ take 5 $ (randomRs ((0,0), (10,20)) gen::[(Int,Int)])

