{-# LANGUAGE FlexibleContexts #-}

module Powers where

data (Show v) => Levels v
  = Level [Levels v]
  | Base [v]

instance (Show v) => Show (Levels v) where
  --show (Level []) = "end. \n"
  show (Level list) = "  L:\n" ++
                      show list
                      ++ "  end.\n"
  show (Base list) = "B:" ++ show list

t1 = Base [1,2,3]
t2 = Level [t1]
t3 = Level [t2,t1]
t4 = Level [t3,t2,t1]

powerLevel (Level list) = Level $ map Level $ powerList list
powerLevel (Base list) = Level $ map Base $ powerList list

p1 = powerLevel $ powerLevel t1

powerPower 0 l = l
powerPower n l = powerLevel $ powerPower (n-1) l


-----------------------------------------------------------------------------------------------------------

data Hierarch v
  = Coll [Hierarch v]
  | Atom v
  deriving Eq

instance Show v => Show (Hierarch v) where
  --show (Level []) = "end. \n"
  show (Coll list) = "C:\n" ++
                      show list ++
                      "end.\n"
  show (Atom v) = "A:" ++ show v

h0 = Atom 1
h00 = Atom 2
h1 = Atom [1]
h2 = Atom [1,2]
h3 = Coll [h1, h2]
h4 = Coll [h00, h0]

powerHierarch :: Hierarch v -> Hierarch v
powerHierarch (Coll list) = Coll $ map Coll $  powerList list
powerHierarch (Atom v) = error " !!! ========>  powers of Elements doesn't exist !!! "

multiPowerHierarch :: Integer -> Hierarch v -> Hierarch v
multiPowerHierarch 0 h = h
multiPowerHierarch n h = powerHierarch $ multiPowerHierarch (n-1) h

permuteHierarch :: Integer -> Hierarch v -> Hierarch v
permuteHierarch _ (Atom _) = error " !!! ========>  permutations are done for sets !!! "
permuteHierarch 0 _           = Coll [ Coll [] ]
permuteHierarch n (Coll list) = Coll [ Coll $ e:r | e <- list, (Coll r) <- rest]
  where 
    (Coll rest) = permuteHierarch (n-1) (Coll list)

multiPermuteHierarch :: [Integer] -> Hierarch v -> Hierarch v
multiPermuteHierarch [] elems = elems
multiPermuteHierarch (i:ints) elems = multiPermuteHierarch ints $ permuteHierarch i elems

powerTest = powerHierarch $  permuteHierarch 4 h4

multiPowerPermuteHierarch :: [Hierarch v -> Hierarch v] -> Hierarch v -> Hierarch v
multiPowerPermuteHierarch [] = id
multiPowerPermuteHierarch (f:rest) = multiPowerPermuteHierarch rest . f 

powerTest0 = multiPowerPermuteHierarch [multiPermuteHierarch [1]]
powerTest1 = multiPowerPermuteHierarch [powerHierarch]
powerTest2 = multiPowerPermuteHierarch [powerHierarch, multiPermuteHierarch [1]]
powerTest3 = multiPowerPermuteHierarch [powerHierarch, multiPermuteHierarch [1,2]]

nOverK :: (Eq v) => Integer -> Hierarch v -> Hierarch v
nOverK 0 _           = Coll [ Coll [] ]
nOverK n (Coll list) = Coll $
  do
    e <- list
    let (Coll rest) = nOverK (n-1) (eliminated e list)
    (Coll r) <- rest
    return $ Coll $ e : r
    -- [ Coll $ e : r | e <- list, (Coll r) <- rest]
  where
    eliminated e (e':r)
         | e == e'   = Coll r
         | otherwise = eliminated e r
    eliminated _ [] = error "not valid"

nOverKTest1 = nOverK 3 $ Coll $ map Atom [1..5]
nOverKpowerHierarchTest = powerHierarch $ nOverK 3 $ Coll $ map Atom [1..5]

multiPowerPermuteNOverKHierarch :: [Hierarch v -> Hierarch v] -> Hierarch v -> Hierarch v
multiPowerPermuteNOverKHierarch [] = id
multiPowerPermuteNOverKHierarch (f:rest) = multiPowerPermuteNOverKHierarch rest . f

nOverKPowerPermuteHierarchTest = multiPowerPermuteHierarch [powerHierarch, multiPermuteHierarch [1,2], nOverK 2] h4


gen list 0 = [ [] ]
gen list n = [ new:old | new <- list, old <- gen list (n-1) ]


-----------------------------------------------------------------------------------------------------------

newtype Set a = Set [a] deriving Show

powerSet :: Eq a => Set a -> Set (Set a)
powerSet (Set xs) = Set (map (\xs -> (Set xs)) (powerList'' xs))

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

powerList'' [] = [[]]
powerList'' list = [] :  [e:l | e <- list
                              , l <- powerList'' 
                                     $ eliminated e list]
  where
      eliminated :: Eq a => a -> [a] -> [a]
      eliminated e (e':r)
         | e == e'   = r
         | otherwise = eliminated e r
      eliminated _ [] = error "not valid"