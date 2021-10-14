module LabeledTransitionsAndCorecursion where

import Prelude hiding (iterate)
import System.Random (mkStdGen, randomRs)

-------------------------------------------------------
----- Streams 


ones = 1 : ones
nats = 0 : map (+1) nats
odds = 1 : map (+2) odds

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

theOnes = iterate id 1
theNats = iterate (+1) 0
theOdds = iterate (+2) 1

theNats1 = 0 : zipWith (+) ones theNats1
theFibs = 0 : 1 : zipWith (+) theFibs (tail theFibs)

pr (x1:x2:x3:xs) = x1*x3 - x2*x2 : pr (x2:x3:xs)

sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
  where
    mark (y:ys) k m 
      | k == m = 0 : (mark ys 1 m)
      | otherwise = y : (mark ys (k+1) m)

sieve' :: [Integer] -> [Integer]
sieve' (n:xs) = n : sieve' (filter (\ m -> (rem m n) /= 0) xs)

primes' :: [Integer]
primes' = sieve' [2..]


------------------------------------------------------


randomInts :: Int -> Int -> [Int]
randomInts bound seed =
  tail (randomRs (0,bound) (mkStdGen seed))


type Process = [Int] -> [String]

start :: Process -> Int -> Int -> [String]
start process bound seed = process (randomInts bound seed)

clock :: Process
clock (0:xs) = "tick" : clock xs
clock (1:xs) = "crack" : []

clockTest = start clock 1 25


----- simple vending machine

vending, vending1, vending2, vending3 :: Process
vending (0:xs)  = "coin"  : vending1 xs
vending (1:xs)  =           vending xs
vending1 (0:xs) = "coin"  : vending2 xs
vending1 (1:xs) = "water" : vending xs
vending2 (0:xs) = "coin"  : vending3 xs
vending2 (1:xs) = "beer"  : vending xs
vending3 (0:xs) = "moneyback" : vending xs
vending3 (1:xs) =           vending3 xs

trail1 = take 9 (start vending 1 1)
trail2 = take 8 (start vending 1 3)
trail3 = take 8 (start vending 1 22)


---- parking ticket dispenser

ptd :: Process
ptd = ptd0 0

ptd0 :: Int -> Process
ptd0 0 (0:xs) = ptd0 0 xs
ptd0 i (0:xs) = ("return " ++ show i ++ " euro") : ptd0 0 xs
ptd0 i (1:xs) = "1 euro" : ptd0 (i+1) xs
ptd0 i (2:xs) = "2 euro" : ptd0 (i+2) xs
ptd0 0 (3:xs) = ptd0 0 xs
ptd0 i (3:xs) = ("ticket " ++ show (i * 20) ++ " min") : ptd0 0 xs

ptdTest = take 6 (start ptd 3 457)


------------- another venidng machine

vend, vend1, vend2, vend3, vend4 :: Process
vend (0:xs) = "coin"    : vend1 xs
vend (1:xs) = "coin"    : vend4 xs
vend1 (0:xs) = "coin"   : vend2 xs
vend1 (1:xs) =            vend1 xs
vend2 (0:xs) = "beer"   : vend xs
vend2 (1:xs) = "coin"   : vend3 xs
vend3 (0:xs) = "moneyback": vend xs
vend3 (1:xs) =            vend3 xs
vend4 (0:xs) = "water"  : vend xs
vend4 (1:xs) =            vend4 xs

----------- mutually recursive actions

actions = user [0,0,1] responses
responses = vending actions
user acts ~(r:s:p:resps) = acts ++ user (proc [r,s,p]) resps
proc ["coin","coin","beer"] = [0,0,1]

testActions = take 8 actions
--- should be [0,0,1,0,0,1,0,0]
testResponses = take 8 responses
--- should be ["coin","coin","beer","coin","coin","beer","coin","coin"]


----- Proofs by approximation

approx :: Integer -> [a] -> [a]
approx (n+1) [] = []
approx (n+1) (x:xs) = x : approx n xs

