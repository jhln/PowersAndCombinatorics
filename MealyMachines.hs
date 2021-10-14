module MealyMachines where

--import Control.Monad (MonadPlus)

push :: ([a], a) -> [a]
push (s, a) = a : s
pop :: [a] -> [a]
pop = tail
top :: [a] -> a
top = head
empty :: [a] -> Bool
empty s = (length s == 0)

(!) :: b -> ()
(!) _ = ()

--push' :: ([a], a) -> ([a], ())
--push' = push /-\ !

(/-\) :: (t -> a) -> (t -> b) -> t -> (a, b)
(f /-\ g) x = (f x, g x)

--empty' :: ([a], ()) -> ([a],Bool)
--empty' = (id /-\ empty) . pi_1

pi_1 :: (a, b) -> a
pi_1 (x,y) = x


(<==) :: (a -> b) -> (a -> Bool) -> a -> Maybe b
(f <== p ) a = if p a then (Just . f ) a else Nothing


top' :: ([a], ()) -> Maybe ([a], a)
top' = ((id /-\ top) <== (not . empty)) . pi_1

pop' :: ([a], ()) -> Maybe ([a], a)
pop' = ((pop /-\ top) <== (not . empty)) . pi_1

push' :: ([a], a) -> Maybe ([a], ())
push' = Just . (push /-\ (!))

empty' :: ([a], ()) -> Maybe ([a], Bool)
empty' = Just . (id /-\ empty) . pi_1


stack :: ([a], Either (Either (Either () ()) a) ()) 
  -> Maybe ([a],  Either (Either (Either a a) ()) Bool)
stack = pop' |+| top' |+| push' |+| empty'

(|+|) :: (Functor f) 
  => ((s,i) -> f (s, o)) -> ((s,j) -> f (s, p))
  -> (s, Either i j) -> f (s, Either o p)
--(|+|) = (fmap drConv) . cozip . (m1 + m2) . dr
(|+|) = undefined

dr :: (s, Either i j) -> Either (s,i) (s,j)
dr (s, Left i) = Left (s,i)
dr (s, Right j) = Right (s,j)

in_1 :: (f, g) -> f
in_1 (f, g) = f
in_2 :: (f, g) -> g
in_2 (f, g) = g

{-
drConv :: Either (s,i) (s,j) -> (s, Either i j)
--drConv = [id × i1, id × i2]
drConv (Left (s,i)) = (s, Left i)
drConv (Right (s,j)) = (s, Right j)

cozip :: Functor f => Either (f a) (f b) -> f (Either a b)
cozip (Left f_l) = fmap f_l 
cozip (Right f_r) = helperRight
  where
    helperLeft (Left a) = fmap f_l a
-}