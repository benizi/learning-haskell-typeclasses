-- {-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( someFunc
    ) where

{-import Prelude (Functor(..), Maybe(..), putStrLn, IO(..), (.)) hiding (Either(..))-}
import Prelude hiding (Either(..))
{-import Control.Applicative (Applicative(..))-}

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' _ [] = []
  fmap' g (x:xs) = g x : fmap' g xs
  -- equiv: fmap' = map

instance Functor' Maybe where
  fmap' _ Nothing = Nothing
  fmap' g (Just a) = Just (g a)

data Either e a = Left e | Right a

-- First attempt:
-- instance Functor (Either t) where
--   fmap g (Left l) = Left (g l)
--   fmap g (Right r) = Right (g r)

instance Functor' (Either a) where
  fmap' _ (Left l) = Left l
  fmap' g (Right r) = Right (g r)

instance Functor' ((,) a) where
  fmap' f (x,y) = (x, f y)

instance Functor' ((->) a) where
  fmap' = (.)

-- Functor laws:
-- fmap id = id
-- fmap (g . h) = fmap g . fmap h
-- fmap 'lifts' a function:  fmap :: (a -> b) -> (f a -> f b)

-- Pointed adds a "default context"
class Functor' f => Pointed f where
  pureP' :: a -> f a

-- Pointed law:
-- fmap g . pure = pure . g

instance Pointed Maybe where
  pureP' = Just

instance Pointed (Either a) where
  pureP' = Right

instance Pointed ((->) e) where
  pureP' = const

instance Monoid e => Pointed ((,) e) where
  pureP' = ((,) mempty)

class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

-- Applicative law
-- fmap g x = pure g <*> x
-- g <$> x = pure g <*> x

instance Applicative' Maybe where
  pure' = Just
  Just f <**> m = fmap' f m
  Nothing <**> _m = Nothing

instance Applicative' [] where
  pure' a = [a]
  _ <**> _ = undefined

newtype ZipList' a = ZipList' { getZipList' :: [a] }

instance Functor' ZipList' where
  fmap' f (ZipList' x) = ZipList' (map f x)

instance Applicative' ZipList' where
  pure' a = ZipList' (repeat a)
  (ZipList' gs) <**> (ZipList' xs) = ZipList' (zipWith ($) gs xs)

class Monad' m where
  return' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b -- >>=
  (>>==) :: m a -> (a -> m b) -> m b
  then' :: m a -> m b -> m b -- >>
  fail' :: String -> m a

  (>>==) = bind'
  then' m n = bind' m (\_ -> n)
  fail' = undefined

instance Monad' Maybe where
  return' = Just
  bind' (Just x) g = g x
  bind' Nothing _ = Nothing

instance Monad' [] where
  return' a = [a]
  {-bind' xs f = [(f x) | x <- xs]-}
  bind' xs f = [y | x <- xs, y <- f x]

instance Monad' ((->) e) where
  return' = const
  {-bind' e f = (\_ -> f)-}
  -- wtf?
  bind' f k = (\r -> k (f r) r)

class Applicative' m => Monad'' m where
  join' :: m (m a) -> m a

instance Monad'' Maybe where
  {-join' (Just (Just x)) = Just x-}
  {-join' _ = Nothing-}
  join' m = bind' m id

instance Monad'' [] where
  join' = concat

(>>=>>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
g >>=>> h = \x -> g x >>= h
(>=>) :: Monad' m => (a -> m b) -> (b -> m c) -> a -> m c
g >=> h = \x -> g x >>== h

{- the Identity monad -}
data W a = W a deriving Show
return_i :: a -> W a
return_i x = W x
fmap_i :: (a -> b) -> W a -> W b
{-fmap_i f (W x) = W (f x)-}
fmap_i f = bind_i (return_i . f)
bind_i :: (a -> W b) -> W a -> W b
bind_i f (W x) = f x
some_fn :: Int -> W Int
some_fn x = W (x + 1)

g_1 :: Int -> W Int -> W Int
g_1 x y = bind_i (return_i . (+ x)) y
{-h_2 :: W Int -> W Int -> W Int-}
{-h_2 x y = bind_i (\a -> return (a + y)) x-}

{- Monad laws -}
-- 1 + 2 = "return behaves nicely"
-- 3 = bind is associative-ish
-- return a >>= k = k a
-- m >>= return = m
-- m >>= (\x -> k x >>= h) = (m >>= k) ==> h
-- fmap f xs = xs >>= return . f = liftM

{- reformulated in terms of "fish": -}
-- g >=> h = \x -> g x >>= h
--
-- return >=> g = g
-- g >=> return = g
-- (g >=> h) >=> k = g >=> (h >=> k)

{- do notation -} {-
a >>= \x ->
b >>
c >>= \y ->
d

do { x <- a ;
     b     ;
     y <- c ;
     d
   }
-}

{- Monoids -}
-- set S, with binary operation (+), and identity (0), which is associative
class Monoid' a where
  mempty' :: a
  mappend' :: a -> a -> a
  mconcat' :: [a] -> a
  mconcat' = foldr mappend' mempty'

instance Monoid' [a] where
  mempty' = []
  mappend' = (++)

{- Monoid laws -}
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

instance Monoid' e => Applicative' ((,) e) where
  pure' x = (mempty', x)
  (u, f) <**> (v, x) = (u `mappend'` v, f x)

class Applicative' f => Alternative' f where
  empty' :: f a
  (<||>) :: f a -> f a -> f a

class Monad' m => MonadPlus' m where
  mzero' :: m a
  mplus' :: m a -> m a -> m a

{- MonadPlus laws -}
-- "choice and failure"
-- mzero >>= f = mzero
-- v >> mzero = mzero

{- Foldable -}
class Foldable' t where
  fold' :: Monoid' m => t m -> m
  foldMap' :: Monoid' m => (a -> m) -> t a -> m

  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl' :: (a -> b -> a) -> a -> t b -> a
  foldr1' :: (a -> a -> a) -> t a -> a
  foldl1' :: (a -> a -> a) -> t a -> a

  fold' = foldMap' id
  foldMap' f = foldr' (mappend' . f) mempty'

  foldr' = undefined
  foldl' = undefined
  foldr1' = undefined
  foldl1' = undefined

instance Foldable' [] where
  foldMap' g = mconcat' . map g

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

newtype Sum' a = Sum' { getSum' :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Sum' a) where
  mempty' = Sum' 0
  Sum' a `mappend'` Sum' b = Sum' (a + b)

instance Foldable' Tree where
  foldMap' f Empty = mempty'
  foldMap' f (Leaf x) = f x
  foldMap' f (Node l k r) = foldMap' f l ++ f k ++ foldMap' f r
    where (++) = mappend'

x :: Integer
x = getSum' $ foldMap' (mappend' (Sum' 1))
                       (Node
                        (Leaf (Sum' 10))
                        (Sum' 20)
                        (Node Empty
                              (Sum' 42)
                              (Leaf (Sum' 21))))

{- Traversable -}
class (Functor' t, Foldable' t) => Traversable' t where
  traverse' :: Applicative' f => (a -> f b) -> t a -> f (t b)
  sequenceA' :: Applicative' f => t (f a) -> f (t a)
  mapM' :: Monad' m => (a -> m b) -> t a -> m (t b)
  sequence' :: Monad' m => t (m a) -> m (t a)

  sequenceA' = undefined
  mapM' = undefined
  sequence' = undefined

instance Functor' Tree where
  fmap' _ Empty = Empty
  fmap' g (Leaf l) = Leaf $ g l
  fmap' g (Node l x r) = Node (fmap' g l) (g x) (fmap' g r)

instance Traversable' Tree where
  traverse' _ Empty = pure' Empty
  traverse' g (Leaf l) = fmap' Leaf (g l)
  traverse' g (Node l x r) = fmap' Node (traverse' g l) <**> (g x) <**> (traverse' g r)

{- Category -}

{- not Haskell98
class Category (~>) where
  id :: a ~> a
  (.) :: (b ~> c) -> (a ~> b) -> (a ~> c)
-}

class Category cat where
  id' :: cat a a
  comp :: cat b c -> cat a b -> cat a c

  (<<<) :: cat b c -> cat a b -> cat a c
  (<<<) = comp
  (>>>) :: cat a b -> cat b c -> cat a c
  g >>> h = comp h g

{-(>>>) :: Category-}
{-(>>>) x y = comp y x-}

-- Category law:
-- ((.), id) should be a Monoid:
-- id is the identity of (.)
-- (.) should be associative

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
instance Monad' m => Category (Kleisli m) where
  id' = Kleisli return'
  {- Kleisli g `comp` Kleisli h = Kleisli (h >=> g) -}
  {- Kleisli g `comp` Kleisli h = Kleisli (\x -> h x >>= g) -}
  Kleisli g `comp` Kleisli h = Kleisli (h >=> g)

{- Arrow -}
infixr 3 ***
infixr 3 &&&
infixr 1 >>>
class Category cat => Arrow cat where
  arr :: (b -> c) -> (cat b c)
  first :: (cat b c) -> (cat (b, d) (c, d))
  second :: (cat b c) -> (cat (d, b) (d, c))
  (***) :: (cat b c) -> (cat b' c') -> (cat (b, b') (c, c'))
  (&&&) :: (cat b c) -> (cat b c') -> (cat b (c, c'))

  second g = arr swap >>> first g >>> arr swap
    where swap ~(x, y) = (y, x)
  g *** h = first g >>> second h
  g &&& h = arr (\b -> (b, b)) >>> g *** h

instance Category (->) where
  id' = id
  comp = (.)

instance Arrow (->) where
  arr = id
  first g (x, y) = (g x, y)
  -- second g (x, y) = (x, g y)
  -- (g *** h) (x, y) = (g x, h y)
  -- (g &&& h) x = (g x, h x)

-- arr = pure
-- first = process the first, second is unchanged
-- second = process the second, first is unchanged
-- (***) = parallel composition

-- Arrow laws:
-- arr id = id
-- arr (h . g) = arr g >>> arr h
-- first (arr g) = arr (g *** id)
-- first (g >>> h) = first g >>> first h
-- first g >>> arr (id *** h) = arr (id *** h) >>> first g
-- first g >>> arr fst = arr fst >>> g
-- first (first g) >>> arr assoc = arr assoc >>> first g
--
--   where assoc ((x, y), z) = (x, (y, z))

class Arrow cat => ArrowChoice cat where
  left :: (cat b c) -> (cat (Either b d) (Either c d))
  right :: (cat b c) -> (cat (Either d b) (Either d c))
  (+++) :: (cat b c) -> (cat b' c') -> (cat (Either b b') (Either c c'))
  (|||) :: (cat b d) -> (cat c d) -> (cat (Either b c) d)

-- left = behavior of g on Left, id on Right
-- right = behavior of id on Left, g on Right
-- (+++) = multiplexing = sum of two arrows
-- (|||) = merge/fan-in = performs either g or h on its input

class Arrow cat => ArrowApply cat where
  app :: cat (cat b c, b) c

-- app = compute (like monadic bind)

-- ArrowApply is exactly equivalent to Monad in terms of expressive power

{- Comonad -}
-- Monad with its arrows reversed

class Functor' f => Copointed f where
  extract :: f a -> a -- dual of return
class Copointed w => Comonad w where
  duplicate :: w a -> w (w a) -- dual of join
  extend :: (w a -> b) -> w a -> w b -- dual of >>= (args in diff order)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
