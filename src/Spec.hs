{-# LANGUAGE ViewPatterns #-}

module Spec where

import Prelude hiding ((.))

import Data.Functor.Syntax
import Data.Monoid
import Test.Hspec
import Test.QuickCheck

-- QuickCheck properties...

functorIdentityEq :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentityEq f = fmap id f == f

functorComposeEq :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorComposeEq f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

-- type IntToInt = Fun Int Int
-- type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
-- functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
-- functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

data Three a b c = Three a b c deriving (Show)

data Three' a b = Three' a b b deriving (Show)

data Four a b c d = Four a b c d deriving (Show)

data Four' a b = Four' a a a b deriving (Show)

-- Can you implement one for this type? Why? Why not?
data Trivial = Trivial deriving (Show)




main :: IO ()
main = do
 hspec spec1
 hspec spec2
 hspec spec3
 hspec spec4
 hspec spec5
 hspec spec6

spec1 :: Spec
spec1 = do
  describe "[a]" $ do
    context "Functor Identity" $ do
      it "" $ do property $ \x -> functorIdentityEq (x :: [Int])
      it "" $ do property $ \x -> functorIdentityEq (x :: [String])
    context "Functor Composability" $ do
      it "" $ do property $ \x -> functorComposeEq (+1) (*2) (x :: [Int])
      it "" $ do property $ \x -> functorComposeEq (++"Hey") ("You " <>) (x :: [String])



-- Either Mock
data Or a b = Fst a | Snd b deriving (Eq, Show)
instance Functor (Or a) where
  fmap _ (Fst a) = Fst a
  fmap f (Snd b) = Snd (f b)

spec2 :: Spec
spec2 = do
  describe "Or a b" $ do
    context "Functor Identity" $ do
      it "" $ do property $ \x -> functorIdentityEq (Fst x :: Or Int Int)
      it "" $ do property $ \x -> functorIdentityEq (Fst x :: Or String Int)
      it "" $ do property $ \x -> functorIdentityEq (Snd x :: Or Int Int)
      it "" $ do property $ \x -> functorIdentityEq (Snd x :: Or Int String)
    context "Functor Composability" $ do
      it "" $ do property $ \x -> functorComposeEq (+1) (*2) (Fst x :: Or Int Int)
      it "" $ do property $ \x -> functorComposeEq (+1) (*2) (Snd x :: Or Int Int)
      it "" $ do property $ \x -> functorComposeEq (++"Hey") ("You " <>) (Fst x :: Or String String)
      it "" $ do property $ \x -> functorComposeEq (++"Hey") ("You " <>) (Snd x :: Or String String)





newtype Identity a = Identity a deriving (Eq, Show)
instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

spec3 :: Spec
spec3 = do
  describe "Identity a" $ do
    context "Functor Identity" $ do
      it "" $ do property $ \x -> functorIdentityEq (Identity x :: Identity Int)
      it "" $ do property $ \x -> functorIdentityEq (Identity x :: Identity String)
    context "Functor Composability" $ do
      it "" $ do property $ \x -> functorComposeEq (+1) (*2) (Identity x :: Identity Int)
      it "" $ do property $ \x -> functorComposeEq (++"Hey") ("You " <>) (Identity x :: Identity String)





data Pair a = Pair a a deriving (Eq, Show)
instance Functor (Pair) where
  fmap f (Pair a b) = Pair (f a) (f b)

spec4 :: Spec
spec4 = do
  describe "Pair a" $ do
    context "Functor Identity" $ do
      it "" $ do property $ \x y -> functorIdentityEq (Pair x y :: Pair Int)
      it "" $ do property $ \x y -> functorIdentityEq (Pair x y :: Pair String)
      it "" $ do property $ \x y -> functorIdentityEq (Pair x y :: Pair Char)
    context "Functor Composability" $ do
      it "" $ do property $ \x y -> functorComposeEq (+1) (*2) (Pair x y :: Pair Int)
      it "" $ do property $ \x y -> functorComposeEq (++"Hey") ("You " <>) (Pair x y :: Pair String)
      it "" $ do property $ \x y -> functorComposeEq (const 'p') (const 'f') (Pair x y :: Pair Char)




-- Tuple Mock
data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

spec5 :: Spec
spec5 = do
  describe "Two a b" $ do
    context "Functor Identity" $ do
      it "" $ do property $ \x y -> functorIdentityEq (Two x y :: Two Int Int)
      it "" $ do property $ \x y -> functorIdentityEq (Two x y :: Two String Int)
      it "" $ do property $ \x y -> functorIdentityEq (Two x y :: Two Int String)
      it "" $ do property $ \x y -> functorIdentityEq (Two x y :: Two String String)
    context "Functor Composability" $ do
      it "" $ do property $ \x y -> functorComposeEq (+1) (*2) (Two x y :: Two Int Int)
      it "" $ do property $ \x y -> functorComposeEq (++"Hey") ("You " <>) (Two x y :: Two String String)
      it "" $ do property $ \x y -> functorComposeEq (const 'p') (const 'f') (Two x y :: Two Char Char)





data And a b = And a b deriving (Eq, Show)
instance Functor (And a) where
  fmap f (And a b) = And a (f b)

spec6 :: Spec
spec6 = do
  describe "Two a b" $ do
    context "Functor Identity" $ do
      it "" $ do property $ \x y -> functorIdentityEq (Two x y :: Two Int Int)
      it "" $ do property $ \x y -> functorIdentityEq (Two x y :: Two String Int)
      it "" $ do property $ \x y -> functorIdentityEq (Two x y :: Two Int String)
      it "" $ do property $ \x y -> functorIdentityEq (Two x y :: Two String String)
    context "Functor Composability" $ do
      it "" $ do property $ \x y -> functorComposeEq (+1) (*2) (Two x y :: Two Int Int)
      it "" $ do property $ \x y -> functorComposeEq (++"Hey") ("You " <>) (Two x y :: Two String String)
      it "" $ do property $ \x y -> functorComposeEq (const 'p') (const 'f') (Two x y :: Two Char Char)


