{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -ddump-simpl #-}

module Lib
    ( forceElems
    , forceElemsList
    -- , forceElemsTree2
    , forceElemsSnocL
    ) where

data T a = L a | S a

getT :: T a -> a
getT (L a) = a
getT (S a) = a

data SnocL a
    = Snoc (SnocL a) a | Lin
    deriving (Functor, Foldable, Traversable)

data Tree2 a
    = Fork2 (Tree2 a) a (Tree2 a) a
    deriving (Functor, Foldable, Traversable)

instance Functor T where
  fmap f ta = pure f <*> ta

instance Applicative T where
  pure      = L
  tf <*> ta = L $
    case ta of
      L a -> getT tf a
      S a -> getT tf $! a

forceElems :: Traversable t => t a -> t a
forceElems = getT . traverse S

forceElemsList :: [a] -> [a]
forceElemsList = forceElems

forceElemsSnocL :: SnocL a -> SnocL a
forceElemsSnocL = forceElems

forceElemsTree2 :: Tree2 a -> Tree2 a
forceElemsTree2 = forceElems
