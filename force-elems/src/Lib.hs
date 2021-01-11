{-# OPTIONS_GHC -ddump-simpl #-}

module Lib
    ( forceElems
    , forceElemsList
    ) where

import Data.Coerce

newtype ValueStrict a = ValueStrict a

instance Functor ValueStrict where
  fmap f (ValueStrict a) = ValueStrict (a `seq` f a)

instance Applicative ValueStrict where
  pure = ValueStrict
  ValueStrict f <*> ValueStrict a = ValueStrict (f a)

forceElems :: Traversable t => t a -> t a
forceElems = coerce . traverse ValueStrict

forceElemsList :: [a] -> [a]
forceElemsList = forceElems
