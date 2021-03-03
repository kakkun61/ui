{-# LANGUAGE CPP #-}

module Example.Data
  ( Model (..)
  , Message (..)
  , Command (..)
  ) where

import           Hui (Program, View (Button, View), program)
import qualified Hui

import qualified Data.Sequence   as Seq
import           Foreign.C.Types (CInt (CInt))

newtype Model = Model { count :: Word } deriving (Show, Read, Eq, Ord)

data Message = ButtonClicked deriving (Show, Read, Eq, Ord)

data Command = NoOp deriving (Show, Read, Eq, Ord)

instance Semigroup Command where
  _ <> _ = NoOp

instance Monoid Command where
  mempty = NoOp
