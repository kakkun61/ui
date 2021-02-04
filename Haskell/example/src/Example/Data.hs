{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Example.Data
  ( Model (..)
  , Message (..)
  ) where

import           Hui (Command, Program, View (Button, View), noopOutTag, program)
import qualified Hui

import qualified Data.Sequence   as Seq
import           Foreign.C.Types (CInt (CInt))

data Model = Model { count :: Word } deriving (Show, Read, Eq, Ord)

data Message = ButtonClicked deriving (Show, Read, Eq, Ord)

instance Hui.Message Message where
  messageTag _ = noopOutTag
