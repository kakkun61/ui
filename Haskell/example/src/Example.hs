{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Example
  ( main
  , Flags (..)
  , Model (..)
  , Message (..)
  ) where

import Example.Data  (Message (ButtonClicked), Model (Model, count))
import Example.Flags (Flags (Flags))
import Example.View  ()

import           Hui (Command, Program, Subscription, View (Button, View, children), noopOutTag, program)
import qualified Hui

import qualified Data.Sequence   as Seq
import qualified Data.Text       as Text
import           Foreign.C.Types (CInt (CInt))

main :: Program Flags Model Message
main = program initialize view update subscriptions

#ifdef FOREIGN
foreign export ccall main :: Program Flags Model Message
#endif

initialize :: Flags -> (Model, Command Message)
initialize _ = (Model 0, mempty)

view :: Model -> View Message
view Model { count } = View $ flip fmap [0 .. count] $ \c -> Button (Text.pack $ show c) (Just ButtonClicked)

update :: Message -> Model -> (Model, Command Message)
update ButtonClicked Model { count } = (Model { count = count + 1 }, mempty)

subscriptions :: Model -> Subscription Message
subscriptions = undefined
