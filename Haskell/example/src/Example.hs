{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Example
  ( main
  ) where

import Example.Flags (Flags)
import Example.View  ()

import           Hui (Command, Program, View (Button, View), noopOutTag, program)
import qualified Hui

import qualified Data.Sequence   as Seq
import           Foreign.C.Types (CInt (CInt))

data Model = Model

data Message = Message

instance Hui.Message Message where
  messageTag _ = noopOutTag

main :: Program Flags Model Message
main = program initialize view update subscriptions

#ifdef FOREIGN
foreign export ccall main :: Program Flags Model Message
#endif

initialize :: Flags -> (Model, Command Message)
initialize _ = (Model, mempty)

view :: Model -> View Message
view _ = View [Button "one", Button "two"]

update = undefined

subscriptions = undefined
