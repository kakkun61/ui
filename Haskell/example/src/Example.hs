{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Example
  ( main
  ) where

import Example.Data (Command (GetDotNetDescription), Flags,
                     Message (ButtonClicked, DotNetDescription, DotNetDescriptionButtonClicked),
                     Model (Model, count, dotNetDescription), Subscription)

import Hui (Program, View (Button, View), program)

import qualified Data.Text as Text

#ifdef FOREIGN
import Foreign.C.Types (CInt (CInt))
#endif

main :: Program Flags Model Message Command Subscription
main = program initialize view update subscriptions

#ifdef FOREIGN
foreign export ccall main :: Program Flags Model Message Command Subscription
#endif

initialize :: Flags -> (Model, [Command])
initialize _ = (Model 0 "-", mempty)

view :: Model -> View Message
view Model { count, dotNetDescription } =
  View
    [ View
        [ Button ".NET Version" (Just DotNetDescriptionButtonClicked)
        , Button dotNetDescription Nothing
        ]
    , View $ flip fmap [0 .. count] $ \c -> Button (Text.pack $ show c) (Just ButtonClicked)
    ]

update :: Message -> Model -> (Model, [Command])
update ButtonClicked model@Model { count }   = (model { count = count + 1 }, mempty)
update DotNetDescriptionButtonClicked model  = (model, [GetDotNetDescription])
update (DotNetDescription description) model = (model { dotNetDescription = description }, mempty)

subscriptions :: Model -> [Subscription]
subscriptions _ = mempty
