module Example.Flags
  ( Flags (..)
  ) where

import Example.Protobuf.Flags (Flags (Flags))

import Hui.Code (Decode)

instance Decode Flags
