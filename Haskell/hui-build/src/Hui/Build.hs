{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Hui.Build
  ( build
  , Config (..)
  , SearchKey (..)
  ) where

import           Cabal.Plan         (CompName (CompNameFLib), PkgId (PkgId), PkgName (PkgName), PlanJson (pjUnits),
                                     SearchPlanJson (ProjectRelativeToDir), Unit (Unit, uComps, uDistDir, uPId),
                                     UnitId (UnitId), findAndDecodePlanJson)
import           Data.Either.Result (pattern Error, Result, pattern Success)
import qualified Data.List          as L
import qualified Data.Map.Strict    as M
import qualified Data.Maybe         as L (mapMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           System.Directory   (copyFile, getCurrentDirectory)
import           System.FilePath    ((</>))

data Config =
  Config
    { searchKey   :: SearchKey
    , dllName     :: Maybe String
    , destination :: String
    }
  deriving (Eq, Ord, Show, Read)

data SearchKey
  = PackageNameKey String
  | UnitIdKey String
  deriving (Eq, Ord, Show, Read)

build :: Config -> IO ()
build Config { searchKey, destination } = do
  dir <- getCurrentDirectory
  plan <- findAndDecodePlanJson $ ProjectRelativeToDir dir
  let
    units = pjUnits plan
    foundUnit :: Result Unit
    foundUnit =
      case searchKey of
        PackageNameKey n ->
          case M.toList $ M.filter (\Unit { uPId = PkgId m _ } -> PkgName (T.pack n) == m) units of
            [(_, u)] -> pure u
            [] -> fail "No units found"
            us -> fail $ "Some units found: " ++ L.intercalate ", " (T.unpack . (\(UnitId i) -> i) . fst <$> us)
        UnitIdKey i ->
          case units M.!? UnitId (T.pack i) of
            Nothing -> fail "No units found"
            Just u  -> pure u
  case foundUnit of
    Error err -> fail err
    Success Unit { uComps, uDistDir } -> do
      let
        isFLib (CompNameFLib n) = Just n
        isFLib _                = Nothing
        foundFLib :: Result Text
        foundFLib =
          case L.mapMaybe isFLib $ M.keys uComps of
            [n] -> pure n
            []  -> fail "No flib components found"
            cs  -> fail $ "Some flib components found: " ++ L.intercalate ", " (T.unpack <$> cs)
      case (foundFLib, uDistDir) of
        (Error err, _) -> fail err
        (_, Nothing) -> fail "No distribution directories are there"
        (Success flib, Just distDir) -> do
          let
            l = T.unpack flib
            dist = distDir </> "build" </> l </> l ++ ".dll"
          copyFile dist destination
