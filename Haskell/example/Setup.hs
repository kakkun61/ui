import Control.Monad                      (filterM, when)
import Data.List.NonEmpty                 (NonEmpty ((:|)))
import Data.Semigroup                     (Max (Max, getMax), Min (Min, getMin), sconcat)
import Data.Traversable                   (for)
import Distribution.Simple                (Args, UserHooks (preBuild), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup          (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import System.Directory                   (doesDirectoryExist, doesFileExist, doesPathExist, getModificationTime,
                                           listDirectory)
import System.FilePath                    ((</>))
import System.Process                     (callProcess)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preBuild = hprotoc }

hprotoc :: Args -> BuildFlags -> IO HookedBuildInfo
hprotoc _ _ = do
  let
    protobufDir = "protobuf"
    genDir = "gen"
  protos <- listDirectory protobufDir
  genExist <- doesPathExist genDir
  if genExist
    then do
      gens <- listDirectoryRec "gen"
      case ((protobufDir </>) <$> protos, gens) of
        (p:ps, g:gs) -> do
          let
            protos' = p:|ps
            gens' = g:|gs
          nec <- necessaryUpdate protos' gens'
          when nec $ protoc protobufDir genDir
        _ -> fail "too few files"
    else protoc protobufDir genDir
  pure emptyHookedBuildInfo

protoc :: FilePath -> FilePath -> IO ()
protoc protobufDir genDir = do
  protos <- listDirectory protobufDir
  callProcess
    "hprotoc"
    $ mconcat
        [ [ "--proto_path", protobufDir
          , "--haskell_out", genDir
          , "--prefix", "Example"
          ]
        , protos
        ]

necessaryUpdate :: NonEmpty FilePath -> NonEmpty FilePath -> IO Bool
necessaryUpdate srcs gens = do
  srcModTimes <- sequence $ getModificationTime <$> srcs
  genModTimes <- sequence $ getModificationTime <$> gens
  let
    lastSrcModTime = getMax $ sconcat $ Max <$> srcModTimes
    firstGenModTime = getMin $ sconcat $ Min <$> genModTimes
  pure $ firstGenModTime < lastSrcModTime

listDirectoryRec :: FilePath -> IO [FilePath]
listDirectoryRec path = do
  children <- ((path </>) <$>) <$> listDirectory path
  childFiles <- filterM doesFileExist children
  childDirectories <- filterM doesDirectoryExist children
  grandchildFiles <- mconcat <$> for childDirectories listDirectoryRec
  pure $ childFiles <> grandchildFiles
