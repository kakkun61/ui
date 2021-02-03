import Distribution.Simple                (Args, UserHooks (preBuild), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup          (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import System.Process                     (callProcess)
import System.Directory (listDirectory)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preBuild = hprotoc }

hprotoc :: Args -> BuildFlags -> IO HookedBuildInfo
hprotoc _ _ = do
  let protobufDir = "protobuf"
  protos <- listDirectory protobufDir
  callProcess
    "hprotoc"
    $ mconcat
        [ [ "--proto_path", protobufDir
          , "--haskell_out", "gen"
          , "--prefix", "Example.ProtocolBuffers"
          ]
        , protos
        ]
  pure emptyHookedBuildInfo
