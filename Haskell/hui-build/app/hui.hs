import Hui.Build          (Config (Config, destination, dllName, searchKey), SearchKey (PackageNameKey), build)
import System.Environment (getArgs)
import System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [p, d] ->
      build $
        Config
          { searchKey = PackageNameKey p
          , dllName = Nothing
          , destination = d
          }
    _ -> hPutStrLn stderr usage

usage :: String
usage = "hui PACKAGE_NAME DESTINATION"
