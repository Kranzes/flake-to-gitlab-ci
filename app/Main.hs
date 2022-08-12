{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
module Main where

import           Control.Exception
import           Control.Lens          hiding ((|>))
import           Data.Aeson            as A
import           Data.Aeson.KeyMap
import           Data.Aeson.Lens
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Map              as Map
import           Data.Maybe
import           Data.Text
import qualified Data.Yaml.Pretty      as Y
import           Formatting
import           GHC.Generics
import           Shh
import           System.Which          (staticWhich)

nixBin :: FilePath
nixBin = $(staticWhich "nix")

data Step = Step {
  script :: [Text]
} deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

nixFlakeShowJson :: Proc ()
nixFlakeShowJson = mkProc (BSC.fromStrict $ BSC.pack nixBin) ["flake", "show", "--allow-import-from-derivation", "--json"]

nixBuildCmd :: Format r (Text -> Text -> r)
nixBuildCmd = "nix build -L .#" % stext % "." % stext

getChecksJSON :: Value -> [Text]
getChecksJSON = Map.keys . toMapText . view (_Object . ix "checks" . _Object . ix "x86_64-linux" . _Object)

getPackagesJSON :: Value -> [Text]
getPackagesJSON = Map.keys . toMapText . view (_Object . ix "packages" . _Object . ix "x86_64-linux" . _Object)

mkCheck :: Text -> (Text, Step)
mkCheck x = (sformat ("Check " % stext) x, Step $ [sformat nixBuildCmd "checks.x86_64-linux" x] )

mkPkg :: Text -> (Text, Step)
mkPkg x = (sformat ("Package " % stext) x, Step $ [sformat nixBuildCmd "packages.x86_64-linux" x])

main = do
  x <- nixFlakeShowJson |> capture
  case A.eitherDecode @Value x of
    Left e -> error "foo"
    Right r -> do
      let pkgs = fmap mkPkg $ getPackagesJSON r
          chks = fmap mkCheck $ getChecksJSON r
      BS.putStr $ Y.encodePretty Y.defConfig $ Map.fromList $ pkgs <> chks
