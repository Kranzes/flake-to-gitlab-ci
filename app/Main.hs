{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Maybe
import Control.Exception
import Data.Aeson as A
import Data.Aeson.KeyMap
import Data.Aeson.Lens
import Data.Map as Map
import Formatting
import GHC.Generics
import Control.Lens hiding ((|>))
import Data.Text
import Shh
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

data Step = Step {
  script :: [Text]
} deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(load SearchPath ["nix"])

nixFlakeShowJson :: Proc ()
nixFlakeShowJson = nix ["flake", "show", "--allow-import-from-derivation", "--json"]

type AttrPath = Text

nixBuildCmd :: Format r (Text -> Text -> r)
nixBuildCmd = "nix build -L .#" % stext % "." % stext

data Item = Item {
   description :: Text
 , name :: Text
} deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

getChecksJSON :: Value -> [Text]
getChecksJSON = Map.keys . toMapText . view (_Object . ix "checks" . _Object . ix "x86_64-linux" . _Object)

getPackagesJSON :: Value -> [Text]
getPackagesJSON = Map.keys . toMapText . view (_Object . ix "packages" . _Object . ix "x86_64-linux" . _Object)

mkCheck :: Text -> (Text, Step)
mkCheck x = (sformat ("Check " % stext) x, Step $ [sformat nixBuildCmd "checks.x86_64-linux" x] )

mkPkg :: Text -> (Text, Step)
mkPkg x = (sformat ("Package " % stext) x, Step $ [sformat nixBuildCmd "packages.x86_64-linux" x])

mkDevShell :: (Text, Step)
mkDevShell = ("Build devShell", Step [ "nix build -L .#devShell.x86_64-linux"])

{--
toStep :: Text -> Text -> Value -> (Text, Step)
toStep id prefix x = (id, format 
--}

main = do
  x <- nixFlakeShowJson |> capture
  case A.eitherDecode @Value x of
    Left e -> error "foo"
    Right r -> do
      let pkgs = fmap mkPkg $ getPackagesJSON r
          chks = fmap mkCheck $ getChecksJSON r
          isDs = (r ^? (key "devShell")) :: Maybe Value
          ds = if (isJust isDs) then pure mkDevShell else []
      BS.putStr $ Y.encodePretty Y.defConfig $ Map.fromList $ pkgs <> chks <> ds
