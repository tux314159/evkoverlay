{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module System.Evdev.TH
  ( genKeyCodes,
    readLinuxInputHdr,
  )
where

import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH
import Text.Read (readMaybe)

genKeyCodes :: String -> String -> [(Text, Int)] -> Q [Dec]
genKeyCodes keyCodeName fromKeyCodeName keys = do
  keyCodeType <-
    dataD
      (pure [])
      (mkName keyCodeName)
      []
      Nothing
      (mkKeyCons $ unpack . fst <$> keys)
      [derivClause Nothing [[t|Eq|], [t|Ord|], [t|Show|]]]

  tyVar' <- varT =<< newName "a"
  funSig <-
    let tyVar = pure tyVar'
        keyCodeT = conT $ mkName keyCodeName
     in sigD
          (mkName fromKeyCodeName)
          [t|(Integral $tyVar) => $tyVar -> Maybe $keyCodeT|]

  funDefs <-
      pure <$>
        funD
          (mkName fromKeyCodeName)
          (genFuncClause <$> keys)

  pure $ keyCodeType : funSig : funDefs
  where
    mkKeyCon name = normalC (mkName name) []
    mkKeyCons = fmap mkKeyCon
    genFuncClause (res, num) =
      clause
          (pure [p|$(litP . IntegerL . fromIntegral $ num)|])
          (normalB [e|Just $(conE . mkName . unpack $ res)|])
          []

readLinuxInputHdr :: FilePath -> Q [(Text, Int)]
readLinuxInputHdr path =
  mapMaybe
    ( toTup . filter (not . T.null) . T.split isSpace
        <=< T.stripPrefix "#define KEY_"
    )
    . T.splitOn "\n"
    <$> runIO (T.readFile path)
  where
    toTup (def : code : _) = readMaybe (T.unpack code)
      >>= \x -> Just ("Key" <> T.toTitle def, x)
    toTup _ = Nothing
