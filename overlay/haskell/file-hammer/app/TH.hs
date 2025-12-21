module TH where

import Data.Char (isUpper, toLower, toUpper)
import Data.Function ((&))
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (maybeToList)
import Language.Haskell.TH.Syntax (Name, mkName, nameBase)
import Lens.Micro (over, (.~))
import Lens.Micro.TH (DefName (..), LensRules, camelCaseFields, lensField)

duplicateRules :: LensRules
duplicateRules =
  camelCaseFields
    & lensField
      .~ camelCaseNamer

camelCaseNamer :: Name -> [Name] -> Name -> [DefName]
camelCaseNamer _tyName _fields field = maybeToList $ do
  let fieldPart = case nameBase field of
        (x : xs) -> toUpper x : xs
        [] -> []
  return $ MethodName (mkName $ "Has" ++ fieldPart) (mkName . ('_' :) $ nameBase field)
