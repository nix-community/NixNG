module TH (duplicateRules) where

import Data.Char (toUpper)
import Data.Function ((&))
import Data.Maybe (maybeToList)
import Language.Haskell.TH.Syntax (Name, mkName, nameBase)
import Lens.Micro ((.~))
import Lens.Micro.TH (DefName (MethodName), LensRules, camelCaseFields, lensField)

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
