module MicroCabal.YAML(
  YAMLValue(..),
  YAMLFieldName,
  showYAML,
  ) where
type YAMLFieldName = String

data YAMLValue
  = YString String
  | YBool   Bool
  | YInt    Int
  | YRecord [(YAMLFieldName, YAMLValue)]
  | YArray  [YAMLValue]
  deriving (Show)

showYAML :: YAMLValue -> String
showYAML = show
