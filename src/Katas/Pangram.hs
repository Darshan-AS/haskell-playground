module Katas.Pangram where

import Data.Char (toLower)
import Prelude

isPangram :: String -> Bool
isPangram str = all (`elem` map toLower str) ['a' .. 'z']
