module Utils
  ( Parser
  ) where

import Data.Text qualified as T
import Data.Void ( Void )
import Text.Megaparsec ( Parsec )

type Parser = Parsec Void T.Text
