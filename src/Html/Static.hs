module Html.Static
  ( static
  ) where

import Language.Haskell.TH
import Html.Convert

static :: Convert a => a -> Q Exp
static = undefined
