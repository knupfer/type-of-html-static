{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Html.Static
  ( static
  ) where

import Language.Haskell.TH
import Data.Proxy
import Html

-- | Template haskell function to annote which parts of a html document are static.
--
-- >>> :t $(static (div_ "<>"))
-- Proxy "<div>&lt;&gt;</div>"
--
-- Note that you can annote any part of the document which is itself a
-- valid document and doesn't contain variables which are defined in
-- the same file.
--
-- >>> :t div_ (\x -> $(static (div_ "<>")) # x)
-- 'Div > (\b -> Proxy "<div>&lt;&gt;</div>" # b)
static :: Document a => a -> Q Exp
static x = pure (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (LitT (StrTyLit (renderString x)))))
