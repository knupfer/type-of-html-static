{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Main where

import Html
import Html.Static

import qualified Html.Attribute as A

import Data.Proxy

main :: IO ()
main = pure ()
  where
    _t_ =
      ( div_ "a" :: 'Div > String
      , $(static (div_ "a")) :: Proxy "<div>a</div>"
      , $(static (map td_ [1..3::Int])) :: Proxy "<td>1</td><td>2</td><td>3</td>"
      , $(static (div_ "<")) :: Proxy "<div>&lt;</div>"
      )

