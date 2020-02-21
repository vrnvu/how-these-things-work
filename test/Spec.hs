{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import HelloWorld (hello)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "hello" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion  = expression `shouldBe` expected
        expression = hello a b

data Case = Case { description :: String
                 , a     :: String
                 , b     :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "empty"
               , a     = ""
               , b     = ""
               , expected    = ""
               }
        , Case { description = "two As"
               , a     = "A"
               , b     = "A"
               , expected    = "AA"
               }
        , Case { description = "with spaces"
               , a     = "G  "
               , b     = "T"
               , expected    = "G  T"
               }
        ]
