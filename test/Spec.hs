{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import UniversalMachine (
    Tape(..)
    , Number(..)
    , Inst(..)
    , Point(..)
    , P(..)
    , readHead
    , moveLeft
    , moveRight
    )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "MoveTapeTest" $ for_ cases test0
  where

    test0 MoveTapeTest{..} = it description assertion
      where
        assertion  = expression `shouldBe` expected
        expression = readHead tape

data MoveTapeTest = MoveTapeTest { description :: String
                 , tape     :: Tape Number
                 , expected    :: Number
                 }

cases :: [MoveTapeTest]
cases = [ MoveTapeTest { description = "Element 0"
               , tape     = Tape [] (Successor Zero) []
               , expected    = (Successor Zero)
               }
        , MoveTapeTest { description = "Element 1"
               , tape     = Tape [Zero] (Successor (Successor Zero)) [Zero]
               , expected    = (Successor (Successor Zero))
               }
        ]
