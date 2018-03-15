module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import App.Component (component)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: ∀ eff. Eff (HA.HalogenEffects (console :: CONSOLE, random :: RANDOM | eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
