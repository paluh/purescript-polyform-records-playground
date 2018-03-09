module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Polyform.Validation (runValidation)
import Form (signupForm)
import Form.Run (notValidate, printResult, validate)
import App.Component (component)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: ∀ eff. Eff (HA.HalogenEffects (console :: CONSOLE, random :: RANDOM | eff)) Unit
main = do
  log "EXAMPLE"

  -- | Nothing is validated - we are getting FORM VALID but without any result ;-)
  v1 <- runValidation signupForm {email: notValidate "wrongemailformat", password1: notValidate "shrt", password2: notValidate "nodigits"}
  printResult v1

  -- | Only first field is validated but wrong
  v2 <- runValidation signupForm {email: validate "wrongemailformat", password1: notValidate "shrt", password2: notValidate "nodigits"}
  printResult v2

  -- | Only first field is validated and it is ok - result is still Nothing
  v3 <- runValidation signupForm {email: validate "email@example.com", password1: notValidate "shrt", password2: notValidate "nodigits"}
  printResult v3

  -- | First two fields are validated
  v4 <- runValidation signupForm {email: validate "wrongemailformat", password1: validate "shrt", password2: notValidate "nodigits"}
  printResult v4

  -- | All fields are validated
  v5 <- runValidation signupForm {email: validate "wrongemailformat", password1: validate "shrt", password2: validate "nodigits"}
  printResult v5

  -- | Form is really validated and valid
  v6 <- runValidation signupForm {email: validate "email@example.com", password1: validate "password921", password2: validate "password921"}
  printResult v6

  -- | Now, run application
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
