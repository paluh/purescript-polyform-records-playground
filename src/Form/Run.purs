module Form.Run where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Debug.Trace (traceAnyA)
import Polyform.Validation (V(Invalid, Valid))

----------
-- Running the form

-- This helper reveals the structure of the forms with `pulp run`.
printResult :: ∀ err eff a. V err a -> Eff (console :: CONSOLE | eff) Unit
printResult =
  case _ of
    Valid form value → do
      log "FORM VALID:"
      traceAnyA form
      log "FINAL VALUE:"
      traceAnyA value

    Invalid form → do
      log "FORM INVALID:"
      traceAnyA form


validate :: ∀ a. a -> { value :: a, validate :: Boolean }
validate value = { value, validate: true }

notValidate :: ∀ a. a -> { value :: a, validate :: Boolean }
notValidate value = { value, validate: false }
