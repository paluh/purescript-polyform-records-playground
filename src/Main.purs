module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (random)
import Data.Array (any, elem)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, indexOf, length, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Debug.Trace (traceAnyA)
import Polyform.Validation (V(..), Validation(..), runValidation)
import Polyform.Validation as Validation
import Type.Prelude (SProxy(..))

-- | Let's assume that our fields are really simple
-- | and contain only validation result.
-- | Errors are kept in `Array`.
type Input err value = V (Array err) value

-- | Let's define some simple validators for email field

-- | ...of course they are really dummy validators ;-)

emailFormat = Validation.hoistFnV \e →
  if contains (Pattern "@") e
    then pure e
    else Invalid [inj (SProxy ∷ SProxy "emailFormat") e]

emailIsUsed = Validation.hoistFnMV \e → do
  -- | Some effectful computation inside your monad
  -- | Let's toss a coin instead of checkin db
  -- | if email is really used
  v ← random
  pure $ if v > 0.5
    then Invalid [inj (SProxy ∷ SProxy "isUsed") e]
    else pure e

emailFieldValidation = emailFormat *> emailIsUsed

-- | Let's define some simple validators for password field

minLength m = Validation.hoistFnV \p →
  if length p < m
    then Invalid [inj (SProxy ∷ SProxy "minLength") (Tuple m p)]
    else pure p

maxLength m = Validation.hoistFnV \p →
  if length p > m
    then Invalid [inj (SProxy ∷ SProxy "maxLength") (Tuple m p)]
    else pure p

hasDigit = Validation.hoistFnV \p →
  let
    chars = toCharArray p
  in
    if any (_ `elem` chars) (toCharArray "0123456789")
      then pure p
      else Invalid [inj (SProxy ∷ SProxy "hasDigit") p]

passwordFieldValidation min max = maxLength max *> minLength min *> hasDigit

data Field
  = EmailField (Input (Variant (emailFormat ∷ String, isUsed ∷ String)) String)
  | PasswordField (Input (Variant (hasDigit ∷ String, maxLength ∷ Tuple Int String, minLength ∷ Tuple Int String)) String)



-- | Form types and form related helpers and validations

type Form = Tuple (Array String) (Array Field)

-- | Let's build our form without any external helpers
-- | This function builds single field form from:
-- |  * value fetcher
-- |  * field type constructor
-- |  * field validation
-- |
-- | Here we can also observe that validation is
-- | nothing more than function from input to V
-- | in monadic context.
fieldForm fetchValue constructor fieldValidation =
  Validation $ \inputRecord → do
    -- | Fetch field value from record using fetcher
    let inputValue = fetchValue inputRecord
    -- | Run field validation agains this value
    r ← Validation.runValidation fieldValidation inputValue
    -- | Based on field validation result let's return:
    pure $ case r of
      -- | form togheter with result value
      -- | so we can combine both into larger values and forms
      Valid e v → Valid (Tuple [] [constructor (Valid e v)]) v
      -- | or form as representation of our error which
      -- | can be combined with other forms
      Invalid e → Invalid (Tuple [] [constructor (Invalid e)])

emailForm = fieldForm (_.email) EmailField emailFieldValidation

buildPasswordForm fetch = fieldForm fetch PasswordField (passwordFieldValidation 5 50)

passwordForm
  = ({password1: _, password2: _} <$> (buildPasswordForm _.password1) <*> (buildPasswordForm _.password2))
  >>> Validation.hoistFnV \{ password1, password2 } →
    if password1 /= password2
      then Invalid (Tuple ["Password dont match"] [])
      else pure password1

signupForm = {password: _, email: _} <$> passwordForm <*> emailForm

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

main = do
  log "EXAMPLE"

  v1 ← runValidation signupForm {email: "wrongemailformat", password1: "shrt", password2: "nodigits"}
  printResult v1

  log "\n\n"

  v2 ← runValidation signupForm {email: "email@example.com", password1: "password1", password2: "password2"}
  printResult v2

  log "\n\n"

  v3 ← runValidation signupForm {email: "email@example.com", password1: "password921", password2: "password921"}
  printResult v3
