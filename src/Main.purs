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
import Polyform.Validation (V(..), Validation(..))
import Polyform.Validation as Validation
import Type.Prelude (SProxy(..))

-- | Let's assume that our fields are really simple
-- | and contain only validation result
type Input err value = V (Array err) value

-- | Let's define some simple validators for email field

emailFormat = Validation.hoistFnV \e →
  if contains (Pattern "@") e
    then Invalid [inj (SProxy ∷ SProxy "emailFormat") e]
    else pure e

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
  | PasswordField (Input (Variant (hasDigit ∷ String, maxLength ∷ Tuple String Int, minLength ∷ Tuple String Int)) String)



-- | Form types and form related helpers and validations

type Form = Tuple (Array String) (Array Field)

-- | Let's build our form without any external helpers
-- | This function builds single field form
-- | given field validation.
fieldForm fetchValue fieldValidation =
  Validation $ \inputRecord → do
    let inputValue = fetchValue inputRecord
    r ← Validation.runValidation fieldValidation inputValue
    pure $ case r of
      Valid e v → Valid (Tuple [] [Valid e v]) v
      Invalid e → Invalid (Tuple [] [Invalid e])

emailForm = fieldForm (_.email) emailFieldValidation

buildPasswordForm fetch = fieldForm fetch (passwordFieldValidation 5 50)

passwordForm
  = ({password1: _, password2: _} <$> (buildPasswordForm _.password1) <*> (buildPasswordForm _.password2))
  -- | we are operating here on form component level
  >>> Validation.hoistFn \{ password1, password2 } →
    if password1 /= password2
      then Invalid (Tuple ["Password dont match"] [])
      else pure password1

signupForm = {password: _, email: _} <$> passwordForm <*> emailForm

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
