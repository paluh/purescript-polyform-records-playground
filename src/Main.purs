module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (random, RANDOM)
import Data.Array (any, elem)
import Data.String (Pattern(Pattern), contains, length, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Debug.Trace (traceAnyA)
import Polyform.Validation (V(..), Validation(..), runValidation)
import Polyform.Validation as Validation
import Type.Prelude (SProxy(..))


-- This represents a basic input field that can be used for validation
-- and also carries information necessary for rendering.
--
-- data V e a = Invalid e | Valid a
--
-- An input will
-- only consists of the validation and result:
-- | Let's assume that our fields are really simple
-- | and contain only validation result.
-- | Errors are kept in `Array`.
type Input attrs err value = { value :: V (Array err) value | attrs }

-- | Let's define some simple validators for email field
-- | ...of course they are really dummy validators ;-)

emailFormat :: ∀ m err
  . Monad m
  => Validation m (Array (Variant (emailFormat :: String | err))) String String
emailFormat = Validation.hoistFnV \e →
  if contains (Pattern "@") e
    then pure e
    else Invalid [inj (SProxy ∷ SProxy "emailFormat") e]

emailIsUsed :: ∀ m eff err
  . MonadEff (random :: RANDOM | eff) m
 => Validation m (Array (Variant (isUsed :: String | err))) String String
emailIsUsed = Validation.hoistFnMV \email -> do
  -- | Some effectful computation inside your monad.
  -- | Let's toss a coin instead of checking db
  -- | if email is really used>
  v ← liftEff random
  pure $ if v > 0.5
    then Invalid [ inj (SProxy ∷ SProxy "isUsed") email ]
    else pure email

emailFieldValidation :: ∀ m eff err
  . MonadEff (random :: RANDOM | eff) m
 => Validation m (Array (Variant (emailFormat :: String, isUsed :: String | err))) String String
emailFieldValidation = emailFormat *> emailIsUsed

-- | Let's define some simple validators for password field.

minLength :: ∀ m err
  . Monad m
 => Int
 -> Validation m (Array (Variant (minLength :: Tuple Int String | err))) String String
minLength m = Validation.hoistFnV \p →
  if length p < m
    then Invalid [inj (SProxy ∷ SProxy "minLength") (Tuple m p)]
    else pure p

maxLength :: ∀ m err
  . Monad m
 => Int
 -> Validation m (Array (Variant (maxLength :: Tuple Int String | err))) String String
maxLength m = Validation.hoistFnV \p →
  if length p > m
    then Invalid [ inj (SProxy ∷ SProxy "maxLength") (Tuple m p) ]
    else pure p

hasDigit :: ∀ m err
  . Monad m
 => Validation m (Array (Variant (hasDigit :: String | err))) String String
hasDigit = Validation.hoistFnV \p →
  let
    chars = toCharArray p
  in
    if any (_ `elem` chars) (toCharArray "0123456789")
      then pure p
      else Invalid [ inj (SProxy ∷ SProxy "hasDigit") p ]

passwordFieldValidation :: ∀ m err
  . Monad m
 => Int
 -> Int
 -> Validation m (Array (Variant (hasDigit :: String, maxLength :: Tuple Int String, minLength :: Tuple Int String | err))) String String
passwordFieldValidation min max =
  maxLength max *> minLength min *> hasDigit

data Field
  = EmailField (Input () (Variant (emailFormat ∷ String, isUsed ∷ String)) String)
  | PasswordField (Input () (Variant (hasDigit ∷ String, maxLength ∷ Tuple Int String, minLength ∷ Tuple Int String)) String)

defaultEmailField :: Input () (Variant (emailFormat :: String, isUsed :: String)) String
defaultEmailField =
  { value: Valid [] "" }

defaultPasswordField :: Input () (Variant (hasDigit :: String, maxLength :: Tuple Int String, minLength :: Tuple Int String)) String
defaultPasswordField =
  { value: Valid [] "" }

-- | Form types and form related helpers and validations

-- | This is our form type so when you see Tuple
-- | below it means that we are building Form.
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
formFromField :: ∀ m attrs record input output err
  . Monad m
 => (record -> input)
 -> ({ value :: V err output | attrs } -> Field)
 -> { value :: V err output | attrs }
 -> Validation m err input output
 -> Validation m Form record output
formFromField accessor constructor default fieldValidation =
  Validation $ \inputRecord → do
    -- | Fetch field value from record using fetcher
    let inputValue = accessor inputRecord
    -- | Run field validation agains this value
    r ← Validation.runValidation fieldValidation inputValue
    -- | Based on field validation result let's return:
    pure $ case r of
      -- | form togheter with result value
      -- | so we can combine both into larger values and forms
      Valid e v → Valid (Tuple [] [ constructor $ default { value = (Valid e v) } ]) v
      -- | or form as representation of our error which
      -- | can be combined with other forms
      Invalid e → Invalid (Tuple [] [ constructor $ default { value = (Invalid e) } ])

emailForm :: ∀ r m eff
  . MonadEff (random :: RANDOM | eff) m
 => Validation m Form { email :: String | r } String
emailForm = formFromField _.email EmailField defaultEmailField emailFieldValidation

buildPasswordForm :: ∀ r m
  . Monad m
 => (r -> String)
 -> Validation m Form r String
buildPasswordForm accessor = formFromField accessor PasswordField defaultPasswordField (passwordFieldValidation 5 50)

passwordForm :: ∀ m r. Monad m => Validation m Form { password1 :: String, password2 :: String | r } String
passwordForm
  = ({password1: _, password2: _} <$> (buildPasswordForm _.password1) <*> (buildPasswordForm _.password2))
  -- | Here we are composing validations
  -- | so previous step results
  -- | (record with email and passwords)
  -- | are input for this next step.
  -- |
  -- | We can always fail here and return
  -- | form representing our failure
  -- | which will be appended to the
  -- | whole form.
  >>> Validation.hoistFnV \{ password1, password2 } →
    if password1 /= password2
      then Invalid (Tuple ["Password dont match"] [])
      else pure password1

signupForm :: ∀ r m eff
  . MonadEff (random :: RANDOM | eff) m
 => Validation m Form { password1 :: String, password2 :: String, email :: String | r } { password :: String, email :: String }
signupForm = {password: _, email: _} <$> passwordForm <*> emailForm

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

main :: ∀ eff. Eff (console :: CONSOLE, random :: RANDOM | eff) Unit
main = do
  log "EXAMPLE"

  v1 <- runValidation signupForm {email: "wrongemailformat", password1: "shrt", password2: "nodigits"}
  printResult v1

  log "\n\n"

  v2 <- runValidation signupForm {email: "email@example.com", password1: "password1", password2: "password2"}
  printResult v2

  log "\n\n"

  v3 <- runValidation signupForm {email: "email@example.com", password1: "password921", password2: "password921"}
  printResult v3
