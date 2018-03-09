module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (random, RANDOM)
import Data.Array (any, elem)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.String (Pattern(Pattern), contains, length, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Debug.Trace (traceAnyA)
import Polyform.Validation (V(..), Validation(..), runValidation)
import Polyform.Validation as Validation
import Type.Prelude (SProxy(..))

----------
-- Validation

-- We'll start our form with validation.
--
-- We can define small validators that can be composed together to create a sequence
-- of checks that a field will have to pass to be considered valid.
--
-- Validators have the Validation type:
-- newtype Validation m err input output = Validation (input -> m (V err output))
--
-- which can be run with the `runValidation` function:
-- runValidation :: input output err m. Validation m err input output -> (input -> m (V err output))
--
-- runValidation makes it more clear what's going on. You have a function from some input type
-- to the result of validating that type in some monadic context. For example, you might validate
-- an input integer from a number field to ensure it is greater than 10, returning either errors
-- or the validated integer.
--
-- If your validator doesn't run any effectful computations (like reading from a database) you can
-- leave the monad polymorphic.
--
-- To turn a simple true/false checker into a Validation, you can use the `hoist` functions from the
-- Validation module. If you have a pure computation, use hoistFnV; if you have an effectful one,
-- use hoistFnMV.


-- This validator will check that the input string has the "@" symbol as a naive email validation.
-- Its input type is a String, and it outputs a validated String. There are no effectful computations,
-- so we leave the monad polymorphic and use hoistFnV. Finally, we're going to create a new error variant.
--
-- The variant will contain the string that failed, so we can easily inspect the reason for failure.
emailFormat :: ∀ m err
  . Monad m
  => Validation m (Array (Variant (emailFormat :: String | err))) String String
emailFormat = Validation.hoistFnV \str →
  if contains (Pattern "@") str
    then pure str
    else Invalid [ inj (SProxy :: SProxy "emailFormat") str ]

-- This validator mimics checking a database to ensure an email is not already used; it
-- does that using a coin toss. Because of that, we're no longer in any monad; we need
-- to be in a monad supporting Eff and the RANDOM effect type.
--
-- Otherwise, this validator looks similar to the previous one. We define a new error
-- variant, we take a string as input, and we return a validated string.
emailIsUsed :: ∀ m eff err
  . MonadEff (random :: RANDOM | eff) m
 => Validation m (Array (Variant (isUsed :: String | err))) String String
emailIsUsed = Validation.hoistFnMV \str -> do
  v <- liftEff random
  pure (pure str)
  -- LET"S DROP TOSSING NOW TO SIMPLIFY DEBUGING :-)
  -- $ if v > 0.5
  --  then pure str
  --  else Invalid [ inj (SProxy :: SProxy "isUsed") str ]

-- These validators ensure that an input string is within some length. It shows how
-- you can create validators that rely on arguments to determine how they behave. It also
-- demonstrates how variants can carry extra information to help you diagnose errors: in
-- this case, our variant shows the string input and the value it failed to be greater than.
minLength :: ∀ m err
  . Monad m
 => Int
 -> Validation m (Array (Variant (minLength :: Tuple Int String | err))) String String
minLength min = Validation.hoistFnV \str ->
  if length str > min
    then pure str
    else Invalid [ inj (SProxy ∷ SProxy "minLength") (Tuple min str) ]

maxLength :: ∀ m err
  . Monad m
 => Int
 -> Validation m (Array (Variant (maxLength :: Tuple Int String | err))) String String
maxLength max = Validation.hoistFnV \str →
  if length str < max
    then pure str
    else Invalid [ inj (SProxy :: SProxy "maxLength") (Tuple max str) ]

-- Another simple validator; this one ensures that the input contains a digit.
hasDigit :: ∀ m err
  . Monad m
 => Validation m (Array (Variant (hasDigit :: String | err))) String String
hasDigit = Validation.hoistFnV \str →
  let
    chars = toCharArray str
  in
    if any (_ `elem` chars) (toCharArray "0123456789")
      then pure str
      else Invalid [ inj (SProxy :: SProxy "hasDigit") str ]


----------
-- Composed Validators

-- We can freely compose validators. The resulting validator will contain the variant of all possible
-- errors, though composing with (>>>) will short-circuit at the first failed validation.
emailFieldValidation :: ∀ m eff err
  . MonadEff (random :: RANDOM | eff) m
 => Validation m (Array (Variant (emailFormat :: String, isUsed :: String | err))) String String
emailFieldValidation = emailFormat >>> emailIsUsed

passwordFieldValidation :: ∀ m err
  . Monad m
 => Int
 -> Int
 -> Validation m (Array (Variant (hasDigit :: String, maxLength :: Tuple Int String, minLength :: Tuple Int String | err))) String String
passwordFieldValidation min max = maxLength max >>> minLength min >>> hasDigit


----------
-- Fields

-- Forms are made up of fields. A given form will generally be a sum type of all the fields
-- in the form; each field will carry with it at least the information necessary for validation,
-- but you'll usually also want to carry some extra attributes you can use for rendering. For
-- example, you might also want to carry an id, label text, set of CSS classes, or other information.
--
-- Below we've defined a simple input type we can use for validation
-- which can carry information for rendering as extra fields.
--
-- An input has at least a "value", which is either an array of validation
-- errors, or a successful value, but can carry other fields too.
--
-- data V e a = Invalid e | Valid e a

type Input attrs err a = { value :: V (Array err) a | attrs }

-- Our actual fields will be a sum type of possiblefield types on the form, where each one is our Input type
-- wrapped in a constructor.

data Field
  = EmailField    (Input () (Variant (emailFormat :: String, isUsed :: String)) String)
  | PasswordField (Input () (Variant (hasDigit :: String, maxLength :: Tuple Int String, minLength :: Tuple Int String)) String)

-- We need to define default values here (not 100% sure about a better way to handle this)

defaultInputString :: ∀ err. Input () (Variant err) String
defaultInputString =
  { value: Valid [] "" }


----------
-- Form types and form-related helpers

-- This is our form type; when you see Tuple below it means that we are
-- building a Form. The first position contains form-level errors;
-- the second contains our fields, which each may contain field-level
-- errors.
type Form = Tuple (Array String) (Array Field)

-- Let's build our form without any external helpers
-- This function builds a single-field form from:
--  * record accessor function
--  * field type constructor
--  * field validation
--
-- Here we can also observe that validation is
-- nothing more than function from input to V
-- in monadic context.
formFromField :: ∀ m attrs record input output err
  . Monad m
  => Monoid err
  => (record -> {value ∷ input, validate ∷ Boolean})
  -> ({ value :: V err input | attrs } -> Field)
  -> { value :: V err input | attrs }
  -> Validation m err input output
  -> Validation m Form record (Maybe output)
formFromField accessor constructor record fieldValidation =
  Validation $ \inputRecord → do
    -- Retrieve the correct input value using the accessor (ex: _.email)
    let {value, validate} = accessor inputRecord
    -- Run field validation against the value
    if validate
      then do
        r <- Validation.runValidation fieldValidation value
        -- Based on the result of the validation, we'll return either...
        pure $ case r of
          -- The form along with the result value, so we can combine both into
          -- larger values and forms. To do this, we'll update the value of the provided
          Valid e a → Valid (Tuple [] [ constructor $ record { value = Valid e value } ]) (Just a)
          -- Or the form as a representation of our error, which can then be combined
          -- with other forms.
          Invalid e -> Invalid $ Tuple [] [ constructor $ record { value = Invalid e } ]
      else
        pure $ Valid (Tuple [] [ constructor $ record { value = Valid mempty value } ]) Nothing


-- Here, we create an single-field form that expects an input record with an 'email'
-- field we can retrieve for validation.
emailForm :: ∀ r m eff
  . MonadEff (random :: RANDOM | eff) m
  => Validation m Form { email :: { value :: String, validate :: Boolean } | r } (Maybe String)
emailForm = formFromField _.email EmailField defaultInputString emailFieldValidation

-- This function takes its accessor as an argument, but otherwise works the same as
-- the email form.
buildPasswordForm :: ∀ r m
  . Monad m
 => (r -> { value ∷ String, validate ∷ Boolean })
 -> Validation m Form r (Maybe String)
buildPasswordForm accessor = formFromField accessor PasswordField defaultInputString (passwordFieldValidation 5 50)

-- | I should have define this earlier ;-)
type StringValue = { value ∷ String, validate ∷ Boolean }

-- This form, composed of two smaller single-field forms, works to validate two passwords.
passwordForm :: ∀ m r. Monad m => Validation m Form ({ password1 :: StringValue, password2 :: StringValue | r }) (Maybe String)
passwordForm
  = (lift2 {password1: _, password2: _} <$> (buildPasswordForm _.password1) <*> (buildPasswordForm _.password2))
  -- -- Here we are composing validations, so the previous step results are
  -- -- inputs for this next step. It's a usual validation like our others.
  -- -- We can always fail here and return a form representing our error, which
  -- -- will be appended to the whole form.
  >>> Validation.hoistFnV (case _ of
    Nothing → pure Nothing
    Just { password1, password2 } →
      if password1 == password2
        then pure (Just password1)
        else Invalid $ Tuple [ "Password dont match" ] [])

-- We can continue composing into bigger and bigger forms. This one expects
-- an input record with at least two password fields and an email field on it,
-- and it will validate to a different type altogether: a record with a password
-- and email field.
signupForm :: ∀ r m eff
  . MonadEff (random :: RANDOM | eff) m
 => Validation m Form { password1 :: StringValue, password2 :: StringValue, email :: StringValue | r } (Maybe { password :: String, email :: String })
signupForm = lift2 {password: _, email: _} <$> passwordForm <*> emailForm


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

validate value = { value, validate: true }
notValidate value = { value, validate: false }

main :: ∀ eff. Eff (console :: CONSOLE, random :: RANDOM | eff) Unit
main = do
  log "EXAMPLE"

  -- | Nothing is validated - we are getting FORM VALID but without any result ;-)
  v1 <- runValidation signupForm {email: notValidate "wrongemailformat", password1: notValidate "shrt", password2: notValidate "nodigits"}
  printResult v1

  -- | Only first field is validated but wrong
  v1 <- runValidation signupForm {email: validate "wrongemailformat", password1: notValidate "shrt", password2: notValidate "nodigits"}
  printResult v1

  -- | Only first field is validated and it is ok - result is still Nothing
  v1 <- runValidation signupForm {email: validate "email@example.com", password1: notValidate "shrt", password2: notValidate "nodigits"}
  printResult v1

  -- | First two fields are validated
  v1' <- runValidation signupForm {email: validate "wrongemailformat", password1: validate "shrt", password2: notValidate "nodigits"}
  printResult v1'

  -- | All fields are validated
  v1' <- runValidation signupForm {email: validate "wrongemailformat", password1: validate "shrt", password2: validate "nodigits"}
  printResult v1'


  -- | Form is really validated and valid
  v3 <- runValidation signupForm {email: validate "email@example.com", password1: validate "password921", password2: validate "password921"}
  printResult v3

  -- log "\n\n"

  -- v2 <- runValidation signupForm {email: "email@example.com", password1: "password1", password2: "password2"}
  -- printResult v2

  -- log "\n\n"

  -- v3 <- runValidation signupForm {email: "email@example.com", password1: "password921", password2: "password921"}
  -- printResult v3
