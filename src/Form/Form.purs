module Form where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random (random, RANDOM)
import Data.Array (any, elem)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.String (Pattern(Pattern), contains, length, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Polyform.Validation (V(Invalid, Valid), Validation(Validation))
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
malformed :: ∀ m err
  . Monad m
  => Validation m (Array (Variant (malformed :: String | err))) String Email
malformed = Validation.hoistFnV \str →
  if contains (Pattern "@") str
    then pure $ Email str
    else Invalid [ inj (SProxy :: SProxy "malformed") str ]

-- This validator mimics checking a database to ensure an email is not already used; it
-- does that using a coin toss. Because of that, we're no longer in any monad; we need
-- to be in a monad supporting Eff and the RANDOM effect type.
--
-- Otherwise, this validator looks similar to the previous one. We define a new error
-- variant, we take a string as input, and we return a validated string.
inUse :: ∀ m eff err
  . MonadEff (random :: RANDOM | eff) m
 => Validation m (Array (Variant (inUse :: String | err))) Email Email
inUse = Validation.hoistFnMV \e@(Email str) -> do
  v <- liftEff random
  pure (pure e)

-- These validators ensure that an input string is within some length. It shows how
-- you can create validators that rely on arguments to determine how they behave. It also
-- demonstrates how variants can carry extra information to help you diagnose errors: in
-- this case, our variant shows the string input and the value it failed to be greater than.
tooShort :: ∀ m err
  . Monad m
 => Int
 -> Validation m (Array (Variant (tooShort :: Tuple Int String | err))) String String
tooShort min = Validation.hoistFnV \str ->
  if length str > min
    then pure str
    else Invalid [ inj (SProxy ∷ SProxy "tooShort") (Tuple min str) ]

tooLong :: ∀ m err
  . Monad m
 => Int
 -> Validation m (Array (Variant (tooLong :: Tuple Int String | err))) String String
tooLong max = Validation.hoistFnV \str →
  if length str < max
    then pure str
    else Invalid [ inj (SProxy :: SProxy "tooLong") (Tuple max str) ]

-- Another simple validator; this one ensures that the input contains a digit.
missingDigit :: ∀ m err
  . Monad m
 => Validation m (Array (Variant (missingDigit :: String | err))) String String
missingDigit = Validation.hoistFnV \str →
  let
    chars = toCharArray str
  in
    if any (_ `elem` chars) (toCharArray "0123456789")
      then pure str
      else Invalid [ inj (SProxy :: SProxy "missingDigit") str ]


----------
-- Composed Validators

-- We can freely compose validators. The resulting validator will contain the variant of all possible
-- errors, though composing with (>>>) will short-circuit at the first failed validation.
emailFieldValidation :: ∀ m eff
  . MonadEff (random :: RANDOM | eff) m
 => Validation m (Array EmailError) String Email
emailFieldValidation = malformed >>> inUse

passwordFieldValidation :: ∀ m
  . Monad m
 => Int
 -> Int
 -> Validation m (Array PasswordError) String String
passwordFieldValidation min max = tooShort min >>> tooLong max >>> missingDigit


-- | INPUT
--
-- Your input is the the set of values that you are holding on to in your state
-- so you can run validation on them. You'll generally have a record with all fields
-- in the form. For this form, we'll be working with a few types of DOM inputs, so
-- we'll write a type for them.
--
-- In each case, we want to hold on to the actual value and whether or not we want
-- validation to run on it. Holding this 'validate' flag will let us not validate
-- fields until they've been edited by the user, for example.

type InputValue a = { value :: a, validate :: Boolean }

-- Here's a set of input values in practice.

type FormFieldsT f =
  ( email     :: f String
  , password1 :: f String
  , password2 :: f String
  )
type Id a = a
type FormFields = FormFieldsT Id
type AFormField = Variant FormFields
type K a b = a
type AFormPart = Variant (FormFieldsT (K Boolean))
type RawForm = Record (FormFieldsT InputValue)


-- Your input state is not the same as your form -- though it will contain the raw
-- values that you want to validate! Your input state is just the contents of what
-- the user has input into the DOM. So where does the form come in?


-- | FIELDS

-- Forms are made up of fields. At minimum, a field on your form will need to carry the list
-- of possible validation errors that can occur in it, and the value that successful validation
-- should result in.
--
-- However, since we're going to use these fields to render our form in the DOM, we're going
-- to want to carry extra information for rendering.
--
-- In our case, we'll define a FieldValue type. It will hold on to a value of type V:
--
-- data V e a = Invalid e | Valid e a
--
-- If the field fails to validate, we'll get an Invalid with an array of errors. If it validates,
-- we'll get a Valid with (why the e?) and the successful value.

type FieldValue attrs err a =
  { value :: V (Array err) a
  , aFormField :: a -> AFormField
  , aFormPart :: Boolean -> AFormPart
  | attrs }

-- Now, our form will be made up of lots of fields, and these fields can have all sorts of different
-- FieldValue types. We need to be able to pass any kind of field into a form, and so we'll wrap them
-- up in a sum type. Then, our form can be made up of any of these field types.
--
-- Our form has two distinct field types: email and password. We'll newtype emails to demonstrate
-- validation has succeeded, but leave passwords as strings.

newtype Email = Email String

-- While we haven't yet actually written our validation, I'll assume we already know how our fields can
-- fail. An email address can fail because it's malformed or because we checked our database and it's
-- already in use. A password can fail because it's too long, too short, or doesn't have a number in it.

type EmailError = Variant ( malformed :: String, inUse :: String )
type PasswordError = Variant ( missingDigit :: String, tooShort :: Tuple Int String, tooLong :: Tuple Int String )

-- Now, we're ready to create that sum type with our fields. Since we defined FieldValue to allow any
-- extra attributes we want, we can provide them now. We'll give labels to both field types, and we'll
-- make sure passwords get help text.

data Field
  = EmailField    (FieldValue (label :: String) EmailError String)
  | PasswordField (FieldValue (label :: String, helpText :: String) PasswordError String)

-- | FORMS

-- Our form is going to be a simple tuple: an array of strings representing
-- form-level errors and an array of fields in the form.

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
formFromField :: ∀ m attrs input value output err
  . Monad m
  => Monoid err
  => ( input -> { value :: value, validate :: Boolean } )  -- From our input (state), retrieve the relevant field
  -> ( { value :: V err value | attrs } -> Field )         -- Given an Input, construct a Field (use our data constructors)
  -> { value :: V err value | attrs }                      -- The starting state of the field, with its attributes
  -> Validation m err value output                         -- A validation to run on the input value, returning an array of errors
  -> Validation m Form input (Maybe output)                -- A new validation ready to produce a full form
formFromField accessor constructor defaultInput fieldValidation =
  Validation $ \inputRecord → do
    -- Retrieve the correct input value using the accessor (ex: _.email)
    let { value, validate } = accessor inputRecord
    -- Run field validation against the value
    if validate
      then do
        r <- Validation.runValidation fieldValidation value
        -- Based on the result of the validation, we'll return either...
        pure $ case r of
          -- The form along with the result value, so we can combine both into
          -- larger values and forms. To do this, we'll update the value of the provided
          Valid e a → Valid ( Tuple [] [ constructor $ defaultInput { value = Valid e value } ] ) ( Just a )
          -- Or the form as a representation of our error, which can then be combined
          -- with other forms.
          Invalid e -> Invalid $ Tuple [] [ constructor $ defaultInput { value = Invalid e } ]
      else
        pure $ Valid (Tuple [] [ constructor $ defaultInput { value = Valid mempty value } ]) Nothing

-- Here, we create an single-field form that expects an input record with an 'email'
-- field we can retrieve for validation.
buildEmailForm :: ∀ r m eff
  . MonadEff (random :: RANDOM | eff) m
 => FieldValue (label :: String) EmailError String
 -> Validation m Form { email :: InputValue String | r } (Maybe Email)
buildEmailForm v = formFromField _.email EmailField v emailFieldValidation

-- This function takes its accessor as an argument, but otherwise works the same as
-- the email form.
buildPasswordForm :: ∀ r m
  . Monad m
 => (r -> { value :: String, validate :: Boolean })
 -> FieldValue (label :: String, helpText :: String) PasswordError String
 -> Validation m Form r (Maybe String)
buildPasswordForm accessor v = formFromField accessor PasswordField v (passwordFieldValidation 5 50)

-- This form, composed of two smaller single-field forms, works to validate two passwords.
passwordForm :: ∀ m r. Monad m => Validation m Form ({ password1 :: InputValue String, password2 :: InputValue String | r }) (Maybe String)
passwordForm
  = (lift2 {password1: _, password2: _} <$> (buildPasswordForm _.password1 pass1) <*> (buildPasswordForm _.password2 pass2))
  -- Here we are composing validations, so the previous step results are
  -- inputs for this next step. It's a usual validation like our others.
  -- We can always fail here and return a form representing our error, which
  -- will be appended to the whole form.
  >>> Validation.hoistFnV (case _ of
    Nothing → pure Nothing
    Just { password1, password2 } →
      if password1 == password2
        then pure (Just password1)
        else Invalid $ Tuple [ "Password dont match" ] [])
  where
    _password1 = SProxy :: SProxy "password1"
    _password2 = SProxy :: SProxy "password2"
    pass1 = { value: Valid [] "", aFormField: inj _password1, aFormPart: inj _password1, helpText: "Here is some help text for the password field.", label: "Password 1" }
    pass2 = { value: Valid [] "", aFormField: inj _password2, aFormPart: inj _password2, helpText: "Enter your password again", label: "Password 2" }

-- We can continue composing into bigger and bigger forms. This one expects
-- an input record with at least two password fields and an email field on it,
-- and it will validate to a different type altogether: a record with a password
-- and email field.
signupForm :: ∀ m eff
  . MonadEff (random :: RANDOM | eff) m
 => Validation m Form RawForm (Maybe { password :: String, email :: Email })
signupForm = lift2 { password: _, email: _ } <$> passwordForm <*> emailForm
  where
    _email = SProxy :: SProxy "email"
    emailForm = buildEmailForm { value: Valid [] "", aFormField: inj _email, aFormPart: inj _email, label: "Email" }
