module App.SignupForm where

import Prelude

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Random (RANDOM)
import Form (formFromField)
import Form.Validation (inUse, malformed, missingDigit, tooLong, tooShort)
import Form.Field (FieldValue, InputValue, Id, K)
import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Polyform.Validation (V(..), Validation)
import Polyform.Validation as Validation
import Type.Prelude (SProxy(..))

-- | THE FORM
--
-- I'll skip to the end. Our ultimate result is a signup form created by composing two
-- other forms together. It will result in { password, email } even though there are
-- more input fields than that.

signupForm = lift2 { password: _, email: _ } <$> passwordForm <*> emailForm

-- | These are the fields we want in our form:
type FormFieldsT f =
  ( email     :: f String
  , password1 :: f String
  , password2 :: f String
  )

-- | They can be identified with these variants:
_password1 = SProxy :: SProxy "password1"
_password2 = SProxy :: SProxy "password2"
_email     = SProxy :: SProxy "email"

-- | We can access the 'value' part of each input like this:
type FormFieldValue = Variant (FormFieldsT Id)

-- | and the 'validation' part like this:
type FormFieldValidate = Variant (FormFieldsT (K Boolean))

-- | and we can generate our raw form like this:
type RawForm = Record  (FormFieldsT InputValue)

-- | Our form will be made up of fields of these types:
data Field
  = EmailField
    (FieldValue (label :: String) FormFieldValue FormFieldValidate EmailError String)
  | PasswordField
    (FieldValue (label :: String, helpText :: String) FormFieldValue FormFieldValidate PasswordError String)


-- | FORM CONSTRUCTORS

-- We can make a helper function for making an email form like this:
buildEmailForm v = formFromField _.email EmailField v emailFieldValidation

-- Same for password forms:
buildPasswordForm accessor v = formFromField accessor PasswordField v (passwordFieldValidation 5 50)


-- | FORM PARTS

-- This is now a legitimate single-field form
emailForm = buildEmailForm
  { value: Valid [] ""
  , inputValue: inj _email
  , inputValidate: inj _email
  , label: "Email" }

-- | This is a legitimate two-field form
passwordForm = (
      lift2 {password1: _, password2: _}
  <$> buildPasswordForm _.password1 pass1
  <*> buildPasswordForm _.password2 pass2
  )
  >>> Validation.hoistFnV (case _ of
    Nothing -> pure Nothing
    Just { password1, password2 } ->
      if password1 == password2
        then pure $ Just password1
        else Invalid $ Tuple [ "Passwords don't match" ] [])
  where
    pass1 = { value: Valid [] ""
            , inputValue: inj _password1
            , inputValidate: inj _password1
            , helpText: "Here is some help text for the password field."
            , label: "Password 1" }
    pass2 = { value: Valid [] ""
            , inputValue: inj _password2
            , inputValidate: inj _password2
            , helpText: "Enter your password again"
            , label: "Password 2" }


-- | Form Validation

type EmailError = Variant
  ( malformed :: String
  , inUse :: String
  )

type PasswordError = Variant
  ( missingDigit :: String
  , tooShort :: Tuple Int String
  , tooLong :: Tuple Int String
  )

emailFieldValidation :: ∀ m eff
  . MonadEff (random :: RANDOM | eff) m
 => Validation m (Array EmailError) String String
emailFieldValidation = malformed >>> inUse

passwordFieldValidation :: ∀ m
  . Monad m
 => Int
 -> Int
 -> Validation m (Array PasswordError) String String
passwordFieldValidation min max =
  tooShort min ( "Passwords must be at least "
               <> show min
               <> " characters long." )
  >>> tooLong max
  >>> missingDigit
