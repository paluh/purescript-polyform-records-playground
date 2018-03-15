module Form where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Polyform.Validation (V(Invalid, Valid), Validation(Validation))
import Polyform.Validation as Validation

-- | FORMS

-- The simplest way to represent a form is a tuple. Its first inhabitant
-- is an array of strings representing form-level errors; its second is
-- an array of fields in the form. All of these are monoids, which allows
-- you to build up successively larger forms with applicative syntax.

type Form field = Tuple (Array String) (Array field)

-- | CONSTRUCTING A FORM FROM A FIELD

-- If you haven't reviewed the Validation and Field modules in this repo,
-- I'd recommend going through those first. If you already know them,
-- carry on.
--
-- This function will run on your input and produce a Form along with some
-- resulting value or some errors. It turns a single "field" into a form
-- that can be composed with other forms.
--
-- `m`       - Some monadic context the form can run in (like Aff for async calls)
-- `attrs`   - Some arbitrary attributes you've given to the InputField type
-- `rawform` - The raw form that serves as the input for this form
-- `a`       - The value of the field in the raw form, which will be transformed
-- `output`  - The value the field should parse into, if successful
-- `err`     - The variant of possible validation errors
-- `field`   - The type of fields in your form (see the Field module)
--
-- This function expects a record accessor that tells it which field to get
-- from the input; a data constructor that will turn that field into your
-- custom Field type; a default starting value for that field; and a validation
-- that the field will have to pass in order to parse and succeed.
--
-- The end result is a validation ready to run on the raw form and produce
-- some valid output or some collection of errrors.
formFromField :: ∀ m attrs rawform a output err field
  . Monad m
  => Monoid err
  => (rawform -> { value :: a, validate :: Boolean })
  -> ({ value :: V err a | attrs } -> field)
  -> { value :: V err a | attrs }
  -> Validation m err a output
  -> Validation m (Form field) rawform (Maybe output)
formFromField accessor constructor defaultInput fieldValidation =
  Validation $ \inputRecord → do
    -- Retrieve the correct input value using the accessor (ex: _.email)
    let { value, validate } = accessor inputRecord
    -- Run the field validation against the value
    if validate
      then do
        r <- Validation.runValidation fieldValidation value
        -- Based on the result of the validation, we'll return either...
        pure $ case r of
          -- The form along with the result value, so we can combine both into
          -- larger values and forms...
          Valid e a ->
            Valid
              (Tuple [] [ constructor $ defaultInput { value = Valid e value } ])
              (Just a)
          -- ...or the form as a representation of our error, which can then be combined
          -- with other forms.
          Invalid e ->
            Invalid
              (Tuple [] [ constructor $ defaultInput { value = Invalid e } ])
      else pure $
        Valid
          (Tuple [] [ constructor $ defaultInput { value = Valid mempty value } ])
          Nothing
