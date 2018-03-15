module Form where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Polyform.Validation (V(Invalid, Valid), Validation(Validation))
import Polyform.Validation as Validation


-- | FORMS

-- Our form is going to be a simple tuple: an array of strings representing
-- form-level errors and an array of fields in the form.

type Form field = Tuple (Array String) (Array field)

-- Let's build our form without any external helpers
-- This function builds a single-field form from:
--  * record accessor function
--  * field type constructor
--  * field validation
--
-- Here we can also observe that validation is
-- nothing more than function from input to V
-- in monadic context.
formFromField :: ∀ m attrs input value output err field
  . Monad m
  => Monoid err
  => ( input -> { value :: value, validate :: Boolean } )  -- From our input (state), retrieve the relevant field
  -> ( { value :: V err value | attrs } -> field )         -- Given an Input, construct a Field (use our data constructors)
  -> { value :: V err value | attrs }                      -- The starting state of the field, with its attributes
  -> Validation m err value output                         -- A validation to run on the input value, returning an array of errors
  -> Validation m (Form field) input (Maybe output)                -- A new validation ready to produce a full form
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
