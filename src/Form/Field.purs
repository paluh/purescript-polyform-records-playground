module Form.Field where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Polyform.Validation (V)

-- | INPUT
--
-- Your input is the the set of values that you are holding on to in your state
-- so you can run validation on them. You'll generally have a record with all fields
-- in the form. A simple input type for most forms would contain just the value
-- coming off the DOM and whether or not to run validation on it.
--
-- NOTE: You can generate a record of input values from your fields! This is the most
-- reliable way to prevent duplication.
type InputValue a = { value :: a, validate :: Boolean }

-- These lenses provide access to input values.
_value :: ∀ t r. Lens' { value :: t | r } t
_value = prop (SProxy :: SProxy "value")

_validate :: ∀ t r. Lens' { validate :: t | r } t
_validate = prop (SProxy :: SProxy "validate")


-- | FIELD
--
-- Fields are the smallest building blocks of forms. They can be turned into forms
-- using `formFromField` from the `Form` module. They at least have to contain a
-- validation result, `V`:
--
-- data V e a = Invalid e | Valid e a
--
-- Fields are what you will actually render onto the page.
--
-- `attrs` - Some arbitrary list of more fields you'd like in this record.
--           Useful for things like including CSS classes.
-- `vl`    - Stands for 'Value` and specifies the symbol proxy used to set
--           the 'value' field for the corresponding InputValue type for
--           this field. See the `SignupForm` module for examples.
-- `vd`    - Stands for `Validate` and specifies the symbol proxy used to
--           set the 'validate' field for the correspodning InputValue type
--           for this field. See the `SignupForm` module for examples.
-- `err`   - The variant of possible validation errors for this field
-- `a`     - The value the corresponding InputValue type for this field has
--
-- The value field holds either your errors or some resulting value. The
-- inputValue function allows you to take some input of the right type for
-- the corresponding input field and set its value. The inputValidate field
-- does the same thing, but rather than being any value, it can only be
-- a true/false value. See `SignupForm` for this in use.
type FieldValue attrs vl vd err a =
  { value         :: V (Array err) a
  , inputValue    :: a -> vl
  , inputValidate :: Boolean -> vd
  | attrs }

-- These two types are helpers for the inputValue and inputValidate functions.
-- See `SignupForm` for them in use.
type Id a = a
type K a b = a
