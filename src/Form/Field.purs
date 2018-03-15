module Form.Field where

import Polyform.Validation (V)

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

type FieldValue attrs vl vd err a =
  { value         :: V (Array err) a
  , inputValue    :: a -> vl
  , inputValidate :: Boolean -> vd
  | attrs }

type Id a = a
type K a b = a
