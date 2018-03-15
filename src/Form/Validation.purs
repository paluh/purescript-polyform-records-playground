module Form.Validation where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random (random, RANDOM)
import Data.Array (any, elem)
import Data.String (Pattern(Pattern), contains, length, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Polyform.Validation (V(Invalid), Validation)
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
  => Validation m (Array (Variant (malformed :: String | err))) String String
malformed = Validation.hoistFnV \str →
  if contains (Pattern "@") str
    then pure str
    else Invalid [ inj (SProxy :: SProxy "malformed") str ]

-- This validator mimics checking a database to ensure an email is not already used; it
-- does that using a coin toss. Because of that, we're no longer in any monad; we need
-- to be in a monad supporting Eff and the RANDOM effect type.
--
-- Otherwise, this validator looks similar to the previous one. We define a new error
-- variant, we take a string as input, and we return a validated string.
inUse :: ∀ m eff err
  . MonadEff (random :: RANDOM | eff) m
 => Validation m (Array (Variant (inUse :: String | err))) String String
inUse = Validation.hoistFnMV \str -> do
  v <- liftEff random
  pure (pure str)

-- These validators ensure that an input string is within some length. It shows how
-- you can create validators that rely on arguments to determine how they behave. It also
-- demonstrates how variants can carry extra information to help you diagnose errors: in
-- this case, our variant shows the string input and the value it failed to be greater than.
tooShort :: ∀ m err
  . Monad m
 => Int
 -> String
 -> Validation m (Array (Variant (tooShort :: Tuple Int String | err))) String String
tooShort min text = Validation.hoistFnV \str ->
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
