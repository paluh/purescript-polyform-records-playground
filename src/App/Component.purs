module App.Component where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (head, take, drop)
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (match)
import App.SignupForm (Field(..), FormFieldValidate, FormFieldValue, RawForm, _email, _password1, _password2, signupForm)
import Form.Field (_value, _validate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Polyform.Validation (V(..), runValidation)

data Query a
  = UpdateContents FormFieldValue a
  | ValidateOne FormFieldValidate a
  | ValidateAll a

type State =
  { form       :: RawForm
  , formErrors :: Array String
  , formFields :: Array Field
  , formValue  :: Maybe { email :: String, password :: String } }


-- | HELPERS

-- This lens provides access to the `form` field in our state
_form :: ∀ t r. Lens' { form :: t | r } t
_form = prop (SProxy :: SProxy "form")

-- This function will set the password field in our raw form
-- to the value picked up by the PasswordField we rendered
-- in the DOM, saving us from having to have multiple handlers.
setValue sym = set $ _form <<< prop sym <<< _value

updateValue :: FormFieldValue -> (State -> State)
updateValue = match
  { password1: setValue _password1
  , password2: setValue _password2
  , email:     setValue _email
  }

-- This does the same, except it changes the Validate value.
-- We can use it to allow validation after blur events.
setValidate sym = set $ _form <<< prop sym <<< _validate

updateValidate :: FormFieldValidate -> (State -> State)
updateValidate = match
  { password1: setValidate _password1
  , password2: setValidate _password2
  , email:     setValidate _email
  }


component :: ∀ eff m
  . MonadAff ( console :: CONSOLE, random :: RANDOM | eff ) m
 => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action ValidateAll
    , finalizer: Nothing
    }
  where
  initialState :: State
  initialState = { form: initialForm, formErrors: [], formFields: [], formValue: Nothing }

  initialForm :: RawForm
  initialForm =
    { email:     { value: "", validate: false }
    , password1: { value: "", validate: false }
    , password2: { value: "", validate: false }
    }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_
    ( [ HH.h1_
        [ case st.formValue of
           Nothing -> HH.text "Form Not Yet Valid"
           Just { email, password } ->
             HH.code_
             [ HH.text $ "Email: " <> email, HH.br_, HH.text $ "Password: " <> password ]
        ]
      , HH.ul_ $ (\e -> HH.li_ [ HH.text e ]) <$> st.formErrors
      ]
    <>
      -- As long as fields are in the right order, we can simply take/drop and intersperse
      -- other rendering.
      ( renderField <$> take 2 st.formFields )
    <>
      [ HH.h3_ [ HH.br_, HH.text "Some unrelated rendering to fields", HH.br_ ] ]
    <>
      ( renderField <$> drop 2 st.formFields )
    )

  -- We can use this function to decide how to render a given field. Since a "field" can
  -- have an arbitrary number of attributes given to it, we can give a field everything
  -- we need to render it. Here, we've given a 'label' and 'helpText'.
  --
  -- In addition, we can use `match` from Variant to display an error with a custom message
  -- depending on which error has occurred. Plus, because we can access the *data* within
  -- the variant, we can actually use it. Below, we might know the password was too short
  -- and that the validator was set to 5, so we can specifically tell them to have a password
  -- at least that long without hard-coding it.
  renderField :: Field -> H.ComponentHTML Query
  renderField f@(EmailField { value, label, inputValidate, inputValue }) =
    formControl
      { helpText: Nothing
      , validation: case value of
          Invalid err -> match
            { inUse: const "This email address is already in use."
            , malformed: const "This email address is malformed. Perhaps you forgot an '@'?" }
            <$> head err
          Valid _ _ -> Nothing
      , label
      , inputId: label
      }
      ( HH.input
        -- The values we're adding here (true, String) are actually going to update the
        -- correct value in our raw form, and then trigger validation on the whole form.
        [ HE.onBlur $ HE.input_ $ ValidateOne (inputValidate true)
        , HE.onValueInput $ HE.input $ UpdateContents <<< inputValue
        ]
      )
  renderField f@(PasswordField { value, label, helpText, inputValidate, inputValue }) =
    formControl
      { helpText: Just helpText
      , validation: case value of
          Invalid err -> match
            { tooShort: \(Tuple i _) -> "This password is too short. It must be more than "
                                        <> show i
                                        <> " characters."
            , tooLong: \(Tuple i _) -> "This password is too long. It must be less than "
                                        <> show i
                                        <> " characters."
            , missingDigit: const "Passwords must contain at least one digit."
            } <$> head err
          Valid _ _ -> Nothing
      , label
      , inputId: label
      }
      ( HH.input
        [ HE.onBlur $ HE.input_ $ ValidateOne (inputValidate true)
        , HE.onValueInput $ HE.input $ UpdateContents <<< inputValue
        ]
      )

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ValidateAll next -> do
      st <- H.get
      -- When we run validation, we'll get our form back out with any errors, plus
      -- we might get the resulting value if everything parsed fine.
      (Tuple form value) <- H.liftAff $ do
         v <- runValidation signupForm st.form
         case v of
           Valid form value -> do
             pure (Tuple form value)
           Invalid form -> do
             pure (Tuple form Nothing)

      -- This isn't necessary, but I found it convenient to map these to records
      -- in state.
      H.modify _ { formErrors = fst form, formFields = snd form, formValue = value }
      pure next

    UpdateContents val next -> do
      -- This value will neatly update the value of the right input field in state
      -- for the field the user interacted with from our Field type
      H.modify $ updateValue val
      pure next

    ValidateOne val next -> do
      H.modify $ updateValidate val
      eval $ ValidateAll next



-- | VISUAL AID
--
-- We can render blocks out easily.
type FormControlProps =
  { helpText :: Maybe String
  , label :: String
  , validation :: Maybe String
  , inputId :: String
  }

formControl
  :: ∀ p i
  . FormControlProps
  -> HH.HTML p i
  -> HH.HTML p i
formControl props html =
  HH.div_
    [ HH.br_
    , HH.label
      [ HP.for props.inputId ]
      [ label props.label ]
    , HH.div_
      [ html ]
    , helpText props.validation props.helpText
    , HH.br_
    ]
  where
    label x = HH.span_ [ HH.text x ]

    helpText (Just error) (Just help) =
      HH.span_ [ HH.text error, HH.br_, HH.text help ]
    helpText (Just error) _ =
      HH.span_ [ HH.text error ]
    helpText _ (Just help) =
      HH.span_ [ HH.text help ]
    helpText _ _ =
      HH.text ""
