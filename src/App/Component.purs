module App.Component where

import Prelude

import Data.Newtype (unwrap)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (head)
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (case_, match, on)
import Form (FormFieldValue, FormFieldValidate, Email, Field(PasswordField, EmailField), RawForm, signupForm)
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
  { form :: RawForm
  , formErrors :: Array String
  , formFields :: Array Field
  , formValue :: Maybe { email :: Email, password :: String } }

_form :: ∀ t r. Lens' { form :: t | r } t
_form = prop (SProxy :: SProxy "form")

_value :: ∀ t r. Lens' { value :: t | r } t
_value = prop (SProxy :: SProxy "value")

_validate :: ∀ t r. Lens' { validate :: t | r } t
_validate = prop (SProxy :: SProxy "validate")

_password1 = SProxy :: SProxy "password1"
_password2 = SProxy :: SProxy "password2"
_email = SProxy :: SProxy "email"

updateValidate :: FormFieldValidate -> (State -> State)
updateValidate = match
  { password1: set $ _form <<< prop _password1 <<< _validate
  , password2: set $ _form <<< prop _password2 <<< _validate
  , email: set $ _form <<< prop _email <<< _validate
  }

updateValue :: FormFieldValue -> (State -> State)
updateValue = match
  { password1: set $ _form <<< prop _password1 <<< _value
  , password2: set $ _form <<< prop _password2 <<< _value
  , email: set $ _form <<< prop _email <<< _value
  }

component :: ∀ eff m. MonadAff ( console :: CONSOLE, random :: RANDOM | eff ) m => H.Component HH.HTML Query Unit Void m
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
           Just { email, password } -> HH.code_ [ HH.text $ "Email: " <> (unwrap email), HH.br_, HH.text $ "Password: " <> password ]
        ]
      , HH.ul_
        $ (\e -> HH.li_ [ HH.text e ]) <$> st.formErrors
      ]
    <>
      ( renderField <$> st.formFields )
    )

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
        [ HE.onBlur $ HE.input_ $ ValidateOne (inputValidate true)
        , HE.onValueInput $ HE.input $ UpdateContents <<< inputValue
        ]
      )
  renderField f@(PasswordField { value, label, helpText, inputValidate, inputValue }) =
    formControl
      { helpText: Just helpText
      , validation: case value of
          Invalid err -> match
            { tooShort: \(Tuple i _) -> "This password is too short. It must be more than " <> show i <> " characters."
            , tooLong: \(Tuple i _) -> "This password is too long. It must be less than " <> show i <> " characters."
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
      H.liftAff $ log "Validating..."
      st <- H.get
      (Tuple form value) <- H.liftAff $ do
         v <- runValidation signupForm st.form
         case v of
           Valid form value -> do
             pure (Tuple form value)
           Invalid form -> do
             pure (Tuple form Nothing)
      H.modify _ { formErrors = fst form, formFields = snd form, formValue = value }
      pure next

    UpdateContents f next -> do
      H.modify (updateValue f)
      pure next

    ValidateOne f next -> do
      H.modify (updateValidate f)
      eval $ ValidateAll next


----------
-- Form Control Helper

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
    helpText (Just error) (Just help) =
      HH.span_ [ HH.text error, HH.br_, HH.text help ]
    helpText (Just error) _ =
      HH.span_ [ HH.text error ]
    helpText _ (Just help) =
      HH.span_ [ HH.text help ]
    helpText _ _ =
      HH.text ""

    label x = HH.span_ [ HH.text x ]
