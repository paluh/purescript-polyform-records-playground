module App.Component where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (head)
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Record (modify)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (case_, on)
import Form (AFormField, Email, Field(PasswordField, EmailField), RawForm, AFormPart, signupForm)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Polyform.Validation (V(..), runValidation)

data Query a
  = ValidateOne AFormPart a
  | ValidateAll a
  | UpdateContents AFormField a

type State =
  { form :: RawForm
  , formErrors :: Array String
  , formFields :: Array Field
  , formValue :: Maybe { email :: Email, password :: String } }

_form :: forall t r. Lens' { form :: t | r } t
_form = prop (SProxy :: SProxy "form")
_value :: forall t r. Lens' { value :: t | r } t
_value = prop (SProxy :: SProxy "value")
_validate :: forall t r. Lens' { validate :: t | r } t
_validate = prop (SProxy :: SProxy "validate")

doUpdate :: AFormField -> (State -> State)
doUpdate = case_
  # on (SProxy :: SProxy "password1") (set (_form <<< prop (SProxy :: SProxy "password1") <<< _value))
  # on (SProxy :: SProxy "password2") (set (_form <<< prop (SProxy :: SProxy "password2") <<< _value))
  # on (SProxy :: SProxy "email") (set (_form <<< prop (SProxy :: SProxy "email") <<< _value))

doUpdate' :: AFormPart -> (State -> State)
doUpdate' = case_
  # on (SProxy :: SProxy "password1") (set (_form <<< prop (SProxy :: SProxy "password1") <<< _validate))
  # on (SProxy :: SProxy "password2") (set (_form <<< prop (SProxy :: SProxy "password2") <<< _validate))
  # on (SProxy :: SProxy "email") (set (_form <<< prop (SProxy :: SProxy "email") <<< _validate))

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
        [ HH.text $ if (isJust st.formValue) then "Form Valid:" else "Form Not Valid:" ]
      , HH.ul_
        $ (\e -> HH.li_ [ HH.text e ]) <$> st.formErrors
      ]
    <>
      ( renderField <$> st.formFields )
    )

  renderField :: Field -> H.ComponentHTML Query
  renderField f@(EmailField { value, label, aFormField, aFormPart }) =
    formControl
      { helpText: Nothing
      , validation: case value of
          Invalid err -> show <$> head err
          Valid err a -> show <$> head err
      , label
      , inputId: label
      }
      ( HH.input
        [ HE.onBlur $ HE.input_ $ ValidateOne (aFormPart true)
        , HE.onValueInput $ HE.input $ UpdateContents <<< aFormField
        ] )
  renderField f@(PasswordField { value, label, helpText, aFormField, aFormPart }) =
    formControl
      { helpText: Just helpText
      , validation: case value of
          Invalid err -> show <$> head err
          Valid err a -> show <$> head err
      , label
      , inputId: label
      }
      ( HH.input
        [ HE.onBlur $ HE.input_ $ ValidateOne (aFormPart true)
        , HE.onValueInput $ HE.input $ UpdateContents <<< aFormField
        ] )

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

    UpdateContents update next -> do
      H.modify (doUpdate update)
      pure next

    ValidateOne update next -> do
      H.modify (doUpdate' update)
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
    helpText (Just errors) _ =
      HH.span_ [ HH.text $ "Errors: " <> errors ]
    helpText _ (Just x) =
      HH.span_ [ HH.text x ]
    helpText Nothing Nothing =
      HH.text ""

    label x = HH.span_ [ HH.text x ]
