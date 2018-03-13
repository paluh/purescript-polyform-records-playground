module App.Component where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (head)
import Data.Maybe (Maybe(..), isJust)
import Data.Record (modify)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Form (Email, Field(PasswordField, EmailField), RawForm, signupForm)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Polyform.Validation (V(..), runValidation)

data Query a
  = ValidateOne String a
  | ValidateAll a
  | UpdateContents String String a

type State =
  { form :: RawForm
  , formErrors :: Array String
  , formFields :: Array Field
  , formValue :: Maybe { email :: Email, password :: String } }

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
  renderField f@(EmailField { value, label, key }) =
    formControl
      { helpText: Nothing
      , validation: case value of
          Invalid err -> show <$> head err
          Valid err a -> show <$> head err
      , label
      , inputId: label
      }
      ( HH.input
        [ HE.onBlur $ HE.input_ $ ValidateOne key
        , HE.onValueInput $ HE.input $ UpdateContents key ] )
  renderField f@(PasswordField { value, label, helpText, key }) =
    formControl
      { helpText: Just helpText
      , validation: case value of
          Invalid err -> show <$> head err
          Valid err a -> show <$> head err
      , label
      , inputId: label
      }
      ( HH.input
        [ HE.onBlur $ HE.input_ $ ValidateOne key
        , HE.onValueInput $ HE.input $ UpdateContents key
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

    UpdateContents key text next -> do
      H.modify (\st -> st { form = modify (SProxy :: SProxy "password1") (_ { value = text }) st.form })
      pure next

    ValidateOne key next -> do
      H.modify (\st -> st { form = modify (SProxy :: SProxy "password1") (_ { validate = true }) st.form })
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
