module App.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = Validate a

type Input = Unit
type Message = Void
type State = Unit

component :: ∀ eff m. MonadAff ( console :: CONSOLE | eff ) m => H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = unit

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ formControl
        { helpText: Nothing
        , label: "Name"
        , validation: Nothing
        , inputId: "name" }
        ( HH.input [ HE.onBlur $ HE.input_ Validate ] )
      , formControl
        { helpText: Nothing
        , label: "Email"
        , validation: Nothing
        , inputId: "email" }
        ( HH.input [ HE.onBlur $ HE.input_ Validate ] )
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Validate next -> do
      H.liftAff $ log "Validating..."
      pure next


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
