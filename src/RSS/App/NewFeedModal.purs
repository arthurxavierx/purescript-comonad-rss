module RSS.App.NewFeedModal where

import Prelude

import Control.Comonad (extract, (=>>))
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Trans.Class (lower)
import Control.Monad.State.Class (modify, put)
import Control.Monad.Transition.Trans (TransitionT, hoistTransitionT)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Functor.Coproduct (Coproduct(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, launchAff_)
import RSS (Feed(..))
import RSS.Parser (parseURL)
import React.DOM as D
import React.DOM.Props as P
import React.SyntheticEvent (SyntheticInputEvent, preventDefault, stopPropagation)
import UI.Optional (Optional, optional_)
import UI.Optional as Optional
import UI.React (ReactComponent, ReactUI)
import UI.Zipper (Zipper(..))
import UI.Zipper as Zipper
import Unsafe.Coerce (unsafeCoerce)

type Space = Optional ModalContent
type Comm = (->) { onSubmit :: Feed String -> Aff Unit }

component :: ReactComponent Space Comm
component =
  optional_ (const (D.div' [])) wizard
  =>>
    -- TODO: improve this
    \component' send props ->
      case Optional.lower component' of
        Coproduct (Left _) ->
          extract component' send props
        Coproduct (Right _) ->
          D.div
            [ P.className "ModalOverlay"
            , P.onClick \_ -> launchAff_ $ send Optional.hide
            ]
            [ D.div
                [ P.className "Card column"
                , P.style { flex: "0 1 320px" }
                , P.onClick stopPropagation
                ]
                [ D.div
                    [ P.className "row"
                    , P.style { justifyContent: "flex-end" }
                    ]
                    [ D.button
                        [ P.className "icon"
                        , P.onClick \_ -> launchAff_ $ send Optional.hide
                        ]
                        [ D.text "×" ]
                    ]
                , extract component' send props
                ]
            ]

type WizardState = { url :: String, feed :: Maybe (Feed String) }

init :: WizardState
init = { url: "", feed: Nothing  }

type Wizard s = StoreT s Zipper
type ModalContent = Wizard WizardState

wizard :: ReactComponent ModalContent Comm
wizard = StoreT (Tuple render init)
  where
    render :: Zipper (WizardState -> ReactUI ModalContent Comm)
    render = Zipper Nil page1 (page2 : Nil) =>> allPages

    allPages
      :: Zipper (WizardState -> ReactUI ModalContent Comm)
      -> WizardState
      -> ReactUI ModalContent Comm
    allPages (Zipper before current _) feedData send onSubmit =
      D.div'
        [ D.div' $
            before # foldMap \ui ->
              [ ui feedData send onSubmit
              , D.hr'
              ]
        , current feedData send onSubmit
        ]

    page1 :: WizardState -> ReactUI ModalContent Comm
    page1 { url } send _ =
      D.div'
        [ D.p' [ D.text "Please, insert the URL of a valid RSS feed:" ]
        , D.form
            [ P.className "column"
            , P.onSubmit \e -> do
                _ <- preventDefault e
                launchAff_ do
                  feedM <- parseURL url
                  send do
                    _ <- modify (_ { feed = feedM })
                    hoistTransitionT lower Zipper.moveRight
            ]
            [ D.input
                [ P._type "text"
                , P.placeholder "RSS feed"
                , P.value url
                , P.autoFocus true
                , P.onChange \e -> launchAff_ $ send $ void $
                    modify (_ { url = value e })
                ]
            , D.button' [ D.text "Search" ]
            ]
        ]

    page2 :: WizardState -> ReactUI ModalContent Comm
    page2 state send { onSubmit } =
      case state.feed of
        Nothing ->
          D.div'
            [ D.text "No valid RSS feed found in this URL." ]

        Just (Feed feed@{ url, title, description }) ->
          D.div'
            [ D.form
                [ P.className "column"
                , P.onSubmit \e -> do
                    _ <- preventDefault e
                    launchAff_ (onSubmit (Feed feed))
                ]
                [ D.input
                    [ P._type "text"
                    , P.placeholder "Feed title"
                    , P.value title
                    , P.autoFocus true
                    , P.onChange \e -> launchAff_ $ send $ void $
                        modify (_ { feed = Just $ Feed (feed { title = value e }) })
                    ]
                , D.input
                    [ P._type "text"
                    , P.placeholder "Feed description (optional)"
                    , P.value description
                    , P.onChange \e -> launchAff_ $ send $ void $
                        modify (_ { feed = Just $ Feed (feed { description = value e }) })
                    ]
                , D.button' [ D.text "Ok" ]
                ]
            ]

show :: forall m. TransitionT Space m Unit
show =
  Optional.lift do
    put init
    hoistTransitionT lower Zipper.moveToFirst

hide :: forall m. TransitionT Space m Unit
hide = Optional.hide

value :: SyntheticInputEvent -> String
value e = (unsafeCoerce (unsafeCoerce e).target).value
