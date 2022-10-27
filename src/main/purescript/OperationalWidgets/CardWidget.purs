module OperationalWidgets.CardWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT)
import Control.Semigroupoid ((<<<))
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (ceil)
import Data.Newtype (unwrap)
import Data.PrettyShow (prettyShow)
import Data.Semigroup ((<>))
import Data.Show (show)
import DataModel.AppState (AppError)
import DataModel.Card (Card(..), CardValues(..))
import DataModel.Index (CardEntry(..))
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Functions.Communication.Cards (getCard, postCard, deleteCard)
import Functions.Time (getCurrentTimestamp)
import OperationalWidgets.CreateCardWidget (createCardWidget)
import Views.CardViews (cardView, CardAction(..))
import Views.CreateCardView (createCardView)
import Views.SimpleWebComponents (loadingDiv)

cardWidget :: CardEntry -> WidgetState -> Widget HTML IndexUpdateData
cardWidget entry@(CardEntry_v1 { title: _, cardReference, archived: _, tags: _ }) state = do
  eitherCard <- case state of 
    Error err -> div [] [text $ "Card could't be loaded: " <> err]
    _ -> loadingDiv <|> (liftAff $ runExceptT $ getCard cardReference)
  case eitherCard of
    Right c -> do 
      res <- cardView c
      manageCardAction res
    Left err -> do
      _ <- liftEffect $ log $ show err
      cardWidget entry (Error (prettyShow err))

  where
    manageCardAction :: CardAction -> Widget HTML IndexUpdateData
    manageCardAction action = 
      case action of
        Edit cc -> do
          IndexUpdateData indexUpdateAction newCard <- createCardWidget cc Default -- here the modified card has already been saved
          case indexUpdateAction of
            AddReference newEntry -> pure $ IndexUpdateData (ChangeReferenceWithEdit entry newEntry) newCard
            _ -> cardWidget entry Default
        Clone cc -> do
          clonedCard <- liftAff $ cloneCardNow cc
          doOp cc false (postCard clonedCard) (\newEntry -> IndexUpdateData (CloneReference newEntry) cc)
        Archive (Card_v1 r) -> do
          timestamp' <- liftEffect $ getCurrentTimestamp
          let newCard = Card_v1 $ r { timestamp = timestamp', archived = true }
          doOp newCard false (postCard newCard) (\newEntry -> IndexUpdateData (ChangeReferenceWithoutEdit entry newEntry) newCard)
        Restore (Card_v1 r) -> do
          timestamp' <- liftEffect $ getCurrentTimestamp
          let newCard = Card_v1 $ r { timestamp = timestamp', archived = false }
          doOp newCard false (postCard newCard) (\newEntry -> IndexUpdateData (ChangeReferenceWithoutEdit entry newEntry) newCard)
        Delete cc -> doOp cc false (deleteCard cardReference) (\_ -> IndexUpdateData (DeleteReference entry) cc)

    doOp :: forall a. Card -> Boolean -> ExceptT AppError Aff a -> (a -> IndexUpdateData) -> Widget HTML IndexUpdateData
    doOp currentCard showForm op mapResult = do
      res <- (if showForm then inertCardFormView currentCard else inertCardView currentCard) <|> (liftAff $ runExceptT $ op)
      case res of
        Right a -> pure $ mapResult a
        Left err -> do
          _ <- liftEffect $ log $ show err
          div [] [ text ("Current operation could't be completed: " <> prettyShow err)
                           , cardView currentCard >>= manageCardAction ]

    inertCardView :: forall a. Card -> Widget HTML a
    inertCardView card = do
      _ <- div [] [
        loadingDiv
      , cardView card -- TODO: need to deactivate buttons to avoid returning some value here
      ]
      loadingDiv

    inertCardFormView :: forall a. Card -> Widget HTML a
    inertCardFormView card = do
      _ <- createCardView card Loading -- TODO: need to deactivate buttons to avoid returning some value here
      loadingDiv

cloneCardNow :: Card -> Aff Card
cloneCardNow (Card_v1 { timestamp: _, content, archived}) =
  case content of
    CardValues values -> do
      timestamp <- liftEffect $ (ceil <<< unwrap <<< unInstant) <$> now
      pure $ Card_v1 { timestamp, archived, content: (CardValues (values { title = (values.title <> " - CLONE")}))}
