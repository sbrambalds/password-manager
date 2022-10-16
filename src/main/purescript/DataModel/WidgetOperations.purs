module DataModel.WidgetOperations where

import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Card (Card)
import DataModel.Index (CardEntry)

data IndexUpdateData = IndexUpdateData IndexUpdateAction Card
instance showIndexUpdateData :: Show IndexUpdateData where
  show (IndexUpdateData action card) = "Do " <> show action <> " while showing " <> show card

data IndexUpdateAction = AddReference CardEntry
                       | CloneReference CardEntry 
                       | DeleteReference CardEntry
                       | ChangeReferenceWithEdit CardEntry CardEntry
                       | ChangeReferenceWithoutEdit CardEntry CardEntry
                       | NoUpdate

instance showIndexUpdateAction :: Show IndexUpdateAction where
  show (AddReference c ) = "Add reference to " <> show c
  show (CloneReference c ) = "Clone reference to " <> show c
  show (DeleteReference c ) = "Delete reference to " <> show c
  show (ChangeReferenceWithEdit c c') = "Change reference with edit of " <> show c <> " to " <> show c'
  show (ChangeReferenceWithoutEdit c c') = "Change reference without edit of " <> show c <> " to " <> show c'
  -- show (ChangeToReference c ) = "Change reference of " <> show c 
  show NoUpdate = "No update"