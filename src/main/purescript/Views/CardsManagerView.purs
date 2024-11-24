module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire)
import Concur.React (HTML)
import Concur.React.DOM (button, dd, div, dl, dt, h3, h4, header, li, ol, span, text)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>), (<|>))
import Control.Alternative (empty)
import Control.Bind ((=<<), (>>=))
import Control.Category ((<<<), (>>>))
import Data.Array (foldl, fromFoldable, mapWithIndex)
import Data.Boolean (otherwise)
import Data.CommutativeRing (add)
import Data.Either (either)
import Data.Eq (eq, (==))
import Data.Function (flip, ($))
import Data.Functor ((<$>), (<$))
import Data.HexString (HexString)
import Data.HeytingAlgebra (not, (||))
import Data.List (List(..), elem, elemIndex, index, length)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (max, min)
import Data.Ring (sub, (-))
import Data.Semigroup ((<>))
import Data.Set (Set, unions)
import Data.Time.Duration (Days)
import Data.Tuple (Tuple(..), swap)
import DataModel.CardVersions.Card (Card, emptyCard)
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..), Index(..))
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.Proxy (DataOnLocalStorage(..), ProxyInfo(..))
import DataModel.WidgetState (CardFormInput(..), CardManagerState, CardViewState(..))
import Effect.Class (liftEffect)
import Functions.Donations (DonationLevel(..))
import Functions.EnvironmentalVariables (donationIFrameURL)
import IndexFilterView (Filter(..), FilterData, FilterViewStatus(..), filteredEntries, getClassNameFromFilterStatus, indexFilterView, initialFilterData, shownEntries)
import OperationalWidgets.Sync (SyncData)
import Unsafe.Coerce (unsafeCoerce)
import Views.CardViews (CardEvent(..), cardView)
import Views.Components (proxyInfoComponent)
import Views.CreateCardView (CardFormData, createCardView)
import Views.DeviceSyncView (EnableSync, syncProgressBar)
import Views.DonationViews (donationIFrame)
import Views.DonationViews as DonationEvent

data CardManagerEvent = AddCardEvent        Card
                      | CloneCardEvent      CardEntry
                      | DeleteCardEvent     CardEntry
                      | EditCardEvent       (Tuple CardEntry Card)
                      | ArchiveCardEvent    CardEntry
                      | RestoreCardEvent    CardEntry
                      | OpenCardFormEvent   (Maybe (Tuple CardEntry Card))
                      | OpenUserAreaEvent
                      | ShowShortcutsEvent  Boolean
                      | ShowDonationEvent   Boolean
                      | ChangeFilterEvent   FilterData
                      | NavigateCardsEvent  NavigateCardsEvent
                      | UpdateDonationLevel Days
                      | UpdateCardForm      CardFormData
                      | NoEvent

data NavigateCardsEvent = Move (Maybe CardEntry) | Open (Maybe CardEntry) | Close (Maybe CardEntry)

cardManagerInitialState :: CardManagerState
cardManagerInitialState = {
  filterData: initialFilterData
, highlightedEntry: Nothing
, cardViewState: NoCard
, showShortcutsHelp: false
, showDonationOverlay: false
}

type EnableShortcuts = Boolean

cardsManagerView :: CardManagerState -> Index -> PasswordGeneratorSettings -> DonationLevel -> ProxyInfo -> EnableShortcuts -> EnableSync -> Maybe (Wire (Widget HTML) SyncData) -> Widget HTML (Tuple CardManagerEvent CardManagerState)
cardsManagerView state@{filterData: filterData@{filterViewStatus, filter, archived, searchString}, highlightedEntry, cardViewState, showShortcutsHelp, showDonationOverlay} index'@(Index {entries}) userPasswordGeneratorSettings donationLevel proxyInfo enableShortcuts enableSync syncDataWire = do
  div ([ Props._id "cardsManager", Props.className $ "filterView_" <> getClassNameFromFilterStatus filterViewStatus ]) [
    indexFilterView filterData index' <#> ChangeFilterEvent
  , div [Props.className "cardToolbarFrame"] [
      toolbarHeader "frame"
    , div ([Props._id "mainView", Props.className cardViewStateClass, Props.tabIndex 0] <> shortcutsHandlers) [
        div [Props._id "indexView"] [
          toolbarHeader "cardList"
        , div [Props.className "addCard"] [
            button [Props.onClick, Props.className "addCard" ] [span [] [text "add card"]] $> OpenCardFormEvent Nothing
          ]
        , (indexView sortedCards proxyInfo getOpenedCard getHighlightedIndex) <#> (NavigateCardsEvent <<< Open <<< Just)
        , donationButton (donationLevel == DonationInfo)
        ]
      , div [Props._id "card"] [
          mainStageView cardViewState
        ]
      ]
    , donationButton (donationLevel == DonationWarning)
    ]
  ] <> (shortcutsHelp showShortcutsHelp) <> (donationOverlay showDonationOverlay)
  <#> (Tuple state >>> swap)

  where

    cardViewStateClass :: String
    cardViewStateClass = case cardViewState of
      NoCard             -> "CardViewClose"
      CardForm _ NewCard -> "CardFormOpen NewCard"
      CardForm _ _       -> "CardFormOpen EditCard"
      Card     _ _       -> "CardViewOpen"

    sortedCards :: List CardEntry
    sortedCards  = filteredEntries filter (shownEntries entries selectedEntry highlightedEntry archived)

    selectedEntry :: Maybe CardEntry
    selectedEntry = case cardViewState of
      Card                   _ entry  -> Just entry
      CardForm _ (ModifyCard _ entry) -> Just entry
      _                               -> Nothing

    getHighlightedEntry :: Maybe CardEntry
    getHighlightedEntry = highlightedEntry <|> selectedEntry
    
    getHighlightedIndex :: Maybe Int
    getHighlightedIndex = getHighlightedEntry >>= flip elemIndex sortedCards

    getOpenedCard :: Maybe CardEntry
    getOpenedCard = case cardViewState of
                      Card _ cardEntry -> Just cardEntry
                      _                -> Nothing

    nextCardEntry     :: Maybe CardEntry
    nextCardEntry     = index sortedCards (min (length sortedCards - 1) (maybe 0 (add 1)      getHighlightedIndex))
    
    previousCardEntry :: Maybe CardEntry
    previousCardEntry = index sortedCards (max  0                       (maybe 0 (flip sub 1) getHighlightedIndex))

    -- getCardToOpen :: List CardEntry -> Maybe CardEntry
    -- getCardToOpen entries' = highlightedEntry >>= (index entries')

    getFilterHeader :: forall a. Filter -> Widget HTML a
    getFilterHeader f =
      case f of
        All                  -> text "clipperz"
        Recent               -> text "recent"
        Untagged             -> text "untagged"
        Search ""            -> text "clipperz"
        Search searchString' -> span [] [text searchString']
        Tag    tag           -> span [] [text tag]

    toolbarHeader :: String -> Widget HTML CardManagerEvent
    toolbarHeader className = header [Props.className className] [
      div [Props.className "toolbar"] [
        div [Props.className "tags"] [button [Props.onClick] [text "tags"]] $> ChangeFilterEvent (filterData {filterViewStatus = FilterViewOpen})
      , div [Props.className "selection"] [getFilterHeader filter]
      , OpenUserAreaEvent <$ div [Props.className "menu"] [button [Props.onClick] [text "menu"]]
      ]
    , syncProgressBar enableSync syncDataWire
    , proxyInfoComponent proxyInfo [Just "withDate"]
    ] 

    allTags :: Set String
    allTags = unions $ (\(CardEntry entry) -> entry.tags) <$> fromFoldable entries

    shortcutsHandlers = if (not enableShortcuts || isCardForm || showDonationOverlay) then [] else [
      Props.onKeyDown <#> (\e -> case (unsafeCoerce e).key of
        key
          | eq   key  "*"                          ->  ChangeFilterEvent initialFilterData
          | eq   key  "/"                          ->  ChangeFilterEvent filterData {filterViewStatus = FilterViewOpen, filter = Search searchString, selected = true}
          | elem key ["k", "ArrowUp"             ] -> (NavigateCardsEvent $ Move (previousCardEntry))
          | elem key ["j", "ArrowDown"           ] -> (NavigateCardsEvent $ Move (nextCardEntry    ))
          | elem key ["l", "ArrowRight", "Enter" ] -> (NavigateCardsEvent $ Open (highlightedEntry ))
          | elem key ["h", "ArrowLeft" , "Escape"] -> if showShortcutsHelp
                                                then   ShowShortcutsEvent   false
                                                else  (NavigateCardsEvent $ Close getHighlightedEntry)
          | eq   key  "?"                          ->  ShowShortcutsEvent   true
          | otherwise                              ->  NoEvent
      )
    ]
      where
        isCardForm = case cardViewState of
          CardForm _ _ -> true
          _            -> false

    mainStageView :: CardViewState -> Widget HTML CardManagerEvent
    mainStageView  NoCard                               = div [] []
    mainStageView (Card card cardEntry)                 = cardView proxyInfo card cardEntry <#> handleCardEvents
    mainStageView (CardForm cardFormData cardFormInput) = createCardView cardFormData inputCard allTags userPasswordGeneratorSettings proxyInfo <#> (either UpdateCardForm (maybe (NavigateCardsEvent $ viewCardStateUpdate) outputEvent))
      where

        inputCard = case cardFormInput of
          NewCard                    -> emptyCard
          NewCardFromFragment card   -> card
          ModifyCard          card _ -> card
        
        viewCardStateUpdate = case cardFormInput of
          NewCard                   -> Close  Nothing
          NewCardFromFragment _     -> Close  getHighlightedEntry 
          ModifyCard _    cardEntry -> Open  (Just cardEntry)

        outputEvent = case cardFormInput of
          NewCard                -> AddCardEvent
          NewCardFromFragment _  -> AddCardEvent
          ModifyCard _ cardEntry -> EditCardEvent <<< Tuple cardEntry

    handleCardEvents :: CardEvent -> CardManagerEvent
    handleCardEvents (Edit    cardEntry card) = OpenCardFormEvent $ Just (Tuple cardEntry card)
    handleCardEvents (Clone   cardEntry     ) = CloneCardEvent   cardEntry
    handleCardEvents (Archive cardEntry     ) = ArchiveCardEvent cardEntry
    handleCardEvents (Restore cardEntry     ) = RestoreCardEvent cardEntry
    handleCardEvents (Delete  cardEntry     ) = DeleteCardEvent  cardEntry
    handleCardEvents (Exit                  ) = NavigateCardsEvent $ Close getHighlightedEntry

-- ==================================================================                                                                                                                             

shortcutsHelp :: Boolean -> Widget HTML CardManagerEvent
shortcutsHelp showShortcutsHelp = div [Props.classList [Just "shortcutsHelp", Just "disableOverlay", hiddenClass]] [
  div [Props.className "mask", Props.onClick] []
, div [Props.className "helpBox"] [
      header [] [
        h3 [] [text "Keyboard shortcuts"]
      , button [Props.className "close", Props.onClick] [text "close"]
      ]
    , div [Props.className "helpContent"] [
        helpBlock "Search"      [ Tuple [ Tuple "/"        Nothing    ] "search cards"
                                , Tuple [ Tuple "*"        Nothing    ] "select all cards"
                                ]       
      , helpBlock "Navigation"  [ Tuple [ Tuple "h"       (Just "or")
                                        , Tuple "<left>"  (Just "or")
                                        , Tuple "<esc>"    Nothing
                                        ]                                "exit current selection"
                                , Tuple [ Tuple "l"       (Just "or")
                                        , Tuple "<right>" (Just "or")
                                        , Tuple "<enter>"  Nothing
                                        ]                                "select detail"   
                                , Tuple [ Tuple "k"        (Just "/")
                                        , Tuple "j"        (Just "or")
                                        , Tuple "<up>"     (Just "/")
                                        , Tuple "<down>"    Nothing
                                        ]                                "previous/next card"
                                ]
      -- , helpBlock "Misc"        [ Tuple [ Tuple "l o c k"   Nothing   ] "lock application"
      --                           ] 
    ]
  ]
] $> ShowShortcutsEvent false
  where
    helpBlock :: forall a. String 
                        -> Array (Tuple (Array (Tuple String (Maybe String))) String) 
                        -> Widget HTML a
    helpBlock title shortcuts = 
      div [Props.className "helpBlock"] [
        h4 [] [text title]
      , dl [] $ foldl (\shortcutsList (Tuple shortcut description) -> shortcutsList <> [
          dt [] $ foldl (\shortcutCombination (Tuple key operator) -> 
               shortcutCombination <> [ span [Props.className "key"] [text key] ] <> case operator of
                                                                                      Just op -> [span [Props.className "operator"] [text op]]
                                                                                      Nothing -> []
          ) [] shortcut
        , dd [] [text description]
        ]) [] shortcuts
      ]
    
    hiddenClass :: Maybe String
    hiddenClass = if showShortcutsHelp
                  then Nothing
                  else Just "hidden"

-- ==================================================================                                                                                                                             

donationButton :: Boolean -> Widget HTML CardManagerEvent
donationButton false = empty
donationButton true  =
  div [Props.classList [Just "donationButton"]] [
    button [Props.onClick $> ShowDonationEvent true, Props.filterProp (\e -> (unsafeCoerce e).key == "Escape") Props.onKeyDown $> ShowDonationEvent false] [span [] [text "Support Clipperz"]]
  ] 

donationOverlay :: Boolean -> Widget HTML CardManagerEvent
donationOverlay false = empty
donationOverlay true  = 
  div [Props.className "donationOverlay"] [
    div [Props.className "disableOverlay"] [
      div [Props.className "mask", Props.onClick] [] $> ShowDonationEvent false
    , div [Props.className "dialog"] [
        (donationIFrame =<< (liftEffect $ donationIFrameURL "button/")) 
        <#> 
        (\res -> case res of
          DonationEvent.CloseDonationPage     ->  ShowDonationEvent false
          DonationEvent.UpdateDonationLevel m ->  UpdateDonationLevel m
        )
      ]
    ]
  ]

-- ==================================================================                                                                                                                             

indexView :: List CardEntry -> ProxyInfo -> Maybe CardEntry -> Maybe Int -> Widget HTML CardEntry
indexView sortedCards proxyInfo openedCard selectedEntry = do
  let disabledCards = getDisabledCards proxyInfo
  ol [] (
    flip mapWithIndex (fromFoldable sortedCards) (\index cardEntry@(CardEntry { title, archived: archived', cardReference: CardReference { reference: ref } }) -> do
      let disabled = proxyInfo == Offline NoData || elem ref disabledCards
      li ([Props.classList [archivedClass archived', selectedClass index, openedClass cardEntry, disabledClass disabled]] <> if disabled then []  else [cardEntry <$ Props.onClick]) [
        text title
      ]
    )
  ) 

  where
    archivedClass archived' = if archived'                       then Just "archived"     else Nothing
    selectedClass index     = if selectedEntry == Just index     then Just "selectedCard" else Nothing
    openedClass   cardEntry = if openedCard    == Just cardEntry then Just "openedCard"   else Nothing
    disabledClass disabled  = if disabled                        then Just "disabled"     else Nothing

    getDisabledCards :: ProxyInfo -> List HexString
    getDisabledCards (Offline (WithData refList)) = refList
    getDisabledCards _                            = Nil
