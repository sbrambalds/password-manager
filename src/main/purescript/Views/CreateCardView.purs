module Views.CreateCardView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, datalist, div, form, h4, input, label, li, option, span, text, textarea, ul)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>), (<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Plus (empty)
import Control.Semigroupoid ((<<<))
import Data.Array (delete, snoc, sort)
import Data.Either (Either(..))
import Data.Eq ((==), (/=))
import Data.Function (($))
import Data.Functor (map, (<$), (<$>))
import Data.HeytingAlgebra (not, (||))
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Data.Set (Set, difference, fromFoldable, member, toUnfoldable)
import Data.String (null)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import DataModel.AsyncValue as Async
import DataModel.CardVersions.Card (Card(..), CardField(..), FieldType(..), _fields, _notes, _tags, _title, emptyCard, emptyCardField)
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.Proxy (ProxyInfo(..))
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Functions.Card (getFieldType)
import Functions.Time (getCurrentTimestamp)
import MarkdownIt (renderString)
import Type.Proxy (Proxy(..))
import Views.Components (dynamicWrapper, entropyMeter)
import Views.PasswordGenerator (passwordGenerator)
import Views.SimpleWebComponents (confirmationWidget, dragAndDropAndRemoveList, simpleButton)

type CardFormData = {
  newTag  :: String
, preview :: Boolean
, card    :: Card
}
_card :: Lens' CardFormData Card
_card = prop (Proxy :: _ "card")
_preview :: Lens' CardFormData Boolean
_preview = prop (Proxy :: _ "preview")
_newTag :: Lens' CardFormData String
_newTag = prop (Proxy :: _ "newTag")

emptyCardFormData :: CardFormData
emptyCardFormData = {newTag: "", preview: false, card: emptyCard}

-- ------------------------------------------------------------------------

createCardView :: CardFormData -> Card -> Set String -> PasswordGeneratorSettings -> ProxyInfo -> Widget HTML (Either CardFormData (Maybe Card))
createCardView cardFormData@{card} originalCard allTags passwordGeneratorSettings proxyInfo = do
  div [Props._id "cardForm"] [
    mask
  , div [Props.className "cardForm"] [
      formSignal passwordGeneratorSettings
    ]
  ] >>= (\res -> 
    case res of
      Right (Just (Card { content, secrets })) -> do
        timestamp' <- liftEffect $ getCurrentTimestamp
        pure $ Right $ Just (Card { content, secrets, archived: false, timestamp: timestamp' })
      _ -> pure res
  )

  where 
    mask = div [Props.className "mask"] []

    getActionButton :: CardField -> Widget HTML Unit
    getActionButton cardField =
      case getFieldType cardField of
        Passphrase  -> button [unit <$ Props.onClick, Props.disabled false, Props.className "action passwordGenerator" ] [text "password generator"]
        Email       -> button [Props.disabled true, Props.className "action email"]                                      [text "email"]
        Url         -> button [Props.className "action url",  Props.disabled true]                                       [text "url"]
        None        -> button [Props.className "action none", Props.disabled true]                                       [text "none"]

    cardFieldWidget :: PasswordGeneratorSettings -> CardField -> Widget HTML CardField
    cardFieldWidget defaultSettings cf@(CardField r@{ name, value, locked, settings}) = do
      let fieldActionWidget = [(\(Tuple v s) -> CardField $ r { value = v, settings = s })
        <$> do
              getActionButton cf
              if locked then
                button [Props.disabled true, Props.className "action passwordGenerator" ] [span [] [text "password generator"]]
                <>
                (div [Props.className "passwordGeneratorOverlay"] [
                  div [(Tuple value settings) <$ Props.onClick, Props.className "passwordGeneratorMask"] []
                , div [Props.className "passwordGeneratorPopup"] [passwordGenerator (fromMaybe defaultSettings settings) (if null value then (Async.Loading Nothing) else (Async.Done value))]
                ])
              else div [] []
      ]

      div [Props.classList ([Just "fieldForm", if (locked) then Just "locked" else Nothing])] [
        div [Props.className "inputs"] [
          ((\v -> CardField $ r { name  = v }) <<< (Props.unsafeTargetValue)) <$> label [Props.className "label"] [
            span [Props.className "label"] [text "Field label"]
          , input [Props._type "text", Props.placeholder "label", Props.value name, Props.onChange]
          ]
        , ((\v -> CardField $ r { value  = v }) <<< (Props.unsafeTargetValue)) <$> label [Props.className "value"] [
            span [Props.className "label"] [text "Field value"]
          , dynamicWrapper Nothing value $ textarea [Props.rows 1, Props.placeholder (if locked then "" else "value"), Props.value value, Props.onChange] []
          , (if locked
            then (entropyMeter value)
            else  empty
            )
          ]
        ]
      , div [Props.className "fieldActions"] $ fieldActionWidget <> [
          (\v -> CardField $ r { locked = v })
          <$>
          button [not locked <$ Props.onClick, Props.className "lock"] [text if locked then "locked" else "unlocked"]
        ]
      ]

    fieldsSignal :: PasswordGeneratorSettings -> Array CardField -> Widget HTML (Array CardField)
    fieldsSignal settings fields = do
      let loopables = (\f -> Tuple f (cardFieldWidget settings)) <$> fields 
      -- fields' <- loopS loopables $ \ls -> do
      dragAndDropAndRemoveList loopables <#> (map fst)
      <>
      (div [Props.className "newCardField", Props.onClick] [
        div [Props.className "fieldGhostShadow"] [
          div [Props.className "label"] []
        , div [Props.className "value"] []
        ]
      , button [Props.className "addNewField"] [text "add new field"]
      ] $> (snoc fields emptyCardField))
      -- pure $ fst <$> fields'

    tagSignal :: String -> Widget HTML String
    tagSignal tag = li [] [
      simpleButton "remove" "remove tag" false unit
    , text tag
    ] $> tag

    inputTagSignal :: String -> Set String -> Widget HTML (Tuple String Boolean)
    inputTagSignal newTag tags = do

      -- loopW (Tuple newTag false) (\(Tuple value _) -> do
        form [(\_ -> Tuple newTag (newTag /= "")) <$> Props.onSubmit] [
          label [] [
            span [Props.className "label"] [text "New Tag"]
            , input [
                Props._type "text"
              , Props.placeholder "add tag"
              , Props.value newTag
              , Props.list "tags-list"
              , (\e -> Tuple (Props.unsafeTargetValue e) (member (Props.unsafeTargetValue e) (difference allTags tags))) <$> Props.onInput
              ]
            , datalist [Props._id "tags-list"] ((\t -> option [] [text t]) <$> (toUnfoldable $ difference allTags tags))
          ]
        ]

    tagsSignal :: String -> Set String -> Widget HTML (Tuple String (Array String))
    tagsSignal newTag tags = do
      let sortedTags = sort $ toUnfoldable tags :: Array String
      div [Props.className "tags"] [
        ul [] $
          ((\tag -> tagSignal tag <#> (\tagToRemove -> Tuple newTag (delete tagToRemove sortedTags))) <$> sortedTags)
          <>
          [li [Props.className "addTag"] [
              inputTagSignal newTag tags <#> (\(Tuple newTag' addTag) -> 
                case addTag of
                  false -> Tuple newTag'  sortedTags
                  true  -> Tuple ""      (snoc sortedTags newTag')
              )
            ]
          ]
      ]
      
    notesSignal :: Boolean -> String -> Widget HTML (Tuple String Boolean)
    notesSignal preview notes = do
      div [Props.className "preview"] [
        h4 [] [text "Notes"]
      , if (null notes) then empty else (label [] [
          span [] [text "Preview Markdown"]
        , input [
            Props._type "checkbox"
          , Props.checked preview
          , Props.onChange $> (Tuple notes (not preview))
          ]
        ])
      ]
      <>
      div [Props.classList [Just "card_notes", if preview then Just "markdown" else Nothing]] [
        if preview 
        then  div [Props.className "markdown-body", Props.dangerouslySetInnerHTML { __html: unsafePerformEffect $ renderString notes}] []
        else  label [Props.className "notes"] [
                span [Props.className "label"] [text "Notes"]
              , dynamicWrapper Nothing notes $ textarea [Props.rows 1, Props.value notes, Props.onChange, Props.placeholder "notes"] []
              ] <#> (\e -> (Tuple (Props.unsafeTargetValue e) preview))
      ]

    formSignal :: PasswordGeneratorSettings -> Widget HTML (Either CardFormData (Maybe Card))
    formSignal settings =
      (div [Props.className "cardFormFields"] [
        label [Props.className "title"] [
          span [Props.className "label"] [text "Title"]
        , dynamicWrapper Nothing                 (view (_card <<< _title)  cardFormData) $ 
            textarea  [ Props.rows 1
                      , Props.placeholder "Card title"
                      , Props.autoFocus true
                      , Props.unsafeTargetValue <$> Props.onChange
                      , Props.value              (view (_card <<< _title)  cardFormData)
                      ] []
        ]                                                                                <#> (\title                 -> (set (_card <<< _title)  title)                                        cardFormData)
      , tagsSignal  (view _newTag cardFormData)  (view (_card <<< _tags)   cardFormData) <#> (\(Tuple newTag tags)   -> (set (_card <<< _tags)   (fromFoldable tags) <<< set _newTag newTag)   cardFormData)
      , fieldsSignal settings                    (view (_card <<< _fields) cardFormData) <#> (\fields                -> (set (_card <<< _fields)  fields)                                      cardFormData)
      , notesSignal (view _preview cardFormData) (view (_card <<< _notes)  cardFormData) <#> (\(Tuple notes preview) -> (set (_card <<< _notes)               notes  <<< set _preview preview) cardFormData)

        -- pure $ {
        --   newTag: newTag'
        -- , preview: preview'
        -- , card: Card { content: (CardValues {title: title', tags: fromFoldable tags', fields: fields', notes: notes'})
        --               , secrets: []
        --               , archived: archived
        --               , timestamp
        --               }
        -- }
      ] <#> Left)
      <>
      (div [Props.className "submitButtons"] [(cancelButton card) <|> (saveButton card)])


    cancelButton v = 
      if (originalCard == v) then 
      -- if ((card == v && not isNew) || (v == emptyCard && isNew)) then 
        simpleButton "inactive cancel" "cancel" false (Right Nothing) 
      else do
        _ <- simpleButton "active cancel" "cancel" false (Right Nothing)
        confirmation <- (simpleButton "active cancel" "cancel" false false) <|> (confirmationWidget "Are you sure you want to exit without saving?")
        if confirmation then pure (Right Nothing) else (cancelButton v)

    saveButton v =
      -- simpleButton "save" "save" ((not isNew && card == v) || (isNew && v == emptyCard)) (Just v)
      simpleButton "save" "save" (originalCard == v || (not (proxyInfo == Online))) (Right $ Just v)
