module Views.ExportView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, h1, h3, li, p, span, text, ul)
import Concur.React.Props as Props
import Control.Alt (($>))
import Functions.Export (UnencryptedExportVersion(..))

data ExportEvent = OfflineCopy | UnencryptedCopy UnencryptedExportVersion

exportView :: Widget HTML ExportEvent
exportView = div [Props._id "exportPage"] [
  h1 [] [text "Export"]
, div [Props.className "content"] [ ul [] [
    li [Props.className "content"] [
      h3 [] [text "Offline copy"]
    , div [Props.className "description"] [
        p [] [text "Download a read-only portable version of Clipperz. Very convenient when no Internet connection is available."]
      , p [] [text "An offline copy is just a single HTML file that contains both the whole Clipperz web application and your encrypted data."]
      , p [] [text "It is as secure as the hosted Clipperz service since they both share the same code and security architecture."]
      ]
    , button [Props.onClick] [span [] [text "download offline copy"]] $> OfflineCopy
    ]
  , li [] [ 
      h3 [] [text "HTML + JSON"]
    , div [Props.className "description"] [
        p [] [text "Download a printer-friendly HTML file that lists the content of all your cards."]
      , p [] [text "This same file also contains all your data in JSON format."]
      , p [Props.className "important"] [text "Beware: all data are unencrypted! Therefore make sure to properly store and manage this file."]
      ]
    , div [Props.className "actions"] [
        div [] [
          button [Props.onClick] [span [] [text "download HTML+JSON - epsilon"]] $> UnencryptedCopy Current
        , div [Props.className "description"] [
            p [] [ text "Compatible with the ", span [Props.className "important"] [text "current version"],  text " of Clipperz" ]
          ]
        ]
      , div [] [
          button [Props.onClick] [span [] [text "download HTML+JSON - delta"]]   $> UnencryptedCopy Legacy
        , div [Props.className "description"] [
            p [] [ text "Compatible with the ", span [Props.className "important"] [text "previous versions"], text " of Clipperz" ]
          ]
        ]
      ]
    ]
  ]]
]