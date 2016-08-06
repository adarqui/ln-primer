{-# LANGUAGE OverloadedStrings #-}

-- | Source definitions.
-- These definitions describe all of the sources used within the LN project.
--

module LN.Primer.Source.Definitions (
    sources
  , source_prefix
  , source_maintainers
  , defaultSource
  , source_ln_yesod
  , source_ln_ui_purescript
  , source_haskell_ln_types
  , source_purescript_ln_types
) where



import           Data.Monoid      ((<>))
import           Data.Text        (Text)

import           LN.Primer.Source



sources :: [Source]
sources =
  [ source_ln_yesod
  , source_ln_ui_purescript
  , source_haskell_ln_types
  , source_purescript_ln_types
  ]



source_prefix :: Text
source_prefix = "github.com/adarqui"



source_maintainers :: [SourceMaintainer]
source_maintainers =
  [ ("Andrew Darqui", "https://github.com/adarqui") ]



defaultSource :: Text -> Source
defaultSource package = Source {
      sourceName        = package
    , sourceDescription = [package]
    , sourceURL         = source_prefix <> "/" <> package
    , sourceLanguages   = []
    , sourceStatus      = Active
    , sourceVisibility  = Public
    , sourceType        = Backend
    , sourceGeneration  = Manual
    , sourceSpecificity = Specific
    , sourceMaintainers = source_maintainers
}



-- | https://github.com/adarqui/ln-yesod
--
source_ln_yesod :: Source
source_ln_yesod = (defaultSource "ln-yesod") {
    sourceDescription = [ "HTTPS backend using Yesod"
                        , "Serves up the API & web socket."
                        , "Persist data to PostgreSQL via Persistent."
                        ]
  , sourceLanguages   = [LangHaskell]
  }



-- | https://github.com/adarqui/ln-ui-purescript
--
source_ln_ui_purescript :: Source
source_ln_ui_purescript = (defaultSource "ln-ui-purescript") {
    sourceDescription = [ "Initial frontend for LN."
                        , "A purescript-halogen app."
                        ]
  , sourceLanguages   = [LangPurescript]
  , sourceStatus      = Deprecated (Just "Purescript 0.9.x compiler is too slow.")
  }



-- | https://github.com/adarqui/haskell-ln-types
--
source_haskell_ln_types :: Source
source_haskell_ln_types = (defaultSource "haskell-ln-types") {
    sourceDescription = [ "ln-types for haskell" ]
  , sourceLanguages   = [LangHaskell]
  , sourceGeneration  = Generated
  }



-- | https://github.com/adarqui/purescript-ln-types
--
source_purescript_ln_types :: Source
source_purescript_ln_types = (defaultSource "purescript-ln-types") {
    sourceDescription = [ "ln-types for purescript" ]
  , sourceLanguages   = [LangPurescript]
  , sourceGeneration  = Generated
  }
