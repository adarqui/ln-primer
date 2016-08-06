{-# LANGUAGE OverloadedStrings #-}

-- | Source definitions.
-- These definitions describe all of the sources used within the LN project.
--

module LN.Primer.Source.Definitions (
    sources
  , source_prefix
  , source_maintainers
  , defaultSource
  , source_ln_types
  , source_haskell_ln_types
  , source_purescript_ln_types
  , source_ln_yesod
  , source_ln_backend_core
  , source_ln_backend_yesod
  , source_ln_backend_servant
  , source_ln_backend_scotty
  , source_ln_ui_core
  , source_ln_ui_ghcjs
  , source_ln_ui_reactflux
  , source_ln_ui_purescript
  , source_ln_shell
) where



import           Data.Monoid      ((<>))
import           Data.Text        (Text)

import           LN.Primer.Source



sources :: [Source]
sources =
  [ source_ln_types
  , source_haskell_ln_types
  , source_purescript_ln_types
  , source_ln_yesod
  , source_ln_backend_core
  , source_ln_backend_yesod
  , source_ln_backend_servant
  , source_ln_backend_scotty
  , source_ln_ui_core
  , source_ln_ui_ghcjs
  , source_ln_ui_reactflux
  , source_ln_ui_purescript
  , source_ln_shell
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



-- | https://github.com/adarqui/ln-types
--
source_ln_types :: Source
source_ln_types = (defaultSource "ln-types") {
    sourceDescription = [ "Base library where all of the core types are defined."
                        , "ln-interop uses ln-types to generate corresponding types in other languages."
                        , "Containers types that are serialized over the API to bridge the frontend & backend."
                        ]
  , sourceLanguages   = [LangHaskell]
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



-- | https://github.com/adarqui/ln-yesod
--
source_ln_yesod :: Source
source_ln_yesod = (defaultSource "ln-yesod") {
    sourceDescription = [ "HTTPS backend using Yesod."
                        , "Serves up the API & websocket."
                        , "Persist data to PostgreSQL via Persistent."
                        ]
  , sourceLanguages   = [LangHaskell]
  , sourceType        = Backend
  }



-- | https://github.com/adarqui/ln-backend-core
-- TODO
--
source_ln_backend_core :: Source
source_ln_backend_core = (defaultSource "ln-backend-core") {
    sourceDescription = [ "Re-useable core backend."
                        , "The core is intended to be re-useable by any backend, allowing us to reduce code in since most of it will be shared."
                        ]
  , sourceLanguages   = [LangHaskell]
  , sourceType        = Backend
  , sourceStatus      = Todo
  }



-- | https://github.com/adarqui/ln-backend-yesod
-- TODO
--
source_ln_backend_yesod :: Source
source_ln_backend_yesod = (defaultSource "ln-backend-yesod") {
    sourceDescription = [ "HTTPS backend using Yesod & ln-backend-core."
                        , "Serves the API and websocket."
                        ]
  , sourceLanguages   = [LangHaskell]
  , sourceType        = Backend
  , sourceStatus      = Todo
  }



-- | https://github.com/adarqui/ln-backend-servant
-- TODO
--
source_ln_backend_servant :: Source
source_ln_backend_servant = (defaultSource "ln-backend-servant") {
    sourceDescription = [ "HTTPS backend using Servant & ln-backend-core."
                        , "Serves the API and websocket."
                        ]
  , sourceLanguages   = [LangHaskell]
  , sourceType        = Backend
  , sourceStatus      = Todo
  }



-- | https://github.com/adarqui/ln-backend-scotty
-- TODO
--
source_ln_backend_scotty :: Source
source_ln_backend_scotty = (defaultSource "ln-backend-scotty") {
    sourceDescription = [ "HTTPS backend using Scotty & ln-backend-core."
                        , "Serves the API and websocket."
                        ]
  , sourceLanguages   = [LangHaskell]
  , sourceType        = Backend
  , sourceStatus      = Todo
  }



-- | https://github.com/adarqui/ln-ui-core
--
source_ln_ui_core :: Source
source_ln_ui_core = (defaultSource "ln-ui-core") {
    sourceDescription = [ "Base system for all haskell frontends."
                        , "The core is intended to be re-useable by any frontend, allowing us to reduce code in since most of it will be shared."
                        ]

  , sourceLanguages   = [LangHaskell]
  , sourceType        = FullStack
  }



-- | https://github.com/adarqui/ln-ui-ghcjs
--
source_ln_ui_ghcjs :: Source
source_ln_ui_ghcjs = (defaultSource "ln-ui-ghcjs") {
    sourceDescription = [ "This repo simply compiles all of the ghcjs frontends (ie; reactflux) into javascript libraries." ]
  , sourceLanguages   = [LangHaskell]
  , sourceType        = Frontend
  }



-- | https://github.com/adarqui/ln-ui-reactflux
--
source_ln_ui_reactflux :: Source
source_ln_ui_reactflux = (defaultSource "ln-ui-reactflux") {
    sourceDescription = [ "A react-flux frontend using ln-ui-core."
                        , "React.Flux uses react & flux under the hood."
                        ]
  , sourceLanguages   = [LangHaskell]
  , sourceType        = Frontend
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
  , sourceType        = Frontend
  }



-- | https://github.com/adarqui/ln-ui-shell
--
source_ln_shell :: Source
source_ln_shell = (defaultSource "ln-shell") {
    sourceDescription = [ "Shell interface" ]
  , sourceLanguages   = [LangHaskell]
  , sourceStatus      = Deprecated (Just "Experiment. Will re-boot this as ln-ui-shell")
  , sourceType        = Frontend
  }
