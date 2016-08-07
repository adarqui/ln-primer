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
  , source_ocaml_ln_types
  , source_idris_ln_types
  , source_rust_ln_types
  , source_javascript_ln_types
  , source_go_ln_types
  , source_c_ln_types
  , source_ln_yesod
  , source_ln_backend_core
  , source_ln_backend_yesod
  , source_ln_backend_servant
  , source_ln_backend_scotty
  , source_ln_ui_core
  , source_ln_ui_ghcjs
  , source_ln_ui_reactflux
  , source_ln_ui_purescript
  , source_ln_ui_shell
  , source_ln_sanitize
  , source_ln_validate
  , source_ln_lib
  , source_ln_shell
  , source_ln_fix
  , source_haskell_web_bootstrap
  , source_haskell_ebyam
  , source_haskell_rehtie
  , source_haskell_ifte
  , source_haskell_either_helpers
) where



import           Data.Monoid      ((<>))
import           Data.Text        (Text)

import           LN.Primer.Source



sources :: [Source]
sources =
  [ source_ln_types
  , source_haskell_ln_types
  , source_purescript_ln_types
  , source_ocaml_ln_types
  , source_idris_ln_types
  , source_rust_ln_types
  , source_javascript_ln_types
  , source_go_ln_types
  , source_c_ln_types
  , source_ln_yesod
  , source_ln_backend_core
  , source_ln_backend_yesod
  , source_ln_backend_servant
  , source_ln_backend_scotty
  , source_ln_ui_core
  , source_ln_ui_ghcjs
  , source_ln_ui_reactflux
  , source_ln_ui_purescript
  , source_ln_ui_shell
  , source_ln_sanitize
  , source_ln_validate
  , source_ln_lib
  , source_ln_shell
  , source_ln_fix
  , source_haskell_web_bootstrap
  , source_haskell_ebyam
  , source_haskell_rehtie
  , source_haskell_ifte
  , source_haskell_either_helpers
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



-- | https://github.com/adarqui/ocaml-ln-types
--
source_ocaml_ln_types :: Source
source_ocaml_ln_types = (defaultSource "ocaml-ln-types") {
    sourceDescription = [ "ln-types for ocaml" ]
  , sourceLanguages   = [LangOcaml]
  , sourceGeneration  = Generated
  , sourceStatus      = Todo
  }



-- | https://github.com/adarqui/idris-ln-types
--
source_idris_ln_types :: Source
source_idris_ln_types = (defaultSource "idris-ln-types") {
    sourceDescription = [ "ln-types for idris" ]
  , sourceLanguages   = [LangIdris]
  , sourceGeneration  = Generated
  , sourceStatus      = Todo
  }



-- | https://github.com/adarqui/rust-ln-types
--
source_rust_ln_types :: Source
source_rust_ln_types = (defaultSource "rust-ln-types") {
    sourceDescription = [ "ln-types for rust" ]
  , sourceLanguages   = [LangRust]
  , sourceGeneration  = Generated
  , sourceStatus      = Todo
  }



-- | https://github.com/adarqui/javascript-ln-types
--
source_javascript_ln_types :: Source
source_javascript_ln_types = (defaultSource "javascript-ln-types") {
    sourceDescription = [ "ln-types for javascript" ]
  , sourceLanguages   = [LangJavascript]
  , sourceGeneration  = Generated
  , sourceStatus      = Todo
  }



-- | https://github.com/adarqui/go-ln-types
--
source_go_ln_types :: Source
source_go_ln_types = (defaultSource "go-ln-types") {
    sourceDescription = [ "ln-types for go" ]
  , sourceLanguages   = [LangGo]
  , sourceGeneration  = Generated
  , sourceStatus      = Todo
  }



-- | https://github.com/adarqui/c-ln-types
--
source_c_ln_types :: Source
source_c_ln_types = (defaultSource "c-ln-types") {
    sourceDescription = [ "ln-types for c" ]
  , sourceLanguages   = [LangGo]
  , sourceGeneration  = Generated
  , sourceStatus      = Todo
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
  , sourceStatus      = Todo
  , sourceType        = Backend
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
  , sourceStatus      = Todo
  , sourceType        = Backend
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
  , sourceStatus      = Todo
  , sourceType        = Backend
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
  , sourceStatus      = Todo
  , sourceType        = Backend
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



-- | https://github.com/adarqui/ln-ui-shell
--
source_ln_ui_shell :: Source
source_ln_ui_shell = (defaultSource "ln-ui-shell") {
    sourceDescription = [ "A shell frontend using ln-ui-core." ]
  , sourceLanguages   = [LangHaskell]
  , sourceStatus      = Todo
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



-- | https://github.com/adarqui/ln-sanitize
--
source_ln_sanitize :: Source
source_ln_sanitize = (defaultSource "ln-sanitize") {
    sourceDescription = [ "Sanitization routines for inputs & outputs" ]
  , sourceType        = FullStack
  }



-- | https://github.com/adarqui/ln-validate
--
source_ln_validate :: Source
source_ln_validate = (defaultSource "ln-validate") {
    sourceDescription = [ "Validation routines for inputs" ]
  , sourceType        = FullStack
  }



-- | https://github.com/adarqui/ln-lib
--
source_ln_lib :: Source
source_ln_lib = (defaultSource "ln-lib") {
    sourceDescription = [ "Misc reuseable functions that don't fit in any other library." ]
  , sourceType        = FullStack
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



-- | https://github.com/adarqui/ln-fix
--
source_ln_fix :: Source
source_ln_fix = (defaultSource "ln-fix") {
    sourceDescription = [ "Misc stuff to fix the database etc" ]
  , sourceLanguages   = [LangHaskell]
  , sourceType        = Backend
  }



-- | https://github.com/adarqui/haskell-web-bootstrap
--
source_haskell_web_bootstrap :: Source
source_haskell_web_bootstrap = (defaultSource "haskell-web-bootstrap") {
    sourceDescription = [ "Bootstrap(3) CSS classes for Haskell" ]
  , sourceStatus      = Complete
  , sourceType        = FullStack
  , sourceSpecificity = Reuseable
  }



-- | https://github.com/adarqui/haskell-ebyam
--
source_haskell_ebyam :: Source
source_haskell_ebyam = (defaultSource "haskell-ebyam") {
    sourceDescription = [ "Flipped maybe for code readability" ]
  , sourceStatus      = Complete
  , sourceType        = FullStack
  , sourceSpecificity = Reuseable
  }



-- | https://github.com/adarqui/haskell-rehtie
--
source_haskell_rehtie :: Source
source_haskell_rehtie = (defaultSource "haskell-rehtie") {
    sourceDescription = [ "Flipped either for code readability" ]
  , sourceStatus      = Complete
  , sourceType        = FullStack
  , sourceSpecificity = Reuseable
  }



-- | https://github.com/adarqui/haskell-ifte
--
source_haskell_ifte :: Source
source_haskell_ifte = (defaultSource "haskell-ifte") {
    sourceDescription = [ "if/then/else helpers for code readability (debatable)" ]
  , sourceStatus      = Complete
  , sourceType        = FullStack
  , sourceSpecificity = Reuseable
  }



-- | https://github.com/adarqui/haskell-either-helpers
--
source_haskell_either_helpers :: Source
source_haskell_either_helpers = (defaultSource "haskell-either-helpers") {
    sourceDescription = [ "EitherT helpers for code readability/efficiency" ]
  , sourceStatus      = Complete
  , sourceType        = FullStack
  , sourceSpecificity = Reuseable
  }
