{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module LN.Primer.Source (
    Source (..)
  , SourceStatus (..)
  , SourceVisibility (..)
  , SourceLanguage (..)
  , SourceType (..)
  , SourceGeneration (..)
) where



import           Data.Text     (Text)
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)



-- | Is this source still actively apart of the LN project, or has it been deprecated?
--
data SourceStatus
  = StatusActive
  | StatusDeprecated
  deriving (Eq, Ord, Show, Generic, Typeable)



-- | Is this source public or private?
-- There are only a few private sources - which aren't very useful.
--
data SourceVisibility
  = VisibilityPublic
  | VisibilityPrivate
  deriving (Eq, Ord, Show, Generic, Typeable)



-- | Supported languages for the LN project.
--
data SourceLanguage
  = LangHaskell
  | LangPurescript
  | LangOcaml
  | LangRust
  | LangGo
  | LangC
  deriving (Eq, Ord, Show, Generic, Typeable)



-- | Is this source only useable on the backend, frontend, or both?
--
data SourceType
  = TypeFrontend
  | TypeBackend
  | TypeFullStack
  deriving (Eq, Ord, Show, Generic, Typeable)



-- | Declares whether this source is manually developed or automatically generated.
-- Automatic generation occurs via tools such as haskell-interop-prime & ln-interop.
--
data SourceGeneration
  = GenManual
  | GenAutomated
  deriving (Eq, Ord, Show, Generic, Typeable)



-- | Declares whether this source is Reuseable or Specific to the LN project.
--
data SourceSpecificty
  = Reuseable
  | Specific
  deriving (Eq, Ord, Show, Generic, Typeable)



-- | A Source for the LN project.
--
data Source = Source {
  sourceName        :: Text,
  sourceDescription :: Text,
  sourceURL         :: Maybe Text,
  sourceLanguages   :: [SourceLanguage],
  sourceStatus      :: SourceStatus,
  sourceVisibility  :: SourceVisibility
} deriving (Eq, Ord, Show, Generic, Typeable)
