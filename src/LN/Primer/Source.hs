{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | Declares our source types. These types describe a source (repository)
-- that contributes to the LN project.
--

module LN.Primer.Source (
    Source (..)
  , SourceStatus (..)
  , DeprecationReason
  , SourceVisibility (..)
  , SourceLanguage (..)
  , SourceType (..)
  , SourceGeneration (..)
  , SourceSpecificity (..)
  , MaintainerName
  , MaintainerURL
  , SourceMaintainer
) where



import           Data.Text     (Text)
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)



-- | Is this source still actively apart of the LN project, or has it been deprecated?
--
data SourceStatus
  = Active
  | Deprecated (Maybe DeprecationReason)
  deriving (Eq, Ord, Show, Generic, Typeable)

type DeprecationReason = Text



-- | Is this source public or private?
-- There are only a few private sources - which aren't very useful.
--
data SourceVisibility
  = Public
  | Private
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
  = Frontend
  | Backend
  | FullStack
  deriving (Eq, Ord, Show, Generic, Typeable)



-- | Declares whether this source is manually developed or automatically generated.
-- Automatic generation occurs via tools such as haskell-interop-prime & ln-interop.
--
data SourceGeneration
  = Manual
  | Automated
  deriving (Eq, Ord, Show, Generic, Typeable)



-- | Declares whether this source is Reuseable or Specific to the LN project.
--
data SourceSpecificity
  = Reuseable
  | Specific
  deriving (Eq, Ord, Show, Generic, Typeable)



-- | A maintainer
--
type MaintainerName   = Text
type MaintainerURL    = Text
type SourceMaintainer = (MaintainerName, MaintainerURL)



-- | A Source for the LN project.
--
data Source = Source {
  sourceName        :: Text,
  sourceDescription :: [Text],
  sourceURL         :: Text,
  sourceLanguages   :: [SourceLanguage],
  sourceStatus      :: SourceStatus,
  sourceVisibility  :: SourceVisibility,
  sourceType        :: SourceType,
  sourceGeneration  :: SourceGeneration,
  sourceSpecificity :: SourceSpecificity,
  sourceMaintainers :: [SourceMaintainer]
} deriving (Eq, Ord, Show, Generic, Typeable)
