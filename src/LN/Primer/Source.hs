{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module LN.Primer.Source (
  Source (..)
) where



import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)


data Source = Source {
} deriving (Show, Generic, Typeable)
