-- | This module provides types and functions for PostgreSQL's @lquery@ https://www.postgresql.org/docs/current/ltree.html
--
-- You will want to use a specific implementation, e.g. @postgresql-simple-ltree@.
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.LQuery
  ( LQuery(..)
  , contains
  ) where

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import Data.Text (Text)

import qualified Database.PostgreSQL.LTree as LTree

-- | Wrapper for Postgres' @lquery@ (label tree query) type.
newtype LQuery = LQuery { unLQuery :: Text }
  deriving newtype (Show, Eq, Ord)

-- | Build an @lquery@ expression which matches any @ltree@ which contains
-- the given @label@.
contains :: LTree.Label -> LQuery
contains label = LQuery $ "*." <> LTree.unLabel label <> ".*"

instance FromJSON LQuery where
  parseJSON = fmap LQuery . parseJSON

instance ToJSON LQuery where
  toJSON = toJSON . unLQuery
