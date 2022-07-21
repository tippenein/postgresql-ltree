-- | Description: This module is a wrapper for PostgreSQL's @lquery@ https://www.postgresql.org/docs/current/ltree.html
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Simple.LQuery
  ( module Database.PostgreSQL.LQuery
  ) where

import Prelude

import Database.PostgreSQL.LQuery

import Control.Monad (when)
import Database.PostgreSQL.Simple.FromField
  ( FromField(fromField), ResultError(Incompatible, UnexpectedNull), returnError, typename
  )
import Database.PostgreSQL.Simple.ToField (ToField(toField))

import qualified Data.Text.Encoding as Text

instance ToField LQuery where
  toField = toField . unLQuery

instance FromField LQuery where
  fromField fld mbs = do
    -- There might be a more efficient way to check this, need to see
    -- if the @lquery@ type has a stable typoid or not.
    typ <- typename fld
    -- Ensure we don't accidentally deserialize a @text@ field which
    -- would produce corrupted @lquery@s.
    when (typ /= "lquery") $
      returnError Incompatible fld $ "Expected type lquery, got: " <> show typ
    case mbs of
      Nothing -> returnError UnexpectedNull fld ""
      Just bs -> pure $ LQuery $ Text.decodeUtf8 bs
