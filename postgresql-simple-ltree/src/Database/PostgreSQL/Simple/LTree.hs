-- | This module is a wrapper for PostgreSQL's @ltree@ https://www.postgresql.org/docs/current/ltree.html
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.PostgreSQL.Simple.LTree
  ( module Database.PostgreSQL.LTree
  ) where

import Prelude hiding (map, null)

import Database.PostgreSQL.LTree

import Control.Monad (when)
import Database.PostgreSQL.Simple.FromField
  ( FromField(fromField), ResultError(Incompatible, UnexpectedNull), returnError, typename
  )
import Database.PostgreSQL.Simple.ToField (ToField(toField))

import qualified Data.Text.Encoding as Text

instance ToField LTree where
  toField = toField . render

instance FromField LTree where
  fromField fld mbs = do
    -- There might be a more efficient way to check this, need to see
    -- if the @ltree@ type has a stable typoid or not.
    typ <- typename fld
    -- Ensure we don't accidentally deserialize a @text@ field which
    -- would produce corrupted @label@s.
    when (typ /= "ltree") $
      returnError Incompatible fld $ "Expected type ltree, got: " <> show typ
    case mbs of
      Nothing -> returnError UnexpectedNull fld ""
      -- Since this is coming from postgres and we've confirmed the type matches
      -- @ltree@, it is safe to use 'unsafeUncheckedParse' here.
      Just bs -> pure $ unsafeUncheckedParse $ Text.decodeUtf8 bs
