{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (bracket, throw, throwIO, try)
import Control.Monad.Logger.CallStack (logWarn, runStderrLoggingT)
import Data.Function ((&))
import Data.Functor ((<&>), void)
import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Only(..), SqlError(sqlErrorMsg), Connection, close, connectPostgreSQL, execute_, query
  )
import Database.PostgreSQL.Simple.LTree (mkLabel)
import Test.Hspec (describe, expectationFailure, hspec, it)
import Test.QuickCheck (Gen, arbitrary, property, suchThat)
import Test.QuickCheck.Monadic (forAllM, monadicIO, run)

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text
import qualified Database.Postgres.Temp as TmpPostgres

main :: IO ()
main = withTmpPostgres $ \conn -> hspec $ do
  describe "mkLabel" $ do
    it "should only produce valid labels" $
      property $ monadicIO $ forAllM genPGText $ \t ->
        run $ mkLabel t & either
          (const $ verifyTextCannotBeALabel conn t)
          (const $ verifyTextCanBeALabel conn t)
  where
  withTmpPostgres f = do
    either throwIO pure =<<
      TmpPostgres.with
        (\db ->
            bracket
            (connectPostgreSQL $ TmpPostgres.toConnectionString db)
            close
            (\conn -> do
                _ <- execute_ conn "create extension ltree"
                f conn
            )
        )

  verifyTextCanBeALabel :: Connection -> Text -> IO ()
  verifyTextCanBeALabel conn t = tryConvertTextToLabelInPG conn t >>= \case
    Left msg -> expectationFailure msg
    Right () -> pure ()

  -- Making this throw an expectationFailure is a bit too strong of a test.
  -- It's probably ok if 'mkLabel' is too strict; it's mostly important that
  -- it's not too lenient. We'll just log a warning for cases found in which
  -- 'mkLabel' is too strict.
  verifyTextCannotBeALabel :: Connection -> Text -> IO ()
  verifyTextCannotBeALabel conn t = tryConvertTextToLabelInPG conn t >>= \case
    Left _ -> pure ()
    Right () -> runStderrLoggingT $
      logWarn $ "PostgreSQL unexpectedly parsed label: " <> Text.pack (show t)

  tryConvertTextToLabelInPG :: Connection -> Text -> IO (Either String ())
  tryConvertTextToLabelInPG conn t =
    try (void $ query @_ @[Int] conn "select ?::ltree where false" $ Only ltree)
      <&>
      Bifunctor.first
        (\(e :: SqlError) ->
          if "syntax error at position " `C8.isPrefixOf` sqlErrorMsg e then
            "Failed to parse label via PostgreSQL: " <> show t
          else
            throw e)
    where
    -- Produce an ltree by joining a label with itself
    ltree = t <> "." <> t

  genAnyText :: Gen Text
  genAnyText = Text.pack <$> arbitrary

  genPGText :: Gen Text
  genPGText = genAnyText `suchThat` (not . Text.isInfixOf "\0")
