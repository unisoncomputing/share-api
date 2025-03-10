{-# LANGUAGE GADTs #-}

-- | Helpers for streamable cursors
module Share.Postgres.Cursors
  ( newRowCursor,
    newColCursor,
    fetchN,
    foldBatched,
    PGCursor,
  )
where

import Data.Char qualified as Char
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Data.UUID (UUID)
import Data.Vector (Vector)
import Share.Postgres
import Share.Prelude
import System.Random (randomIO)

-- | A cursor that can be used to fetch rows from the database.
-- Includes a mapper (CoYoneda) to allow the type to be a functor.
data PGCursor result where
  PGCursor ::
    forall row result.
    (DecodeRow row {- decoder for original row -}) =>
    Text {- cursor name -} ->
    (row -> result {- mapper for Functor instance -}) ->
    PGCursor result

instance Functor PGCursor where
  fmap f (PGCursor name g) = PGCursor name (f . g)

newColCursor :: forall a m. (QueryM m, DecodeField a) => Text -> Sql -> m (PGCursor a)
newColCursor namePrefix query = do
  newRowCursor namePrefix query
    <&> fmap fromOnly

-- | Create a new cursor. The name is only for debugging purposes since it will be munged with
-- a random UUID.
--
-- This cursor will be closed when the transaction ends, and must not be used outside of the
-- transaction in which it was created.
newRowCursor :: forall r m. (QueryM m) => (DecodeRow r) => Text -> Sql -> m (PGCursor r)
newRowCursor namePrefix query =
  do
    uuid <- transactionUnsafeIO $ randomIO @UUID
    -- We can't use a parameter for cursor names; cursor names are hard-coded and don't contain user input,
    -- so we don't need to worry about SQL injection, but filter down the names just to be
    -- safe.
    let cursorName = Text.filter (\c -> Char.isAlphaNum c || c == '_') (namePrefix <> "_" <> into @Text uuid)
    let declaration = fromString $ "DECLARE " <> Text.unpack cursorName <> "\n"
    execute_ $
      declaration
        <> [sql|
      BINARY
      NO SCROLL
      CURSOR
      WITHOUT HOLD
      FOR ^{query}
    |]
    pure $ PGCursor cursorName id

-- | Fetch UP TO the next N results from the cursor. If there are no more rows, returns Nothing.
fetchN :: forall r m. (QueryM m) => PGCursor r -> Int32 -> m (Maybe (Vector r))
fetchN (PGCursor cursorName f) n = do
  -- PG doesn't allow bind params for limits or cursor names.
  -- We're safe from injection here because `n` is just an int, and we guarantee the
  -- cursorName is safe at construction time.
  let sql = fromString . Text.unpack $ Text.intercalate " " ["FETCH FORWARD", tShow n, "FROM", cursorName]
  rows <- queryVectorRows sql
  if null rows
    then do
      execute_ $ fromString $ "CLOSE " <> Text.unpack cursorName
      pure Nothing
    else pure $ Just (f <$> rows)

-- | Fold over the cursor in batches of N rows.
-- N.B. Fold is strict in the accumulator.
foldBatched :: forall r m a. (QueryM m, Monoid a) => PGCursor r -> Int32 -> (Vector r -> m a) -> m a
foldBatched cursor batchSize f = do
  batch <- fetchN cursor batchSize
  case batch of
    Nothing -> pure mempty
    Just rows -> do
      acc <- f rows
      (acc <>) <$!> foldBatched cursor batchSize f
