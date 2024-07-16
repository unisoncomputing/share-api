{-# LANGUAGE GADTs #-}

-- | Helpers for streamable cursors
module Share.Postgres.Cursors
  ( newRowCursor,
    newColCursor,
    fetchN,
    PGCursor,
  )
where

import Data.List.NonEmpty qualified as NEL
import Data.UUID (UUID)
import Share.Postgres
import Share.Prelude
import System.Random (randomIO)

-- | A cursor that can be used to fetch rows from the database.
-- Includes a mapper (CoYoneda) to allow the type to be a functor.
data PGCursor result where
  PGCursor ::
    forall row result.
    DecodeRow row {- decoder for original row -} =>
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
newRowCursor :: forall r m. QueryM m => DecodeRow r => Text -> Sql -> m (PGCursor r)
newRowCursor namePrefix query = do
  uuid <- transactionUnsafeIO $ randomIO @UUID
  let cursorName = namePrefix <> "_" <> into @Text uuid
  execute_
    [sql|
    DECLARE #{uuid}
      NO SCROLL
      CURSOR
      WITHOUT HOLD
      FOR ^{query}
    |]
  pure $ PGCursor cursorName id

-- | Fetch UP TO the next N results from the cursor. If there are no more rows, returns Nothing.
fetchN :: forall r m. QueryM m => Int32 -> PGCursor r -> m (Maybe (NonEmpty r))
fetchN n (PGCursor cursorName f) = do
  rows <-
    queryListRows
      [sql| FETCH FORWARD #{n} FROM #{cursorName}
    |]
  pure $ NEL.nonEmpty (f <$> rows)
