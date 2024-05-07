-- | Helpers for streamable cursors
module Share.Postgres.Cursors
  ( newCursor,
    fetchNRows,
    fetchNCols,
    PGCursor,
  )
where

import Data.List.NonEmpty qualified as NEL
import Data.UUID (UUID)
import Share.Postgres
import Share.Prelude
import System.Random (randomIO)

data PGCursor result = PGCursor Text

-- | Create a new cursor. The name is only for debugging purposes since it will be munged with
-- a random UUID.
--
-- This cursor will be closed when the transaction ends, and must not be used outside of the 
-- transaction in which it was created.
newCursor :: Text -> Sql -> Transaction e (PGCursor r)
newCursor namePrefix query = do
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
  pure $ PGCursor cursorName

-- | Fetch UP TO the next N rows from the cursor. If there are no more rows, returns Nothing.
fetchNRows :: DecodeRow r => PGCursor r -> Int32 -> Transaction e (Maybe (NonEmpty r))
fetchNRows (PGCursor cursorName) n = do
  rows <-
    queryListRows
      [sql| FETCH FORWARD #{n} FROM #{cursorName}
    |]
  pure $ NEL.nonEmpty rows

-- | Fetch UP TO the next N single-column rows from the cursor. If there are no more rows, returns Nothing.
fetchNCols :: DecodeField r => PGCursor r -> Int32 -> Transaction e (Maybe (NonEmpty r))
fetchNCols (PGCursor cursorName) n = do
  rows <-
    queryListCol
      [sql| FETCH FORWARD #{n} FROM #{cursorName}
    |]
  pure $ NEL.nonEmpty rows
