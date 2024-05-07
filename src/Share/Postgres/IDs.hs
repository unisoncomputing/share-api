{-# LANGUAGE RecordWildCards #-}

-- | We define separate '*HashId' types because HashIds from sqlite and Postgres must never
-- be mistaken for one another, you can't simply convert between them.
module Share.Postgres.IDs
  ( -- We must redefine *Id types to ensure Sqlite HashIDs don't mix with Postgres HashIDs.
    CausalId (..),
    BranchHashId (..),
    ComponentHashId (..),
    PatchId,
    -- Hash types are re-exported from U.Codebase.HashTags because we don't have any reason to define new
    -- ones.
    BranchHash (..),
    PatchHash (..),
    CausalHash (..),
    ComponentHash (..),
    ConstructorId (..),
    TermId (..),
    TypeId (..),
    TextId (..),
    BytesId (..),
    EvalResultId (..),
    NamespaceTermMappingId (..),
    NamespaceTypeMappingId (..),

    -- * Conversions
    hash32AsComponentHash_,
    hash32FromComponentHash_,
    hash32AsPatchHash_,
    hash32FromPatchHash_,
    hash32AsBranchHash_,
    hash32FromBranchHash_,
    hash32AsCausalHash_,
    hash32FromCausalHash_,
  )
where

import Control.Lens
import Share.Postgres qualified as PG
import Share.Prelude
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), ComponentHash (..), PatchHash (..))
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32

newtype CausalId = CausalId {unCausalId :: Int32}
  deriving stock (Eq, Ord, Show)
  deriving (PG.DecodeValue, PG.EncodeValue) via Int32

newtype BranchHashId = BranchHashId {unBranchHashId :: Int32}
  deriving stock (Eq, Ord, Show)
  deriving (PG.DecodeValue, PG.EncodeValue) via Int32

newtype ComponentHashId = ComponentHashId {unComponentHashId :: Int32}
  deriving stock (Eq, Ord, Show)
  deriving (PG.DecodeValue, PG.EncodeValue) via Int32

newtype PatchId = PatchId {unPatchId :: Int32}
  deriving stock (Eq, Ord, Show)
  deriving (PG.DecodeValue, PG.EncodeValue) via Int32

-- | The id of a specific Term in the database, not a term component.
newtype TermId = TermId Int32
  deriving stock (Show, Eq, Ord)
  deriving (PG.EncodeValue, PG.DecodeValue) via Int32

-- | The id of a Type Decl in the database, not a type component.
newtype TypeId = TypeId Int32
  deriving stock (Show, Eq, Ord)
  deriving (PG.EncodeValue, PG.DecodeValue) via Int32

-- | Note: This is the id of a Constructor in the database,
-- which is different from a 'PgConstructorIndex'.
newtype ConstructorId = ConstructorId Int32
  deriving stock (Show, Eq, Ord)
  deriving (PG.EncodeValue, PG.DecodeValue) via Int32

newtype TextId = TextId Int32
  deriving stock (Show, Eq, Ord)
  deriving (PG.EncodeValue, PG.DecodeValue) via Int32

newtype BytesId = BytesId Int32
  deriving stock (Show, Eq, Ord)
  deriving (PG.EncodeValue, PG.DecodeValue) via Int32

newtype EvalResultId = EvalResultId Int32
  deriving stock (Show, Eq, Ord)
  deriving (PG.EncodeValue, PG.DecodeValue) via Int32

newtype NamespaceTermMappingId = NamespaceTermMappingId Int32
  deriving stock (Show, Eq, Ord)
  deriving (PG.EncodeValue, PG.DecodeValue) via Int32

newtype NamespaceTypeMappingId = NamespaceTypeMappingId Int32
  deriving stock (Show, Eq, Ord)
  deriving (PG.EncodeValue, PG.DecodeValue) via Int32

hash32AsComponentHash_ :: Iso Hash32 b ComponentHash b
hash32AsComponentHash_ = iso (ComponentHash . Hash32.toHash) id

hash32FromComponentHash_ :: Iso ComponentHash b Hash32 b
hash32FromComponentHash_ = iso (Hash32.fromHash . unComponentHash) id

hash32AsPatchHash_ :: Iso Hash32 b PatchHash b
hash32AsPatchHash_ = iso (PatchHash . Hash32.toHash) id

hash32FromPatchHash_ :: Iso PatchHash b Hash32 b
hash32FromPatchHash_ = iso (Hash32.fromHash . unPatchHash) id

hash32AsBranchHash_ :: Iso Hash32 b BranchHash b
hash32AsBranchHash_ = iso (BranchHash . Hash32.toHash) id

hash32FromBranchHash_ :: Iso BranchHash b Hash32 b
hash32FromBranchHash_ = iso (Hash32.fromHash . unBranchHash) id

hash32AsCausalHash_ :: Iso Hash32 b CausalHash b
hash32AsCausalHash_ = iso (CausalHash . Hash32.toHash) id

hash32FromCausalHash_ :: Iso CausalHash b Hash32 b
hash32FromCausalHash_ = iso (Hash32.fromHash . unCausalHash) id
