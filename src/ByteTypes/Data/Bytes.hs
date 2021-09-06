-- | The main entry point to the library. Provides the types and classes for
-- working with different byte sizes (e.g. B, KB, MB ...). See
-- 'ByteTypes.Data.Network' if there is a need to distinguish between
-- downloaded and uploaded bytes.
module ByteTypes.Data.Bytes
  ( -- * Bytes
    Bytes (..),
    unBytes,
    bytesToSByteSize,

    -- * Unknown Size
    AnySize (..),
  )
where

import ByteTypes.Class.Conversion
  ( Conversion (..),
    DecByteSize (..),
    IncByteSize (..),
  )
import ByteTypes.Class.Conversion qualified as Conv
import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Group (Group (..))
import ByteTypes.Class.Math.Algebra.Module (Module (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Algebra.VectorSpace (VectorSpace (..))
import ByteTypes.Class.Math.Literal (NumLiteral (..))
import ByteTypes.Class.Math.Scalar.Num (ScalarNum (..))
import ByteTypes.Class.Math.Scalar.Ord (ScalarEq (..), ScalarOrd (..))
import ByteTypes.Class.Math.Scalar.Scalar (Scalar)
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Data.Size
  ( ByteSize (..),
    NextSize,
    PrevSize,
    SByteSize (..),
    SingByteSize (..),
  )
import ByteTypes.Data.Size qualified as Size
import Control.Applicative (liftA2)
import Data.Kind (Type)
import Text.Printf (PrintfArg (..))
import Text.Printf qualified as Pf

-- | This is the core type for handling type-safe byte operations. It is
-- intended to be used as a simple wrapper over some numerical type,
-- equipped with a unit tag. If the units are unknown they can be recovered
-- at runtime via 'bytesToSByteSize'.
--
-- To take full advantage of the API (e.g. `normalize`), the underlying
-- numerical type should implement 'Field'.
type Bytes :: ByteSize -> Type -> Type
data Bytes s n where
  MkBytes :: n -> Bytes s n

-- | Unwraps the 'Bytes'.
unBytes :: Bytes s n -> n
unBytes (MkBytes x) = x

-- | Changes the 'ByteSize' tag.
resizeBytes :: Bytes s n -> Bytes t n
resizeBytes (MkBytes x) = MkBytes x

-- | Retrieves the 'SByteSize' witness.
bytesToSByteSize :: SingByteSize s => Bytes s n -> SByteSize s
bytesToSByteSize _ = singByteSize

deriving instance Show n => Show (Bytes s n)

deriving instance Functor (Bytes s)

instance Applicative (Bytes s) where
  pure = MkBytes
  MkBytes f <*> MkBytes x = MkBytes $ f x

instance Monad (Bytes s) where
  MkBytes x >>= f = f x

instance Eq n => Eq (Bytes s n) where
  MkBytes x == MkBytes y = x == y

instance Ord n => Ord (Bytes s n) where
  MkBytes x <= MkBytes y = x <= y

type instance Scalar (Bytes s n) = n

instance Eq n => ScalarEq (Bytes s n) where
  MkBytes x .= k = x == k

instance Ord n => ScalarOrd (Bytes s n) where
  MkBytes x .<= k = x <= k

instance Ring n => ScalarNum (Bytes s n) where
  MkBytes x .+ k = MkBytes $ x .+. k
  MkBytes x .- k = MkBytes $ x .-. k

instance Group n => Group (Bytes s n) where
  (.+.) = liftA2 (.+.)
  (.-.) = liftA2 (.-.)
  gid = MkBytes gid
  ginv = fmap ginv
  gabs = fmap gabs

instance Ring n => Module (Bytes s n) n where
  MkBytes x .* k = MkBytes $ x .*. k

instance Field n => VectorSpace (Bytes s n) n where
  MkBytes x .% k = MkBytes $ x .%. k

instance (Field n, NumLiteral n, SingByteSize s) => Conversion (Bytes s n) where
  type Converted 'B (Bytes s n) = Bytes 'B n
  type Converted 'KB (Bytes s n) = Bytes 'KB n
  type Converted 'MB (Bytes s n) = Bytes 'MB n
  type Converted 'GB (Bytes s n) = Bytes 'GB n
  type Converted 'TB (Bytes s n) = Bytes 'TB n
  type Converted 'PB (Bytes s n) = Bytes 'PB n

  toB (MkBytes x) = MkBytes $ Conv.convertWitness @s B x
  toKB (MkBytes x) = MkBytes $ Conv.convertWitness @s KB x
  toMB (MkBytes x) = MkBytes $ Conv.convertWitness @s MB x
  toGB (MkBytes x) = MkBytes $ Conv.convertWitness @s GB x
  toTB (MkBytes x) = MkBytes $ Conv.convertWitness @s TB x
  toPB (MkBytes x) = MkBytes $ Conv.convertWitness @s PB x

type instance NextSize (Bytes 'B n) = Bytes 'KB n

type instance NextSize (Bytes 'KB n) = Bytes 'MB n

type instance NextSize (Bytes 'MB n) = Bytes 'GB n

type instance NextSize (Bytes 'GB n) = Bytes 'TB n

type instance NextSize (Bytes 'TB n) = Bytes 'PB n

instance (Field n, NumLiteral n) => IncByteSize (Bytes 'B n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

instance (Field n, NumLiteral n) => IncByteSize (Bytes 'KB n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

instance (Field n, NumLiteral n) => IncByteSize (Bytes 'MB n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

instance (Field n, NumLiteral n) => IncByteSize (Bytes 'GB n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

instance (Field n, NumLiteral n) => IncByteSize (Bytes 'TB n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

type instance PrevSize (Bytes 'KB n) = Bytes 'B n

type instance PrevSize (Bytes 'MB n) = Bytes 'KB n

type instance PrevSize (Bytes 'GB n) = Bytes 'MB n

type instance PrevSize (Bytes 'TB n) = Bytes 'GB n

type instance PrevSize (Bytes 'PB n) = Bytes 'TB n

instance (NumLiteral n, Ring n) => DecByteSize (Bytes 'KB n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (NumLiteral n, Ring n) => DecByteSize (Bytes 'MB n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (NumLiteral n, Ring n) => DecByteSize (Bytes 'GB n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (NumLiteral n, Ring n) => DecByteSize (Bytes 'TB n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (NumLiteral n, Ring n) => DecByteSize (Bytes 'PB n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (Field n, NumLiteral n, Ord n, SingByteSize s) => Normalize (Bytes s n) where
  type Norm (Bytes s n) = AnySize n

  normalize bytes =
    case bytesToSByteSize bytes of
      SB
        | absBytes .< fromLit 1_000 -> MkAnySize SB bytes
        | otherwise -> normalize $ next bytes
      SPB
        | absBytes .>= fromLit 1 -> MkAnySize SPB bytes
        | otherwise -> normalize $ prev bytes
      SKB
        | absBytes .< fromLit 1 -> normalize $ prev bytes
        | absBytes .>= fromLit 1_000 -> normalize $ next bytes
        | otherwise -> MkAnySize sz bytes
      SMB
        | absBytes .< fromLit 1 -> normalize $ prev bytes
        | absBytes .>= fromLit 1_000 -> normalize $ next bytes
        | otherwise -> MkAnySize sz bytes
      SGB
        | absBytes .< fromLit 1 -> normalize $ prev bytes
        | absBytes .>= fromLit 1_000 -> normalize $ next bytes
        | otherwise -> MkAnySize sz bytes
      STB
        | absBytes .< fromLit 1 -> normalize $ prev bytes
        | absBytes .>= fromLit 1_000 -> normalize $ next bytes
        | otherwise -> MkAnySize sz bytes
    where
      sz = bytesToSByteSize bytes
      absBytes = gabs bytes

instance (PrintfArg n, SingByteSize s) => PrettyPrint (Bytes s n) where
  pretty (MkBytes x) = case singByteSize @s of
    SB -> Pf.printf "%.2f" x <> " B"
    SKB -> Pf.printf "%.2f" x <> " KB"
    SMB -> Pf.printf "%.2f" x <> " MB"
    SGB -> Pf.printf "%.2f" x <> " GB"
    STB -> Pf.printf "%.2f" x <> " TB"
    SPB -> Pf.printf "%.2f" x <> " PB"

-- | Wrapper for 'Bytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return, e.g.,
--
-- @
--   getFileSize :: IO (AnySize Float)
--   getFileSize path = do
--     (bytes, units) <- getRawFileSize path
--     case units of
--       "B" -> MkAnySize SB $ MkB bytes
--       "KB" -> MkAnySize SKB $ MkKB bytes
--       ...
-- @
--
-- 'AnySize' carries along an 'SByteSize' runtime witness for when we
-- need the size. Its 'Group' functions are 'normalize'd.
--
-- N.B. 'AnySize'\'s instances for lawful typeclasses (e.g. 'Eq', 'Ord',
-- 'Group') are themselves lawful w.r.t. the notion of equivalence defined
-- in its 'Eq' instance.
type AnySize :: Type -> Type
data AnySize n where
  MkAnySize :: SByteSize s -> Bytes s n -> AnySize n

deriving instance Show n => Show (AnySize n)

deriving instance Functor AnySize

-- | Note: This instance defines an equivalence relation on 'AnySize' that
-- takes units into account. For instance,
--
-- @
-- MkAnySize SKB (MkBytes 1000) == MkAnySize SMB (MkBytes 1).
-- @
--
-- Because we expose the underlying @Bytes@ in several ways (e.g. 'Show',
-- the 'SByteSize' witness), this is technically unlawful for equality
-- as it breaks the substitutivity law:
--
-- \[
-- x = y \implies f(x) = f(y).
-- \]
--
-- For instance:
--
-- @
-- let x = MkAnySize SKB (MkBytes 1000)
-- let y = MkAnySize SMB (MkBytes 1)
-- x == y
-- isKB x /= isKB y
-- @
--
-- With apologies to Leibniz, such comparisons are too useful to ignore
-- and enable us to implement other lawful classes (e.g. 'Group') that respect
-- this notion of equivalence.
instance (Eq n, Field n, NumLiteral n) => Eq (AnySize n) where
  x == y = toB x == toB y

-- | Like the 'Eq' instance, this instance compares both the numeric value
-- __and__ label, so that, e.g.,
--
-- @
-- MkAnySize SKB (MkBytes 5_000) <= MkAnySize SMB (MkBytes 8)
-- MkAnySize SMB (MkBytes 2) <= MkAnySize SKB (MkBytes 5_000)
-- @
instance (Field n, NumLiteral n, Ord n) => Ord (AnySize n) where
  x <= y = toB x <= toB y

instance (Field n, NumLiteral n, Ord n) => Group (AnySize n) where
  x .+. y = normalize $ toB x .+. toB y
  x .-. y = normalize $ toB x .-. toB y
  gid = MkAnySize SB gid
  ginv = fmap ginv
  gabs = fmap gabs

instance (Field n, NumLiteral n, Ord n) => Module (AnySize n) n where
  MkAnySize sz x .* k = MkAnySize sz $ x .* k

instance (Field n, NumLiteral n, Ord n) => VectorSpace (AnySize n) n where
  MkAnySize sz x .% k = MkAnySize sz $ x .% k

instance (Field n, NumLiteral n) => Conversion (AnySize n) where
  type Converted 'B (AnySize n) = Bytes 'B n
  type Converted 'KB (AnySize n) = Bytes 'KB n
  type Converted 'MB (AnySize n) = Bytes 'MB n
  type Converted 'GB (AnySize n) = Bytes 'GB n
  type Converted 'TB (AnySize n) = Bytes 'TB n
  type Converted 'PB (AnySize n) = Bytes 'PB n

  toB (MkAnySize sz x) = Size.withSingByteSize sz $ toB x
  toKB (MkAnySize sz x) = Size.withSingByteSize sz $ toKB x
  toMB (MkAnySize sz x) = Size.withSingByteSize sz $ toMB x
  toGB (MkAnySize sz x) = Size.withSingByteSize sz $ toGB x
  toTB (MkAnySize sz x) = Size.withSingByteSize sz $ toTB x
  toPB (MkAnySize sz x) = Size.withSingByteSize sz $ toPB x

instance (Field n, NumLiteral n, Ord n) => Normalize (AnySize n) where
  type Norm (AnySize n) = AnySize n
  normalize (MkAnySize sz x) = Size.withSingByteSize sz $ normalize x

instance PrintfArg n => PrettyPrint (AnySize n) where
  pretty (MkAnySize sz b) = Size.withSingByteSize sz $ pretty b
