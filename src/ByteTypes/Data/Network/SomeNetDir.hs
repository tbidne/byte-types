-- | This module provides types for hiding the
-- 'ByteTypes.Data.Direction.Direction' on 'NetBytes'.
--
-- This paradigm differs from the previous size hiding
-- 'ByteTypes.Data.Bytes.SomeSize' and 'SomeNetSize' in that because we had
-- sensible ways to convert between sizes (e.g. K -> G), we could
-- combine arbitrary byte types by first converting to a common type.
--
-- Here, there is no sensible way to convert between uploaded and downloaded
-- byte directions by design. These units are meant to be kept separate.
-- While the witnesses allows us to recover the types at will (and we can
-- \"forget\" the direction tag by dropping to 'ByteTypes.Data.Bytes'),
-- we are much more limited in what we can do. For example, we lose instances
-- like 'Applicative', 'ByteTypes.Class.Math.Algebra.Group'.
module ByteTypes.Data.Network.SomeNetDir
  ( SomeNetDir (..),
    SomeNet (..),
  )
where

import ByteTypes.Class.Conversion (Conversion (..))
import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Literal (NumLiteral (..))
import ByteTypes.Class.Math.Scalar.Num (ScalarNum (..))
import ByteTypes.Class.Math.Scalar.Ord (ScalarEq (..), ScalarOrd (..))
import ByteTypes.Class.Math.Scalar.Scalar (Scalar)
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Data.Direction (SDirection (..))
import ByteTypes.Data.Direction qualified as Direction
import ByteTypes.Data.Network.NetBytes (NetBytes, SomeNetSize (..))
import ByteTypes.Data.Size (SSize (..), SingSize (..), Size (..))
import ByteTypes.Data.Size qualified as Size
import Data.Kind (Type)

-- | Wrapper for 'NetBytes', existentially quantifying the direction.
-- This is useful when a function does not know a priori what
-- direction it should return,
-- e.g.,
--
-- @
--   getMaxTraffic :: IO (SomeNetDir K Double)
--   getMaxTraffic = do
--     (bytes, direction) <- getMaxTrafficRaw
--     case direction of
--       "Down" -> MkSomeNetDir SDown $ MkBytes bytes
--       "Up" -> MkSomeNetDir SUp $ MkBytes bytes
--       ...
-- @
--
-- We deliberately do not provide instances for SomeX classes that could be
-- used to combine arbitrary 'SomeNetDir's (e.g. 'Applicative',
-- 'ByteTypes.Class.Math.Algebra.Group'), as that would defeat the purpose of
-- enforcing the distinction between upload and downloaded bytes.
type SomeNetDir :: Size -> Type -> Type
data SomeNetDir s n where
  MkSomeNetDir :: SDirection d -> NetBytes d s n -> SomeNetDir s n

deriving instance Show n => Show (SomeNetDir s n)

deriving instance Functor (SomeNetDir s)

-- | Returns true when the two @SomeNetDir@ have the same @Direction@
-- /and/ the underlying @NetBytes@ has the same value.
--
-- @
-- MkSomeNetDir SK (MkNetBytes @Up (MkBytes 1000)) == MkSomeNetDir SM (MkNetBytes @Up (MkBytes 1))
-- MkSomeNetDir SK (MkNetBytes @Up (MkBytes 1000)) /= MkSomeNetDir SM (MkNetBytes @Down (MkBytes 1))
-- @
--
-- Notice no 'Ord' instance is provided, as we provide no ordering for
-- 'ByteTypes.Data.Direction.Direction'.
instance (Eq n, Field n, NumLiteral n, SingSize s) => Eq (SomeNetDir s n) where
  MkSomeNetDir dx x == MkSomeNetDir dy y =
    case (dx, dy) of
      (SDown, SDown) -> x == y
      (SUp, SUp) -> x == y
      _ -> False

type instance Scalar (SomeNetDir s n) = n

instance Eq n => ScalarEq (SomeNetDir s n) where
  MkSomeNetDir _ x .= k = x .= k

instance Ord n => ScalarOrd (SomeNetDir s n) where
  MkSomeNetDir _ x .<= k = x .<= k

instance Ring n => ScalarNum (SomeNetDir s n) where
  MkSomeNetDir sz x .+ k = MkSomeNetDir sz $ x .+ k
  MkSomeNetDir sz x .- k = MkSomeNetDir sz $ x .- k

instance (Field n, NumLiteral n, SingSize s) => Conversion (SomeNetDir s n) where
  type Converted 'B (SomeNetDir s n) = SomeNetDir 'B n
  type Converted 'K (SomeNetDir s n) = SomeNetDir 'K n
  type Converted 'M (SomeNetDir s n) = SomeNetDir 'M n
  type Converted 'G (SomeNetDir s n) = SomeNetDir 'G n
  type Converted 'T (SomeNetDir s n) = SomeNetDir 'T n
  type Converted 'P (SomeNetDir s n) = SomeNetDir 'P n

  toB (MkSomeNetDir dir x) = MkSomeNetDir dir $ toB x
  toK (MkSomeNetDir dir x) = MkSomeNetDir dir $ toK x
  toM (MkSomeNetDir dir x) = MkSomeNetDir dir $ toM x
  toG (MkSomeNetDir dir x) = MkSomeNetDir dir $ toG x
  toT (MkSomeNetDir dir x) = MkSomeNetDir dir $ toT x
  toP (MkSomeNetDir dir x) = MkSomeNetDir dir $ toP x

instance (Field n, NumLiteral n, Ord n, SingSize s) => Normalize (SomeNetDir s n) where
  type Norm (SomeNetDir s n) = SomeNet n
  normalize (MkSomeNetDir dir x) =
    case normalize x of
      MkSomeNetSize sz y -> MkSomeNet dir sz y

instance (PrettyPrint n, SingSize s) => PrettyPrint (SomeNetDir s n) where
  pretty (MkSomeNetDir dir x) =
    Direction.withSingDirection dir $ pretty x

-- | Wrapper for 'NetBytes', existentially quantifying the size /and/
-- direction. This is useful when a function does not know a priori what
-- size or direction it should return,
-- e.g.,
--
-- @
--   getMaxTraffic :: IO (SomeNet Double)
--   getMaxTraffic = do
--     (bytes, direction, size) <- getMaxTrafficRaw
--     case (direction, size) of
--       ("Down", "K" -> MkSomeNet SDown SK $ MkBytes bytes
--       ("Up", "M") -> MkSomeNet SUp SM $ MkBytes bytes
--       ...
-- @
--
-- 'SomeNet' carries along 'SDirection' and 'SSize' runtime witnesses
-- for recovering the 'ByteTypes.Data.Direction.Direction'
-- and 'Size', respectively.
--
-- N.B. 'SomeNetSize'\'s instances for lawful typeclasses (e.g. 'Eq', 'Ord')
-- are themselves lawful w.r.t. the notion of equivalence defined
-- in its 'Eq' instance.
type SomeNet :: Type -> Type
data SomeNet n where
  MkSomeNet :: SDirection d -> SSize s -> NetBytes d s n -> SomeNet n

deriving instance Show n => Show (SomeNet n)

deriving instance Functor SomeNet

-- | Note: This instance uses the same equivalence relation from 'SomeNetSize'
-- w.r.t the size, and also includes an equality check on the direction.
-- Thus we have, for instance,
--
-- @
-- MkSomeNet 'Up 'K (MkNetBytes 1_000) == MkSomeNet 'Up 'M (MkNetBytes 1)
-- MkSomeNet 'Up 'K (MkNetBytes 1_000) /= MkSomeNet 'Down 'M (MkNetBytes 1)
-- @
instance (Eq n, Field n, NumLiteral n) => Eq (SomeNet n) where
  MkSomeNet dx szx x == MkSomeNet dy szy y =
    Size.withSingSize szx $
      Size.withSingSize szy $
        case (dx, dy) of
          (SDown, SDown) -> toB x == toB y
          (SUp, SUp) -> toB x == toB y
          _ -> False

instance (Field n, NumLiteral n) => Conversion (SomeNet n) where
  type Converted 'B (SomeNet n) = SomeNetDir 'B n
  type Converted 'K (SomeNet n) = SomeNetDir 'K n
  type Converted 'M (SomeNet n) = SomeNetDir 'M n
  type Converted 'G (SomeNet n) = SomeNetDir 'G n
  type Converted 'T (SomeNet n) = SomeNetDir 'T n
  type Converted 'P (SomeNet n) = SomeNetDir 'P n

  toB (MkSomeNet dir sz x) = Size.withSingSize sz $ toB (MkSomeNetDir dir x)
  toK (MkSomeNet dir sz x) = Size.withSingSize sz $ toK (MkSomeNetDir dir x)
  toM (MkSomeNet dir sz x) = Size.withSingSize sz $ toM (MkSomeNetDir dir x)
  toG (MkSomeNet dir sz x) = Size.withSingSize sz $ toG (MkSomeNetDir dir x)
  toT (MkSomeNet dir sz x) = Size.withSingSize sz $ toT (MkSomeNetDir dir x)
  toP (MkSomeNet dir sz x) = Size.withSingSize sz $ toP (MkSomeNetDir dir x)

instance (Field n, NumLiteral n, Ord n) => Normalize (SomeNet n) where
  type Norm (SomeNet n) = SomeNet n
  normalize (MkSomeNet dir sz x) =
    case Size.withSingSize sz normalize x of
      MkSomeNetSize sz' x' -> MkSomeNet dir sz' x'

instance PrettyPrint n => PrettyPrint (SomeNet n) where
  pretty (MkSomeNet dir sz x) =
    Direction.withSingDirection dir $
      Size.withSingSize sz $ pretty x
