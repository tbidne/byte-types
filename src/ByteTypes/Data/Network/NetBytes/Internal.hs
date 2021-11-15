{-# LANGUAGE UndecidableInstances #-}

-- | Provides the core alternative to 'ByteTypes.Data.Bytes', for when there
-- is a need to distinguish between downloaded and uploaded bytes.
--
-- This module differs from 'ByteTypes.Data.Network.NetBytes' only in that
-- the former exports the real constructor 'MkNetBytes' and corresponding
-- 'unNetBytes'. That 'NetBytes' is implemented in terms of
-- 'ByteTypes.Data.Bytes' is largely an implementation detail, thus
-- 'MkNetBytesP' and 'unNetBytesP' should be preferred.
--
-- The primary difference between this \"Internal\" module and the \"public\"
-- one, "ByteTypes.Data.Network.NetBytes", is that this module exports
-- functions and constructors that allow one to recover the 'Size'. For
-- example, we expose 'netToSSize' and 'SomeNetSize'\'s actual constructor,
-- 'MkSomeNetSize', which includes a runtime witness 'SSize'. These are hidden
-- by default as they complicate the API, and the latter can be used to break
-- 'SomeNetSize'\'s equivalence-class based 'Eq'.
module ByteTypes.Data.Network.NetBytes.Internal
  ( -- * Network Bytes
    NetBytes (.., MkNetBytesP),
    unNetBytesP,
    netToSSize,
    netToSDirection,

    -- * Unknown Size
    SomeNetSize (..),
    hideNetSize,
    someNetSizeToSDirection,
  )
where

import ByteTypes.Class.Conversion (Conversion (..))
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Data.Bytes.Internal (Bytes (..), SomeSize (..))
import ByteTypes.Data.Direction
  ( Direction (..),
    SDirection (..),
    SingDirection (..),
  )
import ByteTypes.Data.Size (SSize (..), SingSize (..), Size (..))
import ByteTypes.Data.Size qualified as Size
import Control.Applicative (liftA2)
import Data.Kind (Type)
import GHC.Show qualified as Show
import Numeric.Algebra
  ( AGroup (..),
    AMonoid (..),
    ASemigroup (..),
    Field,
    Module (..),
    Ring,
    VectorSpace (..),
  )
import Numeric.Class.Literal (NumLiteral (..))

-- | Wrapper around the 'Bytes' type that adds the 'Direction' tag.
type NetBytes :: Direction -> Size -> Type -> Type
newtype NetBytes d s n = MkNetBytes
  { -- | Unwraps the 'NetBytes'.
    unNetBytes :: Bytes s n
  }

-- | Convenience function using 'MkNetBytesP'.
unNetBytesP :: NetBytes d s n -> n
unNetBytesP (MkNetBytesP x) = x

-- | Pattern for de/constructing 'NetBytes'.
pattern MkNetBytesP :: n -> NetBytes d s n
pattern MkNetBytesP x <-
  MkNetBytes (MkBytes x)
  where
    MkNetBytesP x = MkNetBytes (MkBytes x)

{-# COMPLETE MkNetBytesP #-}

-- | Retrieves the 'SDirection' witness. Can be used to recover the
-- 'Direction'.
netToSDirection :: SingDirection d => NetBytes d s n -> SDirection d
netToSDirection _ = singDirection

-- | Retrieves the 'SingSize' witness. Can be used to recover the 'Size'.
netToSSize :: SingSize s => NetBytes d s n -> SSize s
netToSSize _ = singSize

instance Show n => Show (NetBytes d s n) where
  showsPrec p (MkNetBytesP x) =
    showParen (p > Show.appPrec) $
      showString "MkNetBytesP {unNetBytesP = "
        . showsPrec Show.appPrec1 x
        . showString "}"

deriving instance Functor (NetBytes d s)

instance Applicative (NetBytes d s) where
  pure = MkNetBytes . pure
  MkNetBytes f <*> MkNetBytes x = MkNetBytes $ f <*> x

instance Monad (NetBytes d s) where
  MkNetBytes x >>= f = MkNetBytes $ x >>= (unNetBytes . f)

instance Eq n => Eq (NetBytes d s n) where
  MkNetBytes x == MkNetBytes y = x == y

instance Ord n => Ord (NetBytes d s n) where
  MkNetBytes x <= MkNetBytes y = x <= y

instance ASemigroup n => ASemigroup (NetBytes d s n) where
  (.+.) = liftA2 (.+.)

instance AMonoid n => AMonoid (NetBytes d s n) where
  zero = MkNetBytes zero

instance AGroup n => AGroup (NetBytes d s n) where
  (.-.) = liftA2 (.-.)
  aabs = fmap aabs

instance Ring n => Module (NetBytes d s n) n where
  MkNetBytes x .* k = MkNetBytes $ x .* k

instance Field n => VectorSpace (NetBytes d s n) n where
  MkNetBytes x .% k = MkNetBytes $ x .% k

instance (Field n, NumLiteral n, SingSize s) => Conversion (NetBytes d s n) where
  type Converted 'B (NetBytes d s n) = NetBytes d 'B n
  type Converted 'K (NetBytes d s n) = NetBytes d 'K n
  type Converted 'M (NetBytes d s n) = NetBytes d 'M n
  type Converted 'G (NetBytes d s n) = NetBytes d 'G n
  type Converted 'T (NetBytes d s n) = NetBytes d 'T n
  type Converted 'P (NetBytes d s n) = NetBytes d 'P n
  type Converted 'E (NetBytes d s n) = NetBytes d 'E n
  type Converted 'Z (NetBytes d s n) = NetBytes d 'Z n
  type Converted 'Y (NetBytes d s n) = NetBytes d 'Y n

  toB (MkNetBytes b) = MkNetBytes $ toB b
  toK (MkNetBytes b) = MkNetBytes $ toK b
  toM (MkNetBytes b) = MkNetBytes $ toM b
  toG (MkNetBytes b) = MkNetBytes $ toG b
  toT (MkNetBytes b) = MkNetBytes $ toT b
  toP (MkNetBytes b) = MkNetBytes $ toP b
  toE (MkNetBytes b) = MkNetBytes $ toE b
  toZ (MkNetBytes b) = MkNetBytes $ toZ b
  toY (MkNetBytes b) = MkNetBytes $ toY b

instance (Field n, NumLiteral n, Ord n, SingSize s) => Normalize (NetBytes d s n) where
  type Norm (NetBytes d s n) = SomeNetSize d n

  normalize (MkNetBytes bytes) = case normalize bytes of
    MkSomeSize sz bytes' -> MkSomeNetSize sz $ MkNetBytes bytes'

instance
  forall d s n.
  (PrettyPrint n, SingDirection d, SingSize s) =>
  PrettyPrint (NetBytes d s n)
  where
  pretty (MkNetBytes x) = case singDirection @d of
    SDown -> pretty x <> " Down"
    SUp -> pretty x <> " Up"

-- | Wrapper for 'NetBytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return, e.g.,
--
-- @
--   getUpTraffic :: IO (SomeNetSize Up Float)
--   getUpTraffic = do
--     (bytes, units) <- getUpTrafficRaw
--     case units of
--       \"B\" -> MkSomeNetSize SUp $ MkNetBytesP \@B bytes
--       \"K\" -> MkSomeNetSize SUp $ MkNetBytesP \@K bytes
--       ...
-- @
--
-- 'SomeNetSize' carries along an 'SSize' runtime witness for when we
-- need the size. Its 'Group' functions are 'normalize'd.
--
-- N.B. 'SomeNetSize'\'s instances for lawful typeclasses (e.g. 'Eq', 'Ord',
-- 'Group') are themselves lawful w.r.t. the notion of equivalence defined
-- in its 'Eq' instance.
type SomeNetSize :: Direction -> Type -> Type
data SomeNetSize d n where
  MkSomeNetSize :: SSize s -> NetBytes d s n -> SomeNetSize d n

-- | Wraps a 'Bytes' in an existentially quantified 'SomeSize'.
hideNetSize :: forall d s n. SingSize s => NetBytes d s n -> SomeNetSize d n
hideNetSize bytes = case singSize @s of
  SB -> MkSomeNetSize SB bytes
  SK -> MkSomeNetSize SK bytes
  SM -> MkSomeNetSize SM bytes
  SG -> MkSomeNetSize SG bytes
  ST -> MkSomeNetSize ST bytes
  SP -> MkSomeNetSize SP bytes
  SE -> MkSomeNetSize SE bytes
  SZ -> MkSomeNetSize SZ bytes
  SY -> MkSomeNetSize SY bytes

deriving instance Show n => Show (SomeNetSize d n)

deriving instance Functor (SomeNetSize d)

-- | Note: This instance defines an equivalence relation on 'SomeNetSize' that
-- takes units into account. For instance,
--
-- @
-- MkSomeNetSize SK (MkNetBytesP 1000) == MkSomeNetSize SM (MkNetBytesP 1).
-- @
--
-- Because we expose the underlying @NetBytes@ in several ways (e.g. 'Show',
-- the 'SSize' witness), this is technically unlawful for equality
-- as it breaks the substitutivity law:
--
-- \[
-- x = y \implies f(x) = f(y).
-- \]
--
-- For instance:
--
-- @
-- let x = MkSomeNetSize SK (MkNetBytesP 1000)
-- let y = MkSomeNetSize SM (MkNetBytesP 1)
-- x == y
-- isK x /= isK y
-- @
--
-- With apologies to Leibniz, such comparisons are too useful to ignore
-- and enable us to implement other lawful classes (e.g. 'Group') that respect
-- this notion of equivalence.
instance (Eq n, Field n, NumLiteral n) => Eq (SomeNetSize d n) where
  x == y = toB x == toB y

instance (Field n, NumLiteral n, Ord n) => Ord (SomeNetSize d n) where
  x <= y = toB x <= toB y

instance (Field n, NumLiteral n, Ord n) => ASemigroup (SomeNetSize d n) where
  x .+. y = normalize $ toB x .+. toB y

instance (Field n, NumLiteral n, Ord n) => AMonoid (SomeNetSize d n) where
  zero = MkSomeNetSize SB zero

instance (Field n, NumLiteral n, Ord n) => AGroup (SomeNetSize d n) where
  x .-. y = normalize $ toB x .-. toB y
  aabs = fmap aabs

instance (Field n, NumLiteral n, Ord n) => Module (SomeNetSize d n) n where
  MkSomeNetSize sz x .* k = MkSomeNetSize sz $ x .* k

instance (Field n, NumLiteral n, Ord n) => VectorSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .% k = MkSomeNetSize sz $ x .% k

instance (Field n, NumLiteral n) => Conversion (SomeNetSize d n) where
  type Converted 'B (SomeNetSize d n) = NetBytes d 'B n
  type Converted 'K (SomeNetSize d n) = NetBytes d 'K n
  type Converted 'M (SomeNetSize d n) = NetBytes d 'M n
  type Converted 'G (SomeNetSize d n) = NetBytes d 'G n
  type Converted 'T (SomeNetSize d n) = NetBytes d 'T n
  type Converted 'P (SomeNetSize d n) = NetBytes d 'P n
  type Converted 'E (SomeNetSize d n) = NetBytes d 'E n
  type Converted 'Z (SomeNetSize d n) = NetBytes d 'Z n
  type Converted 'Y (SomeNetSize d n) = NetBytes d 'Y n

  toB (MkSomeNetSize sz x) = Size.withSingSize sz $ toB x
  toK (MkSomeNetSize sz x) = Size.withSingSize sz $ toK x
  toM (MkSomeNetSize sz x) = Size.withSingSize sz $ toM x
  toG (MkSomeNetSize sz x) = Size.withSingSize sz $ toG x
  toT (MkSomeNetSize sz x) = Size.withSingSize sz $ toT x
  toP (MkSomeNetSize sz x) = Size.withSingSize sz $ toP x
  toE (MkSomeNetSize sz x) = Size.withSingSize sz $ toE x
  toZ (MkSomeNetSize sz x) = Size.withSingSize sz $ toZ x
  toY (MkSomeNetSize sz x) = Size.withSingSize sz $ toY x

instance (Field n, NumLiteral n, Ord n) => Normalize (SomeNetSize d n) where
  type Norm (SomeNetSize d n) = SomeNetSize d n
  normalize (MkSomeNetSize sz x) = Size.withSingSize sz $ normalize x

instance (PrettyPrint n, SingDirection d) => PrettyPrint (SomeNetSize d n) where
  pretty (MkSomeNetSize sz b) = Size.withSingSize sz $ pretty b

-- | Retrieves the 'SingDirection' witness. Can be used to recover the
-- 'Direction'.
someNetSizeToSDirection :: SingDirection d => SomeNetSize d n -> SDirection d
someNetSizeToSDirection _ = singDirection
