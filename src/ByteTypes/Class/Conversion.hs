{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides typeclasses for convert between byte sizes.
module ByteTypes.Class.Conversion
  ( DecSize (..),
    IncSize (..),
    Conversion (..),
    convert,
    convertWitness,
  )
where

import ByteTypes.Data.Size (NextSize, PrevSize, SSize (..), SingSize (..), Size (..))
import Numeric.Algebra (AMonoid (..), MGroup (..), MSemigroup (..))
import Numeric.Algebra qualified as Algebra
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.NonZero (NonZero (..))

-- | Typeclass for decrementing bytes to the next units.
class DecSize a where
  prev :: a -> PrevSize a

-- | Typeclass for increasing bytes to the next units.
class IncSize a where
  next :: a -> NextSize a

-- | Provides a common interface for converting between byte sizes.
class Conversion a where
  type Converted (b :: Size) a = r | r -> b

  toB :: a -> Converted 'B a
  toK :: a -> Converted 'K a
  toM :: a -> Converted 'M a
  toG :: a -> Converted 'G a
  toT :: a -> Converted 'T a
  toP :: a -> Converted 'P a
  toE :: a -> Converted 'E a
  toZ :: a -> Converted 'Z a
  toY :: a -> Converted 'Y a

-- | Low level function for converting a numeric literal /from/ the inferred
-- 'SingSize' /to/ the parameter 'Size'. For instance,
-- @
-- convertWitness @SK 'M 5_000 == 5
-- @
--
-- This is slightly more principled than 'convert', but the higher level
-- byte types and functions should still be preferred
-- (e.g. 'ByteTypes.Data.Bytes', 'ByteTypes.Class.Normalize').
convertWitness ::
  forall s n.
  ( AMonoid n,
    MGroup n,
    NumLiteral n,
    SingSize s
  ) =>
  Size ->
  n ->
  n
convertWitness toUnits n = case singSize @s of
  SB -> convert B toUnits n
  SK -> convert K toUnits n
  SM -> convert M toUnits n
  SG -> convert G toUnits n
  ST -> convert T toUnits n
  SP -> convert P toUnits n
  SE -> convert E toUnits n
  SZ -> convert Z toUnits n
  SY -> convert Y toUnits n

-- | Low level function for converting a numeric literal between
-- byte sizes. @convert b1 b2@ converts /from/ @b1@ /to/ @b2@,
-- e.g. @convert G K = \\n -> n * 1_000_000@. The higher level
-- byte types and functions should be preferred
-- (e.g. 'ByteTypes.Data.Bytes', 'ByteTypes.Class.Normalize'), but this is
-- here when it is needed.
convert ::
  forall n.
  ( AMonoid n,
    MGroup n,
    NumLiteral n
  ) =>
  Size ->
  Size ->
  n ->
  n
convert B B n = n
convert B K n = n .%. nzFromLit @n 1_000
convert B M n = n .%. nzFromLit @n 1_000_000
convert B G n = n .%. nzFromLit @n 1_000_000_000
convert B T n = n .%. nzFromLit @n 1_000_000_000_000
convert B P n = n .%. nzFromLit @n 1_000_000_000_000_000
convert B E n = n .%. nzFromLit @n 1_000_000_000_000_000_000
convert B Z n = n .%. nzFromLit @n 1_000_000_000_000_000_000_000
convert B Y n = n .%. nzFromLit @n 1_000_000_000_000_000_000_000_000
convert K B n = n .*. fromLit 1_000
convert K K n = n
convert K M n = n .%. nzFromLit @n 1_000
convert K G n = n .%. nzFromLit @n 1_000_000
convert K T n = n .%. nzFromLit @n 1_000_000_000
convert K P n = n .%. nzFromLit @n 1_000_000_000_000
convert K E n = n .%. nzFromLit @n 1_000_000_000_000_000
convert K Z n = n .%. nzFromLit @n 1_000_000_000_000_000_000
convert K Y n = n .%. nzFromLit @n 1_000_000_000_000_000_000_000
convert M B n = n .*. fromLit 1_000_000
convert M K n = n .*. fromLit 1_000
convert M M n = n
convert M G n = n .%. nzFromLit @n 1_000
convert M T n = n .%. nzFromLit @n 1_000_000
convert M P n = n .%. nzFromLit @n 1_000_000_000
convert M E n = n .%. nzFromLit @n 1_000_000_000_000
convert M Z n = n .%. nzFromLit @n 1_000_000_000_000_000
convert M Y n = n .%. nzFromLit @n 1_000_000_000_000_000_000
convert G B n = n .*. fromLit 1_000_000_000
convert G K n = n .*. fromLit 1_000_000
convert G M n = n .*. fromLit 1_000
convert G G n = n
convert G T n = n .%. nzFromLit @n 1_000
convert G P n = n .%. nzFromLit @n 1_000_000
convert G E n = n .%. nzFromLit @n 1_000_000_000
convert G Z n = n .%. nzFromLit @n 1_000_000_000_000
convert G Y n = n .%. nzFromLit @n 1_000_000_000_000_000
convert T B n = n .*. fromLit 1_000_000_000_000
convert T K n = n .*. fromLit 1_000_000_000
convert T M n = n .*. fromLit 1_000_000
convert T G n = n .*. fromLit 1_000
convert T T n = n
convert T P n = n .%. nzFromLit @n 1_000
convert T E n = n .%. nzFromLit @n 1_000_000
convert T Z n = n .%. nzFromLit @n 1_000_000_000
convert T Y n = n .%. nzFromLit @n 1_000_000_000_000
convert P B n = n .*. fromLit 1_000_000_000_000_000
convert P K n = n .*. fromLit 1_000_000_000_000
convert P M n = n .*. fromLit 1_000_000_000
convert P G n = n .*. fromLit 1_000_000
convert P T n = n .*. fromLit 1_000
convert P P n = n
convert P E n = n .%. nzFromLit @n 1_000
convert P Z n = n .%. nzFromLit @n 1_000_000
convert P Y n = n .%. nzFromLit @n 1_000_000_000
convert E B n = n .*. fromLit 1_000_000_000_000_000_000
convert E K n = n .*. fromLit 1_000_000_000_000_000
convert E M n = n .*. fromLit 1_000_000_000_000
convert E G n = n .*. fromLit 1_000_000_000
convert E T n = n .*. fromLit 1_000_000
convert E P n = n .*. fromLit 1_000
convert E E n = n
convert E Z n = n .%. nzFromLit @n 1_000
convert E Y n = n .%. nzFromLit @n 1_000_000
convert Z B n = n .*. fromLit 1_000_000_000_000_000_000_000
convert Z K n = n .*. fromLit 1_000_000_000_000_000_000
convert Z M n = n .*. fromLit 1_000_000_000_000_000
convert Z G n = n .*. fromLit 1_000_000_000_000
convert Z T n = n .*. fromLit 1_000_000_000
convert Z P n = n .*. fromLit 1_000_000
convert Z E n = n .*. fromLit 1_000
convert Z Z n = n
convert Z Y n = n .%. nzFromLit @n 1_000
convert Y B n = n .*. fromLit 1_000_000_000_000_000_000_000_000
convert Y K n = n .*. fromLit 1_000_000_000_000_000_000_000
convert Y M n = n .*. fromLit 1_000_000_000_000_000_000
convert Y G n = n .*. fromLit 1_000_000_000_000_000
convert Y T n = n .*. fromLit 1_000_000_000_000
convert Y P n = n .*. fromLit 1_000_000_000
convert Y E n = n .*. fromLit 1_000_000
convert Y Z n = n .*. fromLit 1_000
convert Y Y n = n

nzFromLit :: forall n. (AMonoid n, NumLiteral n) => Integer -> NonZero n
nzFromLit = Algebra.unsafeAMonoidNonZero . fromLit
