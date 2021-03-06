{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Size' type and typeclasses for converting
-- between units.
--
-- @since 0.1
module Data.Bytes.Size
  ( -- * Size Tags
    Size (..),
    SSize (..),
    SingSize (..),
    withSingSize,
    ssizeToSize,

    -- * Sized Types
    Sized (..),

    -- * Type Families for Relating Tags
    NextSize,
    PrevSize,
  )
where

import Control.Applicative ((<|>))
import Data.Bytes.Class.Parser (Parser (..))
import Data.Kind (Constraint, Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Byte units.
--
-- @since 0.1
type Size :: Type
data Size
  = -- | Bytes
    --
    -- @since 0.1
    B
  | -- | Kilobytes
    --
    -- @since 0.1
    K
  | -- | Megabytes
    --
    -- @since 0.1
    M
  | -- | Gigabytes
    --
    -- @since 0.1
    G
  | -- | Terabytes
    --
    -- @since 0.1
    T
  | -- | Petabytes
    --
    -- @since 0.1
    P
  | -- | Exabytes
    --
    -- @since 0.1
    E
  | -- | Zettabytes
    --
    -- @since 0.1
    Z
  | -- | Yottabytes
    --
    -- @since 0.1
    Y
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Parser Size where
  parser =
    MP.choice
      [ parseB,
        parseU K 'k' "ilobytes",
        parseU M 'm' "egabytes",
        parseU G 'g' "igabytes",
        parseU T 't' "erabytes",
        parseU P 'p' "etabytes",
        parseU E 'e' "xabytes",
        parseU Z 'z' "ettabytes",
        parseU Y 'y' "ottabytes"
      ]
    where
      parseB = do
        _ <- MPC.char' 'b'
        _ <- MP.optional (MPC.string' "ytes")
        pure B
      parseU u ushort ulong = do
        _ <- MPC.char' ushort
        _ <- MP.optional (MP.try (MPC.string' "b") <|> MPC.string' ulong)
        pure u
  {-# INLINEABLE parser #-}

-- | Singleton for 'Size'.
--
-- @since 0.1
type SSize :: Size -> Type
data SSize (s :: Size) where
  -- | @since 0.1
  SB :: SSize B
  -- | @since 0.1
  SK :: SSize K
  -- | @since 0.1
  SM :: SSize M
  -- | @since 0.1
  SG :: SSize G
  -- | @since 0.1
  ST :: SSize T
  -- | @since 0.1
  SP :: SSize P
  -- | @since 0.1
  SE :: SSize E
  -- | @since 0.1
  SZ :: SSize Z
  -- | @since 0.1
  SY :: SSize Y

-- | @since 0.1
ssizeToSize :: SSize s -> Size
ssizeToSize SB = B
ssizeToSize SK = K
ssizeToSize SM = M
ssizeToSize SG = G
ssizeToSize ST = T
ssizeToSize SP = P
ssizeToSize SE = E
ssizeToSize SZ = Z
ssizeToSize SY = Y
{-# INLINEABLE ssizeToSize #-}

-- | @since 0.1
instance TestEquality SSize where
  testEquality x y = case (x, y) of
    (SB, SB) -> Just Refl
    (SK, SK) -> Just Refl
    (SM, SM) -> Just Refl
    (SG, SG) -> Just Refl
    (ST, ST) -> Just Refl
    (SP, SP) -> Just Refl
    (SE, SE) -> Just Refl
    (SZ, SZ) -> Just Refl
    (SY, SY) -> Just Refl
    _ -> Nothing
  {-# INLINEABLE testEquality #-}

-- | @since 0.1
deriving stock instance Show (SSize s)

-- | Typeclass for recovering the 'Size' at runtime.
--
-- @since 0.1
type SingSize :: Size -> Constraint
class SingSize (s :: Size) where
  -- | @since 0.1
  singSize :: SSize s

-- | @since 0.1
instance SingSize B where
  singSize = SB
  {-# INLINE singSize #-}

-- | @since 0.1
instance SingSize K where
  singSize = SK
  {-# INLINE singSize #-}

-- | @since 0.1
instance SingSize M where
  singSize = SM
  {-# INLINE singSize #-}

-- | @since 0.1
instance SingSize G where
  singSize = SG
  {-# INLINE singSize #-}

-- | @since 0.1
instance SingSize T where
  singSize = ST
  {-# INLINE singSize #-}

-- | @since 0.1
instance SingSize P where
  singSize = SP
  {-# INLINE singSize #-}

-- | @since 0.1
instance SingSize E where
  singSize = SE
  {-# INLINE singSize #-}

-- | @since 0.1
instance SingSize Z where
  singSize = SZ
  {-# INLINE singSize #-}

-- | @since 0.1
instance SingSize Y where
  singSize = SY
  {-# INLINE singSize #-}

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingSize d => r@ without explicitly pattern-matching
-- every time.
--
-- @since 0.1
withSingSize :: SSize s -> (SingSize s => r) -> r
withSingSize s x = case s of
  SB -> x
  SK -> x
  SM -> x
  SG -> x
  ST -> x
  SP -> x
  SE -> x
  SZ -> x
  SY -> x
{-# INLINEABLE withSingSize #-}

-- | Type family that relates units to the next larger one.
--
-- ==== __Examples__
--
-- >>> :kind! NextSize 'M
-- NextSize 'M :: Size
-- = 'G
--
-- >>> :kind! NextSize 'Y
-- NextSize 'Y :: Size
-- = (TypeError ...)
--
-- @since 0.1
type NextSize :: Size -> Size
type family NextSize (s :: Size) = (t :: Size) where
  NextSize B = K
  NextSize K = M
  NextSize M = G
  NextSize G = T
  NextSize T = P
  NextSize P = E
  NextSize E = Z
  NextSize Z = Y
  NextSize Y = TypeError ('Text "The byte unit Y does not have a 'next size'.")

-- | Type family that relates units to the previous smaller one.
--
-- ==== __Examples__
--
-- >>> :kind! PrevSize 'M
-- PrevSize 'M :: Size
-- = 'K
--
-- >>> :kind! PrevSize 'B
-- PrevSize 'B :: Size
-- = (TypeError ...)
--
-- @since 0.1
type PrevSize :: Size -> Size
type family PrevSize (s :: Size) = (t :: Size) where
  PrevSize B = TypeError ('Text "The byte unit B does not have a 'previous size'.")
  PrevSize K = B
  PrevSize M = K
  PrevSize G = M
  PrevSize T = G
  PrevSize P = T
  PrevSize E = P
  PrevSize Z = E
  PrevSize Y = Z

-- | Types that have a size.
--
-- ==== __Examples__
--
-- >>> import Data.Bytes
-- >>> sizeOf (MkBytes @G 7)
-- G
--
-- >>> sizeOf (hideSize $ MkBytes @M 7)
-- M
--
-- @since 0.1
class Sized a where
  -- | Retrieves the size.
  --
  -- @since 0.1
  sizeOf :: a -> Size
