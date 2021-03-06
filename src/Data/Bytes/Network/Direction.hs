-- | Provides the 'Direction' type and singletons.
--
-- @since 0.1
module Data.Bytes.Network.Direction
  ( -- * Direction Tags
    Direction (..),
    SDirection (..),
    SingDirection (..),
    Directed (..),
    withSingDirection,
    sdirectionToDirection,
  )
where

import Data.Bytes.Class.Parser (Parser (..))
import Data.Kind (Constraint, Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Tags for differentiating downloaded vs. uploaded bytes.
--
-- @since 0.1
type Direction :: Type
data Direction
  = -- | @since 0.1
    Down
  | -- | @since 0.1
    Up
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Parser Direction where
  parser =
    MP.choice
      [ parseU Up 'u' "p",
        parseU Down 'd' "own"
      ]
    where
      parseU u ushort ulong = do
        _ <- MPC.char' ushort
        _ <- MP.optional (MPC.string' ulong)
        pure u
  {-# INLINEABLE parser #-}

-- | Singleton for 'Direction'.
--
-- @since 0.1
type SDirection :: Direction -> Type
data SDirection (d :: Direction) where
  -- | @since 0.1
  SDown :: SDirection Down
  -- | @since 0.1
  SUp :: SDirection Up

-- | @since 0.1
deriving stock instance Show (SDirection d)

-- | @since 0.1
sdirectionToDirection :: SDirection d -> Direction
sdirectionToDirection SDown = Down
sdirectionToDirection SUp = Up
{-# INLINEABLE sdirectionToDirection #-}

-- | @since 0.1
instance TestEquality SDirection where
  testEquality x y = case (x, y) of
    (SDown, SDown) -> Just Refl
    (SUp, SUp) -> Just Refl
    _ -> Nothing
  {-# INLINEABLE testEquality #-}

-- | Typeclass for recovering the 'Direction' at runtime.
--
-- @since 0.1
type SingDirection :: Direction -> Constraint
class SingDirection (d :: Direction) where
  -- | @since 0.1
  singDirection :: SDirection d

-- | @since 0.1
instance SingDirection Down where
  singDirection = SDown
  {-# INLINE singDirection #-}

-- | @since 0.1
instance SingDirection Up where
  singDirection = SUp
  {-# INLINE singDirection #-}

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingDirection d => r@ without explicitly pattern-matching
-- every time.
--
-- @since 0.1
withSingDirection :: SDirection d -> (SingDirection d => r) -> r
withSingDirection s x = case s of
  SDown -> x
  SUp -> x
{-# INLINEABLE withSingDirection #-}

-- | Types that have a direction.
--
-- ==== __Examples__
--
-- >>> import Data.Bytes.Network
-- >>> directionOf (MkNetBytesP @Up @G 7)
-- Up
--
-- >>> directionOf (hideNetSizeDir $ MkNetBytesP @Down @M 100)
-- Down
--
-- @since 0.1
class Directed a where
  -- | Retrieve the direction.
  --
  -- @since 0.1
  directionOf :: a -> Direction
