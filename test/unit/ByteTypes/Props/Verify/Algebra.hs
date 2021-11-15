-- | Exports functions for verifying algebraic laws
module ByteTypes.Props.Verify.Algebra
  ( -- * Verifying laws
    eqLaws,
    ordLaws,
    groupLaws,
    ringLaws,
    fieldLaws,
    moduleLaws,
    vectorSpaceLaws,
  )
where

import ByteTypes.Utils ((<=>), (==>))
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Numeric.Algebra
  ( AGroup (..),
    AMonoid (..),
    ASemigroup (..),
    Field,
    MGroup (..),
    MMonoid (..),
    MSemigroup (..),
    Module (..),
    Ring,
    VectorSpace (..),
  )
import Numeric.Algebra qualified as Algebra
import Numeric.Data.NonZero (NonZero (..))

-- | Verifies 'Eq' laws for 'BytesEq'.
eqLaws :: (Eq a, Show a) => a -> a -> a -> PropertyT IO ()
eqLaws x y z = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z
  -- reflexivity, symmetry, transitivity
  H.assert $ x == x
  H.assert $ x == y <=> y == x
  H.assert $ x == y && y == z ==> x == z

-- | Verifies 'Ord' laws for 'BytesOrd'.
ordLaws :: (Ord a, Show a) => a -> a -> a -> PropertyT IO ()
ordLaws x y z = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z
  -- reflexivity, transitivity, antisymmetry
  H.assert $ x <= x
  H.assert $ x <= y && y <= z ==> x <= z
  H.assert $ x <= y && y <= x <=> x == y
  -- additional laws
  H.assert $ x <= y <=> y >= x
  H.assert $ x < y <=> x <= y && x /= y
  H.assert $ x < y <=> y > x

-- | Verify 'Group' laws.
groupLaws :: (AGroup a, Show a) => a -> a -> a -> PropertyT IO ()
groupLaws x y z = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z
  -- identity
  x === x .+. zero
  x === zero .+. x
  -- associativity
  H.annotateShow (x .+. y, (x .+. y) .+. z)
  H.annotateShow (y .+. z, x .+. (y .+. z))
  (x .+. y) .+. z === x .+. (y .+. z)

-- | Verify 'Ring' laws.
ringLaws :: (Ring a, Show a) => a -> a -> a -> PropertyT IO ()
ringLaws x y z = do
  groupLaws x y z
  -- group commutativity
  x .+. y === y .+. x
  -- associativity
  (x .*. y) .*. z === x .*. (y .*. z)
  -- identity
  x === x .*. one
  x === one .*. x
  -- distributivity
  x .*. (y .+. z) === (x .*. y) .+. (x .*. z)
  (y .+. z) .*. x === (y .*. x) .+. (z .*. x)

-- | Verify 'Field' laws.
fieldLaws :: (Field a, Show a) => NonZero a -> a -> a -> PropertyT IO ()
fieldLaws x'@(MkNonZero x) y z = do
  ringLaws x y z
  -- identity
  one === x .%. x'

-- | Verify 'Module' laws.
moduleLaws :: forall m r. (Module m r, Show m) => m -> m -> r -> r -> PropertyT IO ()
moduleLaws x y k l = do
  -- left-distributivity
  k *. (x .+. y) === (k *. x) .+. (k *. y)
  (k .+. l) *. x === (k *. x) .+. (l *. x)

  -- right-distributivity
  (x .+. y) .* k === (x .* k) .+. (y .* k)
  x .* (k .+. l) === (x .* k) .+. (x .* l)

  (k .*. l) *. x === l *. (k *. x)

  -- identity
  x === one @r *. x
  x === x .* one @r

-- | Verify 'VectorSpace' laws.
vectorSpaceLaws ::
  (VectorSpace v k, Show v) =>
  v ->
  v ->
  NonZero k ->
  NonZero k ->
  PropertyT IO ()
vectorSpaceLaws x y k'@(MkNonZero k) l'@(MkNonZero l) = do
  moduleLaws x y k l

  x .% Algebra.unsafeAMonoidNonZero (k .*. l) === (x .% k') .% l'

  -- identity
  x === one *. x
  x === x .* one
