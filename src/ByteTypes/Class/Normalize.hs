-- | Provides the 'Normalize' typeclass.
module ByteTypes.Class.Normalize
  ( Normalize (..),
  )
where

-- | Used for normalizing bytes @b@ such that
--
-- \[
--  1 \le \text{normalize}(b) < 1000 \iff 1\text{ B} \le b < 1000\text{ P}.
-- \]
--
-- In the strictest sense, \(\textrm{normalize} : \mathcal{C} \rightarrow \mathcal{C}\)
-- is not a homomorphism, as the combination of two normalized values may
-- itself not be normalized.
--
-- However, because the normalized units varies with the value, @normalize@
-- always returns a type that existentially quantifies the size
-- (e.g. 'BytesTypes.Data.Bytes.SomeSize'). 'Eq' for these types is defined in
-- terms of an equivalence class that takes units into account, e.g.,
-- @1 P = 1,000 T = 1,000,000 G ...@. Viewed this way, @normalize@ is actually
-- an /isomorphism/, as it is essentially a no-op, never leaving the
-- equivalence class.
--
-- This means we can happily mix normalization with different functions
-- without worrying about the order. The only requirement we have is that
-- such functions respect substitution:
--
-- \[ x = y \implies f(x) = f(y). \]
--
-- This is certainly true for all the usual mathematical operations we would
-- normally use, e.g., 'Simple.Algebra.Group' addition,
-- 'ByteTypes.Class.Algebra.Module' scalar multiplication. On
-- the other hand, any functions that inspect the underlying numeric value or
-- Bytes types could easily break this law. As such they should be treated
-- with suspicion, at least when used in conjunction with 'normalize'.
--
-- The other consideration we must keep in mind is that the final result of a
-- series of computations may not be normalized. If this is desired, then
-- @normalize@ should be the /last/ operation performed. Using @normalize@ in
-- the middle would not cause any harm (other than, perhaps, impacting
-- efficiency), but it would not guarantee the final result is normalized.
class Normalize a where
  type Norm a
  normalize :: a -> Norm a
