-- | This module provides types for hiding the
-- 'ByteTypes.Data.Direction.Direction' on
-- 'ByteTypes.Data.Network.NetBytes'.
--
-- This paradigm differs from the previous size hiding
-- 'ByteTypes.Data.Bytes.SomeSize' and
-- 'ByteTypes.Data.Network.SomeNetSize' in that because we had
-- sensible ways to convert between sizes (e.g. K -> G), we could
-- combine arbitrary byte types by first converting to a common type.
--
-- Here, there is no sensible way to convert between uploaded and downloaded
-- byte directions by design. These units are meant to be kept separate.
-- While the witnesses allows us to recover the types at will (and we can
-- \"forget\" the direction tag by dropping to 'ByteTypes.Data.Bytes'),
-- we are much more limited in what we can do. For example, we lose instances
-- like 'Applicative', 'Simple.Algebra.Group'.
module ByteTypes.Data.Network.SomeNetDir
  ( SomeNetDir,
    hideNetDir,
    SomeNet,
    hideNetSizeDir,
  )
where

import ByteTypes.Data.Network.SomeNetDir.Internal
  ( SomeNet,
    SomeNetDir,
    hideNetDir,
    hideNetSizeDir,
  )
