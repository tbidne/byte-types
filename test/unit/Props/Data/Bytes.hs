{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'Bytes'.
module Props.Data.Bytes (props) where

import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Internal (Bytes (..), SomeSize (..))
import Data.Bytes.Size (SSize (..), SingSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Props.Generators.Bytes qualified as Gens
import Props.Generators.Size qualified as SGens
import Props.MaxRuns (MaxRuns (..))
import Props.Utils qualified as U
import Props.Verify.Algebra qualified as VAlgebra
import Props.Verify.Conversion (ResultConvs (..))
import Props.Verify.Conversion qualified as VConversion
import Props.Verify.Normalize qualified as VNormalize
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Bytes"
    $ bytesProps <> someSizeProps

bytesProps :: [TestTree]
bytesProps =
  [ unBytesProps,
    convertProps,
    normalizeProps,
    bytesEqProps,
    bytesOrdProps,
    bytesGroupProps,
    bytesVectorSpaceProps
  ]

someSizeProps :: [TestTree]
someSizeProps =
  [ someConvertProps,
    someSizeEqProps,
    someSizeOrdProps,
    someSizeGroupProps,
    someVectorSpaceProps,
    someNormalizeProps
  ]

unBytesProps :: TestTree
unBytesProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes unwrapping + wrap is a no-op" "unBytesProps" $
    H.withTests limit $
      H.property $ do
        (MkSomeSize _ bytes) <- H.forAll Gens.genSomeBytes
        bytes === MkBytes (unwrap bytes)

convertProps :: TestTree
convertProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes Conversions" "convertProps" $
    H.withTests limit $
      H.property $ do
        b <- H.forAll (Gens.genBytes @'B)
        k <- H.forAll (Gens.genBytes @'K)
        m <- H.forAll (Gens.genBytes @'M)
        g <- H.forAll (Gens.genBytes @'G)
        t <- H.forAll (Gens.genBytes @'T)
        p <- H.forAll (Gens.genBytes @'P)
        e <- H.forAll (Gens.genBytes @'E)
        z <- H.forAll (Gens.genBytes @'Z)
        y <- H.forAll (Gens.genBytes @'Y)
        convert b VConversion.convertB
        convert k VConversion.convertK
        convert m VConversion.convertM
        convert g VConversion.convertG
        convert t VConversion.convertT
        convert p VConversion.convertP
        convert e VConversion.convertE
        convert z VConversion.convertZ
        convert y VConversion.convertY

convert ::
  SingSize s =>
  Bytes s Rational ->
  (ResultConvs Rational -> PropertyT IO ()) ->
  PropertyT IO ()
convert bytes@(MkBytes x) convertAndTestFn = do
  let original = x
      bRes = unwrap $ toB bytes
      kRes = unwrap $ toK bytes
      mRes = unwrap $ toM bytes
      gRes = unwrap $ toG bytes
      tRes = unwrap $ toT bytes
      pRes = unwrap $ toP bytes
      eRes = unwrap $ toE bytes
      zRes = unwrap $ toZ bytes
      yRes = unwrap $ toY bytes
  convertAndTestFn MkResultConvs {..}

normalizeProps :: TestTree
normalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes normalizes" "normalizeProps" $
    H.withTests limit $
      H.property $ do
        (MkSomeSize sz bytes) <- H.forAll Gens.genSomeBytes
        let normalized@(MkSomeSize _ (MkBytes x)) = Size.withSingSize sz $ normalize bytes
            label = someSizeToLabel normalized
        H.footnote $ "original: " <> show bytes
        H.footnote $ "normalized: " <> show normalized
        VNormalize.isNormalized label x

bytesEqProps :: TestTree
bytesEqProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes Eq laws" "bytesEqProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.eqLaws x y z

bytesOrdProps :: TestTree
bytesOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes Ord laws" "bytesOrdProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.ordLaws x y z

bytesGroupProps :: TestTree
bytesGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes Group laws" "bytesGroupProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.groupLaws x y z

bytesVectorSpaceProps :: TestTree
bytesVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes Vector Space laws" "bytesVectorSpaceProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someConvertProps :: TestTree
someConvertProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize conversions match underlying Bytes" "someConvertProps" $
    H.withTests limit $
      H.property $ do
        someSize@(MkSomeSize sz bytes) <- H.forAll Gens.genSomeBytes
        toB someSize === Size.withSingSize sz (toB bytes)
        toK someSize === Size.withSingSize sz (toK bytes)
        toM someSize === Size.withSingSize sz (toM bytes)
        toG someSize === Size.withSingSize sz (toG bytes)
        toT someSize === Size.withSingSize sz (toT bytes)
        toP someSize === Size.withSingSize sz (toP bytes)
        toE someSize === Size.withSingSize sz (toE bytes)
        toZ someSize === Size.withSingSize sz (toZ bytes)
        toY someSize === Size.withSingSize sz (toY bytes)

someSizeToLabel :: SomeSize n -> Size
someSizeToLabel (MkSomeSize sz _) = case sz of
  SB -> B
  SK -> K
  SM -> M
  SG -> G
  ST -> T
  SP -> P
  SE -> E
  SZ -> Z
  SY -> Y

someSizeEqProps :: TestTree
someSizeEqProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize Eq laws" "someSizeEqProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.eqLaws x y z

someSizeOrdProps :: TestTree
someSizeOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize Ord laws" "someSizeOrdProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.ordLaws x y z

someSizeGroupProps :: TestTree
someSizeGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize Group laws" "someSizeGroupProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.groupLaws x y z

someVectorSpaceProps :: TestTree
someVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize Vector Space laws" "someVectorSpaceProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someNormalizeProps :: TestTree
someNormalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize normalization" "someNormalizeProps" $
    H.withTests limit $
      H.property $ do
        x@(MkSomeSize szx bytes) <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        k <- H.forAll SGens.genD
        nz <- H.forAll SGens.genNonZero
        -- matches underlying bytes
        normalize x === Size.withSingSize szx (normalize bytes)
        -- laws
        VNormalize.normalizeLaws x y k nz
