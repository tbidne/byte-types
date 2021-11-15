{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'Bytes'.
module ByteTypes.Props.Data.Bytes (props) where

import ByteTypes.Class.Conversion
  ( Conversion (..),
    DecSize (..),
    IncSize (..),
  )
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Data.Bytes.Internal (Bytes (..), SomeSize (..))
import ByteTypes.Data.Size (SSize (..), SingSize (..), Size (..))
import ByteTypes.Data.Size qualified as Size
import ByteTypes.Props.Data.Bytes.Generators qualified as Gens
import ByteTypes.Props.Data.Size.Generators qualified as SGens
import ByteTypes.Props.MaxRuns (MaxRuns (..))
import ByteTypes.Props.Verify.Algebra qualified as VAlgebra
import ByteTypes.Props.Verify.Conversion (ResultConvs (..))
import ByteTypes.Props.Verify.Conversion qualified as VConversion
import ByteTypes.Props.Verify.Normalize qualified as VNormalize
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Numeric.Algebra (Field, MGroup (..), MSemigroup (..))
import Numeric.Algebra qualified as Algebra
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.NonZero (NonZero)
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

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
    incProps,
    decProps,
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
  TH.testProperty "Bytes unwrapping + wrap is a no-op" $
    H.withTests limit $
      H.property $ do
        (MkSomeSize _ bytes) <- H.forAll Gens.genSomeBytes
        bytes === MkBytes (Bytes.unBytes bytes)

convertProps :: TestTree
convertProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Conversions" $
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
      bRes = Bytes.unBytes $ toB bytes
      kRes = Bytes.unBytes $ toK bytes
      mRes = Bytes.unBytes $ toM bytes
      gRes = Bytes.unBytes $ toG bytes
      tRes = Bytes.unBytes $ toT bytes
      pRes = Bytes.unBytes $ toP bytes
      eRes = Bytes.unBytes $ toE bytes
      zRes = Bytes.unBytes $ toZ bytes
      yRes = Bytes.unBytes $ toY bytes
  convertAndTestFn MkResultConvs {..}

incProps :: TestTree
incProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes increasing label reduces size by 1,000" $
    H.withTests limit $
      H.property $ do
        (MkSomeSize sz bytes@(MkBytes x)) <- H.forAll Gens.genNormalizedBytes
        let (expected, result) :: (Rational, Rational) = case sz of
              SY -> (x, Bytes.unBytes bytes)
              SB -> Size.withSingSize sz (x .%. divisor, Bytes.unBytes (next bytes))
              SK -> Size.withSingSize sz (x .%. divisor, Bytes.unBytes (next bytes))
              SM -> Size.withSingSize sz (x .%. divisor, Bytes.unBytes (next bytes))
              SG -> Size.withSingSize sz (x .%. divisor, Bytes.unBytes (next bytes))
              ST -> Size.withSingSize sz (x .%. divisor, Bytes.unBytes (next bytes))
              SP -> Size.withSingSize sz (x .%. divisor, Bytes.unBytes (next bytes))
              SE -> Size.withSingSize sz (x .%. divisor, Bytes.unBytes (next bytes))
              SZ -> Size.withSingSize sz (x .%. divisor, Bytes.unBytes (next bytes))
        H.footnote $ "expected: " <> show expected
        H.footnote $ " result: " <> show result
        result === expected
  where
    divisor :: NonZero Rational
    divisor = nzFromLit 1_000

decProps :: TestTree
decProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes decreasing label multiplies size by 1,000" $
    H.withTests limit $
      H.property $ do
        (MkSomeSize sz bytes@(MkBytes x)) <- H.forAll Gens.genNormalizedBytes
        let (expected, result) :: (Rational, Rational) = case sz of
              SB -> (x, Bytes.unBytes bytes)
              SK -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SM -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SG -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              ST -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SP -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SE -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SZ -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SY -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
        H.footnote $ "expected: " <> show expected
        H.footnote $ " result: " <> show result
        result === expected

normalizeProps :: TestTree
normalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes normalizes" $
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
  TH.testProperty "Bytes Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.eqLaws x y z

bytesOrdProps :: TestTree
bytesOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Ord laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.ordLaws x y z

bytesGroupProps :: TestTree
bytesGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Group laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.groupLaws x y z

bytesVectorSpaceProps :: TestTree
bytesVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Vector Space laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someConvertProps :: TestTree
someConvertProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeSize conversions match underlying Bytes" $
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
  TH.testProperty "SomeSize Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.eqLaws x y z

someSizeOrdProps :: TestTree
someSizeOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeSize Ord laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.ordLaws x y z

someSizeGroupProps :: TestTree
someSizeGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeSize Group laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.groupLaws x y z

someVectorSpaceProps :: TestTree
someVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeSize Vector Space laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someNormalizeProps :: TestTree
someNormalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeSize normalization" $
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

nzFromLit :: (Field n, NumLiteral n) => Integer -> NonZero n
nzFromLit = Algebra.unsafeAMonoidNonZero . fromLit
