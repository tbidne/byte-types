{-# LANGUAGE RecordWildCards #-}

-- | Exports functions for verifying 'Conversion' properties.
module ByteTypes.Props.Verify.Conversion
  ( ResultConvs (..),
    convertB,
    convertK,
    convertM,
    convertG,
    convertT,
    convertP,
    convertE,
    convertZ,
    convertY,
  )
where

import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Numeric.Algebra (Field, MGroup (..), MSemigroup (..))
import Numeric.Algebra qualified as Algebra
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.NonZero (NonZero)

data ExpectedConvs n = MkExpectedConvs
  { bExp :: n -> n,
    kExp :: n -> n,
    mExp :: n -> n,
    gExp :: n -> n,
    tExp :: n -> n,
    pExp :: n -> n,
    eExp :: n -> n,
    zExp :: n -> n,
    yExp :: n -> n
  }

-- | Record that holds an \"original value\", along with the result of
-- various conversions.
data ResultConvs n = MkResultConvs
  { original :: n,
    bRes :: n,
    kRes :: n,
    mRes :: n,
    gRes :: n,
    tRes :: n,
    pRes :: n,
    eRes :: n,
    zRes :: n,
    yRes :: n
  }

-- | Tests that the \"to bytes\" conversion matches expectations.
convertB :: (Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertB results = do
  let bExp = (.%. nzFromLit 1)
      kExp = (.%. nzFromLit 1_000)
      mExp = (.%. nzFromLit 1_000_000)
      gExp = (.%. nzFromLit 1_000_000_000)
      tExp = (.%. nzFromLit 1_000_000_000_000)
      pExp = (.%. nzFromLit 1_000_000_000_000_000)
      eExp = (.%. nzFromLit 1_000_000_000_000_000_000)
      zExp = (.%. nzFromLit 1_000_000_000_000_000_000_000)
      yExp = (.%. nzFromLit 1_000_000_000_000_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to kilobytes\" conversion matches expectations.
convertK :: (Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertK results = do
  let bExp = (.*. fromLit 1_000)
      kExp = id
      mExp = (.%. nzFromLit 1_000)
      gExp = (.%. nzFromLit 1_000_000)
      tExp = (.%. nzFromLit 1_000_000_000)
      pExp = (.%. nzFromLit 1_000_000_000_000)
      eExp = (.%. nzFromLit 1_000_000_000_000_000)
      zExp = (.%. nzFromLit 1_000_000_000_000_000_000)
      yExp = (.%. nzFromLit 1_000_000_000_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to megabytes\" conversion matches expectations.
convertM :: (Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertM results = do
  let bExp = (.*. fromLit 1_000_000)
      kExp = (.*. fromLit 1_000)
      mExp = id
      gExp = (.%. nzFromLit 1_000)
      tExp = (.%. nzFromLit 1_000_000)
      pExp = (.%. nzFromLit 1_000_000_000)
      eExp = (.%. nzFromLit 1_000_000_000_000)
      zExp = (.%. nzFromLit 1_000_000_000_000_000)
      yExp = (.%. nzFromLit 1_000_000_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to gigabytes\" conversion matches expectations.
convertG :: (Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertG results = do
  let bExp = (.*. fromLit 1_000_000_000)
      kExp = (.*. fromLit 1_000_000)
      mExp = (.*. fromLit 1_000)
      gExp = id
      tExp = (.%. nzFromLit 1_000)
      pExp = (.%. nzFromLit 1_000_000)
      eExp = (.%. nzFromLit 1_000_000_000)
      zExp = (.%. nzFromLit 1_000_000_000_000)
      yExp = (.%. nzFromLit 1_000_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to terabytes\" conversion matches expectations.
convertT :: (Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertT results = do
  let bExp = (.*. fromLit 1_000_000_000_000)
      kExp = (.*. fromLit 1_000_000_000)
      mExp = (.*. fromLit 1_000_000)
      gExp = (.*. fromLit 1_000)
      tExp = id
      pExp = (.%. nzFromLit 1_000)
      eExp = (.%. nzFromLit 1_000_000)
      zExp = (.%. nzFromLit 1_000_000_000)
      yExp = (.%. nzFromLit 1_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to petabytes\" conversion matches expectations.
convertP :: (Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertP results = do
  let bExp = (.*. fromLit 1_000_000_000_000_000)
      kExp = (.*. fromLit 1_000_000_000_000)
      mExp = (.*. fromLit 1_000_000_000)
      gExp = (.*. fromLit 1_000_000)
      tExp = (.*. fromLit 1_000)
      pExp = id
      eExp = (.%. nzFromLit 1_000)
      zExp = (.%. nzFromLit 1_000_000)
      yExp = (.%. nzFromLit 1_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to exabytes\" conversion matches expectations.
convertE :: (Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertE results = do
  let bExp = (.*. fromLit 1_000_000_000_000_000_000)
      kExp = (.*. fromLit 1_000_000_000_000_000)
      mExp = (.*. fromLit 1_000_000_000_000)
      gExp = (.*. fromLit 1_000_000_000)
      tExp = (.*. fromLit 1_000_000)
      pExp = (.*. fromLit 1_000)
      eExp = id
      zExp = (.%. nzFromLit 1_000)
      yExp = (.%. nzFromLit 1_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to zettabytes\" conversion matches expectations.
convertZ :: (Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertZ results = do
  let bExp = (.*. fromLit 1_000_000_000_000_000_000_000)
      kExp = (.*. fromLit 1_000_000_000_000_000_000)
      mExp = (.*. fromLit 1_000_000_000_000_000)
      gExp = (.*. fromLit 1_000_000_000_000)
      tExp = (.*. fromLit 1_000_000_000)
      pExp = (.*. fromLit 1_000_000)
      eExp = (.*. fromLit 1_000)
      zExp = id
      yExp = (.%. nzFromLit 1_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to yottabytes\" conversion matches expectations.
convertY :: (Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertY results = do
  let bExp = (.*. fromLit 1_000_000_000_000_000_000_000_000)
      kExp = (.*. fromLit 1_000_000_000_000_000_000_000)
      mExp = (.*. fromLit 1_000_000_000_000_000_000)
      gExp = (.*. fromLit 1_000_000_000_000_000)
      tExp = (.*. fromLit 1_000_000_000_000)
      pExp = (.*. fromLit 1_000_000_000)
      eExp = (.*. fromLit 1_000_000)
      zExp = (.*. fromLit 1_000)
      yExp = id

  convert MkExpectedConvs {..} results

convert :: (Eq n, Show n) => ExpectedConvs n -> ResultConvs n -> PropertyT IO ()
convert MkExpectedConvs {..} MkResultConvs {..} = do
  convertAndTest bExp original bRes "B"
  convertAndTest kExp original kRes "K"
  convertAndTest mExp original mRes "M"
  convertAndTest gExp original gRes "G"
  convertAndTest tExp original tRes "T"
  convertAndTest pExp original pRes "P"
  convertAndTest eExp original eRes "E"
  convertAndTest zExp original zRes "Z"
  convertAndTest yExp original yRes "Y"

convertAndTest ::
  (Eq t, Show t) =>
  (s -> t) ->
  s ->
  t ->
  String ->
  PropertyT IO ()
convertAndTest convFn original result label = do
  let expected = convFn original
  H.footnote label
  expected === result

nzFromLit :: (Field n, NumLiteral n) => Integer -> NonZero n
nzFromLit = Algebra.unsafeAMonoidNonZero . fromLit
