-- #############################################################################
-- #
-- # (c) Copyright 2004, Bluespec Incorporated and Donald G. Baltus.
-- #
-- # Time-stamp: <2004-12-09 12:53:10 baltus>
-- #
-- # $Id$
-- # $Source:  $
-- #
-- #############################################################################

-- #############################################################################
-- #
-- #############################################################################

module RandSeq where

-- #############################################################################
-- #
-- #############################################################################

import Random
import Maybe
--import Trace

-- #############################################################################
-- #
-- #############################################################################

type RandValue = Maybe (Integer)

type RandSeq = [RandValue]

-- #############################################################################
-- #
-- #############################################################################

randSeqIsConstant :: RandSeq -> Bool
randSeqIsConstant seq =
    let min = (randSeqMinimum seq)
	max = (randSeqMaximum seq)
    in (isNothing min) || ((min == max) && (not (elem Nothing seq)))

randSeqNothingCount :: RandSeq -> Integer
randSeqNothingCount seq =
    let zow Nothing = 1
	zow _ = 0
    in (foldl1 (+) (map zow seq))

randSeqMinimum :: RandSeq -> RandValue
randSeqMinimum seq = (foldl1 randValueMin seq)

randSeqMaximum :: RandSeq -> RandValue
randSeqMaximum seq = (foldl1 randValueMax seq)

randValueMin :: RandValue -> RandValue -> RandValue
randValueMin (Just x) (Just y) = (Just (min x y))
randValueMin (Just x) _ = (Just x)
randValueMin _ (Just y) = (Just y)
randValueMin _ _ = Nothing

randValueMax :: RandValue -> RandValue -> RandValue
randValueMax (Just x) (Just y) = (Just (max x y))
randValueMax (Just x) _ = (Just x)
randValueMax _ (Just y) = (Just y)
randValueMax _ _ = Nothing

-- #############################################################################
-- #
-- #############################################################################

createRandSeq :: Int -> Integer -> Int -> IO RandSeq
createRandSeq length init bits =
    do seq <- (createRandomIntegerList init length)
       let zow x = (Just (mod x (max 1 (2::Integer)^bits)))
       return (map zow seq)

-- #############################################################################
-- #
-- #############################################################################

createConstantSeq :: Int -> Integer -> IO RandSeq
createConstantSeq length value =
    do return (replicate length (Just value))

-- #############################################################################
-- #
-- #############################################################################

createRandomIntegerList :: Integer -> Int -> IO [Integer]
createRandomIntegerList init length =
    do gen <- (createInitializedRNG init)
       return (take length (randoms gen))

createInitializedRNG :: Monad m => Integer -> m StdGen
createInitializedRNG value =
    do return (mkStdGen (fromInteger value))

-- #############################################################################
-- #
-- #############################################################################

