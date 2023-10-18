{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Data.Ulid
Description : Another ULID implementation with tight memory representation
Copyright   : (c) 2023 Keito Kajitani
License     : MIT
Maintainer  : Keito Kajitani <ijaketak@gmail.com>

This module exposes the data type `Ulid` and related functions.
The data type `Ulid` consumes only 128-bit.
-}
module Data.Ulid
  ( Ulid
  , nil
  , getUlid
  , randomUlidM
  , genUlid
  , showUlid
  , readUlid
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Binary (Binary (get, put))
import Data.Binary.Get (getWord64be)
import Data.Binary.Put (putWord64be)
import Data.Bits (Bits (..), FiniteBits (..), shiftL)
import Data.Char (toUpper)
import Data.Data (Data)
import Data.Hashable (Hashable(..))
import Data.Ix (Ix)
import Data.List (genericTake)
import Data.Maybe (fromJust)
import Data.Primitive.Types (Prim (..), defaultSetByteArray#, defaultSetOffAddr#)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Word (Word64)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.Base (Int (..))
import GHC.Enum (predError, succError)
import GHC.Exts ((*#), (+#), Int#, State#, ByteArray#, MutableByteArray#, Addr#)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import System.Random (Random(..), RandomGen(..), uniform)
import System.Random.Stateful (StatefulGen(..), Uniform(..))

data Ulid = Ulid {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Data, Eq, Ix, Lift, Ord, Generic)

instance Hashable Ulid where
  hashWithSalt s (Ulid wh wl) = s `hashWithSalt` wh `hashWithSalt` wl

-- | big endian order
instance Binary Ulid where
  put (Ulid wh wl) = putWord64be wh >> putWord64be wl
  get = Ulid <$> getWord64be <*> getWord64be

-- | @random = uniform@
instance Random Ulid where
  random = uniform
  randomR _ = random -- range is ignored

-- | generates 128-bit random entropy, may be irregular as ULID
instance Uniform Ulid where
  uniformM gen = do
    w0 <- uniformM gen
    w1 <- uniformM gen
    pure $ Ulid w0 w1

-- | @nil = minBound@
nil :: Ulid
nil = minBound

-- | Generates 'POSIXTime' and random entropy, then combines it into one 'Ulid'.
--
-- @
-- getUlid gen = do
--   t <- liftIO getPOSIXTime
--   randomUlidM t gen
-- @
getUlid :: (MonadIO m, StatefulGen g m) => g -> m Ulid
getUlid gen = do
  t <- liftIO getPOSIXTime
  randomUlidM t gen

randomUlidM :: StatefulGen g m => POSIXTime -> g -> m Ulid
randomUlidM p gen = do
  eh <- uniformWord16 gen
  el <- uniformWord64 gen
  let t = (round $ p * 1000) `shiftL` 16
  return $ Ulid (t .|. fromIntegral eh) el

genUlid :: RandomGen g => POSIXTime -> g -> (Ulid, g)
genUlid p gen =
  let t = (round $ p * 1000) `shiftL` 16
      (eh, gen1) = genWord16 gen
      (el, gen2) = genWord64 gen1
   in (Ulid (t .|. fromIntegral eh) el, gen2)

instance Show Ulid where
  showsPrec _ = showString . T.unpack . showUlid

instance Read Ulid where
  readsPrec _ = f . readUlid' . T.pack
   where
    f mu = case mu of
      Nothing -> []
      Just (u, t) -> [(u, T.unpack t)]

-- | Uses upper case.
showUlid :: Ulid -> T.Text
showUlid (Ulid hi lo) = s3 <> s2 <> s1
 where
  n1 = lo .&. (pred $ 1 `shiftL` 60)
  n2 = ((hi `shiftL` 4) .|. (lo `shiftR` 60)) .&. (pred $ 1 `shiftL` 60)
  n3 = (hi `shiftR` 56) .&. (pred $ 1 `shiftL` 8)
  s1 = encode 12 n1
  s2 = encode 12 n2
  s3 = encode 2 n3

-- | case-insensitive
readUlid :: T.Text -> Maybe Ulid
readUlid = fmap fst . readUlid'

readUlid' :: T.Text -> Maybe (Ulid, T.Text)
readUlid' t = do
  (b1, t1) <- decode 2 t
  (b2, t2) <- decode 12 t1
  (b3, t3) <- decode 12 t2
  let nh1 = (b1 `shiftL` 56) .&. ((pred $ 1 `shiftL` 8) `shiftL` 56)
      nh2 = (b2 `shiftR` 4) .&. (pred $ 1 `shiftL` 56)
      nl1 = (b2 `shiftL` 60) .&. ((pred $ 1 `shiftL` 4) `shiftL` 60)
      nl2 = b3 .&. (pred $ 1 `shiftL` 60)
  return (Ulid (nh1 .|. nh2) (nl1 .|. nl2), t3)


-- Following code are stolen from ulid-0.3.2.0.

-- | >>> decode 5 "0003V"
-- [(123,"")]
decode
  :: Integral i
  => Int  -- ^ Overall length of input Text
  -> T.Text  -- ^ Base 32 encoded Text
  -> Maybe (i, T.Text)  -- ^ Possible parses
decode width str  | T.length str >= width   = let
                      (crock, remainder) = T.splitAt width str
                    in case decodePlain crock of
                        Nothing -> Nothing
                        Just c  -> Just (c, remainder)
                  | otherwise             = Nothing

-- | >>> encode 5 (-123)
-- "0003V"
--
-- | >>> encode (-5) (-123)
-- ""
encode
  :: Integral i
  => Int  -- ^ Overall length of resulting Text
  -> i  -- ^ Natural number to encode
  -> T.Text  -- ^ 0 padded, Douglas Crockford's base 32 encoded Text
encode width =
  leftpad (clampZero width) . encodePlain . clampZero

-- | Decodes a Crockford base 32 encoded `Text` into an natural number,
-- if possible. Returns `Nothing` if the `Text` is not a valid encoded value.
decodePlain :: Integral i => T.Text -> Maybe i
decodePlain base32text = do
  numbers <- mapM decodeChar $ T.unpack base32text
  pure $ unDigits 32 numbers

-- Encodes an natural number into a Text,
-- using Douglas Crockford's base 32 encoding.
-- Returns `Nothing` if number is negative.
encodePlain :: Integral i => i -> T.Text
encodePlain =
  T.pack . fmap encodeChar . digits 32

-- Source: https://stackoverflow.com/a/29153602
-- The safety for m > length was removed, because that should never happen.
-- If it does, it should crash.
leftpad :: Int -> T.Text -> T.Text
leftpad m xs =
  T.replicate (m - T.length xs) "0" <> xs

-- Converts all negative numbers to 0
clampZero :: Integral i => i -> i
clampZero x =
  if x < 0
  then 0
  else x

-- | Decode a character to its corresponding integer
decodeChar :: Integral i => Char -> Maybe i
decodeChar c = case toUpper c of
    '0' -> Just 0
    'O' -> Just 0
    '1' -> Just 1
    'I' -> Just 1
    'L' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    'A' -> Just 10
    'B' -> Just 11
    'C' -> Just 12
    'D' -> Just 13
    'E' -> Just 14
    'F' -> Just 15
    'G' -> Just 16
    'H' -> Just 17
    'J' -> Just 18
    'K' -> Just 19
    'M' -> Just 20
    'N' -> Just 21
    'P' -> Just 22
    'Q' -> Just 23
    'R' -> Just 24
    'S' -> Just 25
    'T' -> Just 26
    'V' -> Just 27
    'W' -> Just 28
    'X' -> Just 29
    'Y' -> Just 30
    'Z' -> Just 31
    _ -> Nothing

-- Encode an integer to its corresponding character
encodeChar :: Integral i => i -> Char
encodeChar i = case i of
  0  -> '0'
  1  -> '1'
  2  -> '2'
  3  -> '3'
  4  -> '4'
  5  -> '5'
  6  -> '6'
  7  -> '7'
  8  -> '8'
  9  -> '9'
  10 -> 'A'
  11 -> 'B'
  12 -> 'C'
  13 -> 'D'
  14 -> 'E'
  15 -> 'F'
  16 -> 'G'
  17 -> 'H'
  18 -> 'J'
  19 -> 'K'
  20 -> 'M'
  21 -> 'N'
  22 -> 'P'
  23 -> 'Q'
  24 -> 'R'
  25 -> 'S'
  26 -> 'T'
  27 -> 'V'
  28 -> 'W'
  29 -> 'X'
  30 -> 'Y'
  31 -> 'Z'
  _  -> '0'

-- Returns the digits of a positive integer as a Maybe list, in reverse order
-- or Nothing if a zero or negative base is given
-- This is slightly more efficient than in forward order.
mDigitsRev :: Integral n
           => n         -- The base to use.
           -> n         -- The number to convert to digit form.
           -> Maybe [n] -- Nothing or Just the digits of the number
                        -- in list form, in reverse.
mDigitsRev base i = if base < 1
  then Nothing -- We do not support zero or negative bases
  else Just $ dr base i
 where
  dr _ 0 = []
  dr b x = case base of
    1 -> genericTake x $ repeat 1
    _ -> let (rest, lastDigit) = quotRem x b
          in lastDigit : dr b rest

-- Returns the digits of a positive integer as a list, in reverse order.
-- Throws an error if given a zero or negative base.
digitsRev :: Integral n
          => n   -- The base to use.
          -> n   -- The number to convert to digit from.
          -> [n] -- The digits of the number in list from, in reverse.
digitsRev base = fromJust . mDigitsRev base

-- Returns the digits of a positive integer as a list.
-- Throws an error if given a zero or negative base.
digits :: Integral n
       => n   -- The base to use (typically 10).
       -> n   -- The number to convert to digit form.
       -> [n] -- Either Nothing or the digits of the number in list form.
digits base = reverse . digitsRev base

-- Takes a list of digits, and converts them back into a positive integer.
unDigits :: Integral n
         => n   -- The base to use.
         -> [n] -- The digits of the number in list form.
         -> n   -- The original number.
unDigits base = foldl (\ a b -> a * base + b) 0


-- Following code are stolen from wide-word-0.1.5.0

instance Bounded Ulid where
  minBound = zeroWord128
  maxBound = Ulid maxBound maxBound

instance Enum Ulid where
  succ = succ128
  pred = pred128
  toEnum = toEnum128
  fromEnum = fromEnum128

instance Bits Ulid where
  (.&.) = and128
  (.|.) = or128
  xor = xor128
  complement = complement128
  shiftL = shiftL128
  unsafeShiftL = shiftL128
  shiftR = shiftR128
  unsafeShiftR = shiftR128
  rotateL = rotateL128
  rotateR = rotateR128

  bitSize _ = 128
  bitSizeMaybe _ = Just 128
  isSigned _ = False

  testBit = testBit128
  bit = bit128

  popCount = popCount128

instance FiniteBits Ulid where
  finiteBitSize _ = 128
  countLeadingZeros = countLeadingZeros128
  countTrailingZeros = countTrailingZeros128

instance Storable Ulid where
  sizeOf w = I# (sizeOf128# w)
  alignment w = I# (alignment128# w)
  peek = peek128
  peekElemOff = peekElemOff128
  poke = poke128
  pokeElemOff = pokeElemOff128

instance NFData Ulid where
  rnf = flip seq ()

instance Prim Ulid where
  sizeOf#         = sizeOf128#
  alignment#      = alignment128#
  indexByteArray# = indexByteArray128#
  readByteArray#  = readByteArray128#
  writeByteArray# = writeByteArray128#
  setByteArray#   = setByteArray128#
  indexOffAddr#   = indexOffAddr128#
  readOffAddr#    = readOffAddr128#
  writeOffAddr#   = writeOffAddr128#
  setOffAddr#     = setOffAddr128#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}

-- -----------------------------------------------------------------------------
-- Functions for `Enum` instance.

succ128 :: Ulid -> Ulid
succ128 (Ulid a1 a0)
  | a0 == maxBound = if a1 == maxBound
                     then succError "Ulid"
                     else Ulid (succ a1) 0
  | otherwise = Ulid a1 (succ a0)


pred128 :: Ulid -> Ulid
pred128 (Ulid a1 a0)
  | a0 == 0 = if a1 == 0
              then predError "Ulid"
              else Ulid (pred a1) maxBound
  | otherwise = Ulid a1 (pred a0)


{-# INLINABLE toEnum128 #-}
toEnum128 :: Int -> Ulid
toEnum128 i = Ulid 0 (toEnum i)

{-# INLINABLE fromEnum128 #-}
fromEnum128 :: Ulid -> Int
fromEnum128 (Ulid _ a0) = fromEnum a0

-- -----------------------------------------------------------------------------
-- Functions for `Bits` instance.

{-# INLINABLE and128 #-}
and128 :: Ulid -> Ulid -> Ulid
and128 (Ulid a1 a0) (Ulid b1 b0) = Ulid (a1 .&. b1) (a0 .&. b0)

{-# INLINABLE or128 #-}
or128 :: Ulid -> Ulid -> Ulid
or128 (Ulid a1 a0) (Ulid b1 b0) = Ulid (a1 .|. b1) (a0 .|. b0)

{-# INLINABLE xor128 #-}
xor128 :: Ulid -> Ulid -> Ulid
xor128 (Ulid a1 a0) (Ulid b1 b0) = Ulid (xor a1 b1) (xor a0 b0)

{-# INLINABLE complement128 #-}
complement128 :: Ulid -> Ulid
complement128 (Ulid a1 a0) = Ulid (complement a1) (complement a0)

-- Probably not worth inlining this.
shiftL128 :: Ulid -> Int -> Ulid
shiftL128 w@(Ulid a1 a0) s
  | s == 0 = w
  | s == minBound = zeroWord128
  | s < 0 = shiftR128 w (negate s)
  | s >= 128 = zeroWord128
  | s == 64 = Ulid a0 0
  | s > 64 = Ulid (a0 `shiftL` (s - 64)) 0
  | otherwise =
      Ulid s1 s0
      where
        s0 = a0 `shiftL` s
        s1 = a1 `shiftL` s + a0 `shiftR` (64 - s)

-- Probably not worth inlining this.
shiftR128 :: Ulid -> Int -> Ulid
shiftR128 w@(Ulid a1 a0) s
  | s == 0 = w
  | s == minBound = zeroWord128
  | s < 0 = shiftL128 w (negate s)
  | s >= 128 = zeroWord128
  | s == 64 = Ulid 0 a1
  | s > 64 = Ulid 0 (a1 `shiftR` (s - 64))
  | otherwise =
      Ulid s1 s0
      where
        s1 = a1 `shiftR` s
        s0 = a0 `shiftR` s + a1 `shiftL` (64 - s)

rotateL128 :: Ulid -> Int -> Ulid
rotateL128 w@(Ulid a1 a0) r
  | r == 0 = w
  | r < 0 = rotateL128 w (128 - (abs r `mod` 128))
  | r >= 128 = rotateL128 w (r `mod` 128)
  | r == 64 = Ulid a0 a1
  | r > 64 = rotateL128 (Ulid a0 a1) (r `mod` 64)
  | otherwise =
      Ulid s1 s0
      where
        s0 = a0 `shiftL` r + a1 `shiftR` (64 - r)
        s1 = a1 `shiftL` r + a0 `shiftR` (64 - r)

rotateR128 :: Ulid -> Int -> Ulid
rotateR128 w@(Ulid a1 a0) r
  | r == 0 = w
  | r < 0 = rotateR128 w (128 - (abs r `mod` 128))
  | r >= 128 = rotateR128 w (r `mod` 128)
  | r == 64 = Ulid a0 a1
  | r > 64 = rotateR128 (Ulid a0 a1) (r `mod` 64)
  | otherwise =
      Ulid s1 s0
      where
        s0 = a0 `shiftR` r + a1 `shiftL` (64 - r)
        s1 = a1 `shiftR` r + a0 `shiftL` (64 - r)

testBit128 :: Ulid -> Int -> Bool
testBit128 (Ulid a1 a0) i
  | i < 0 = False
  | i >= 128 = False
  | i >= 64 = testBit a1 (i - 64)
  | otherwise = testBit a0 i

bit128 :: Int -> Ulid
bit128 indx
  | indx < 0 = zeroWord128
  | indx >= 128 = zeroWord128
  | otherwise = shiftL128 oneWord128 indx

popCount128 :: Ulid -> Int
popCount128 (Ulid a1 a0) = popCount a1 + popCount a0

-- -----------------------------------------------------------------------------
-- Functions for `FiniteBits` instance.

countLeadingZeros128 :: Ulid -> Int
countLeadingZeros128 (Ulid a1 a0) =
  case countLeadingZeros a1 of
    64 -> 64 +  countLeadingZeros a0
    res -> res

countTrailingZeros128 :: Ulid -> Int
countTrailingZeros128 (Ulid a1 a0) =
  case countTrailingZeros a0 of
    64 -> 64 + countTrailingZeros a1
    res -> res

-- -----------------------------------------------------------------------------
-- Functions for `Storable` instance.

peek128 :: Ptr Ulid -> IO Ulid
peek128 ptr =
  Ulid <$> peekElemOff (castPtr ptr) index1 <*> peekElemOff (castPtr ptr) index0

peekElemOff128 :: Ptr Ulid -> Int -> IO Ulid
peekElemOff128 ptr idx =
  Ulid <$> peekElemOff (castPtr ptr) (idx2 + index1)
            <*> peekElemOff (castPtr ptr) (idx2 + index0)
  where idx2 = 2 * idx

poke128 :: Ptr Ulid -> Ulid -> IO ()
poke128 ptr (Ulid a1 a0) =
  pokeElemOff (castPtr ptr) index1 a1 >> pokeElemOff (castPtr ptr) index0 a0

pokeElemOff128 :: Ptr Ulid -> Int -> Ulid -> IO ()
pokeElemOff128 ptr idx (Ulid a1 a0) = do
  let idx2 = 2 * idx
  pokeElemOff (castPtr ptr) (idx2 + index0) a0
  pokeElemOff (castPtr ptr) (idx2 + index1) a1

-- -----------------------------------------------------------------------------
-- Functions for `Prim` instance.

{-# INLINE sizeOf128# #-}
sizeOf128# :: Ulid -> Int#
sizeOf128# _ = 2# *# sizeOf# (0 :: Word64)

{-# INLINE alignment128# #-}
alignment128# :: Ulid -> Int#
alignment128# _ = 2# *# alignment# (0 :: Word64)

{-# INLINE indexByteArray128# #-}
indexByteArray128# :: ByteArray# -> Int# -> Ulid
indexByteArray128# arr# i# =
  let i2# = 2# *# i#
      x = indexByteArray# arr# (i2# +# unInt index1)
      y = indexByteArray# arr# (i2# +# unInt index0)
  in Ulid x y

{-# INLINE readByteArray128# #-}
readByteArray128# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Ulid #)
readByteArray128# arr# i# =
  \s0 -> case readByteArray# arr# (i2# +# unInt index1) s0 of
    (# s1, x #) -> case readByteArray# arr# (i2# +# unInt index0) s1 of
      (# s2, y #) -> (# s2, Ulid x y #)
  where i2# = 2# *# i#

{-# INLINE writeByteArray128# #-}
writeByteArray128# :: MutableByteArray# s -> Int# -> Ulid -> State# s -> State# s
writeByteArray128# arr# i# (Ulid a b) =
  \s0 -> case writeByteArray# arr# (i2# +# unInt index1) a s0 of
    s1 -> case writeByteArray# arr# (i2# +# unInt index0) b s1 of
      s2 -> s2
  where i2# = 2# *# i#

{-# INLINE setByteArray128# #-}
setByteArray128# :: MutableByteArray# s -> Int# -> Int# -> Ulid -> State# s -> State# s
setByteArray128# = defaultSetByteArray#

{-# INLINE indexOffAddr128# #-}
indexOffAddr128# :: Addr# -> Int# -> Ulid
indexOffAddr128# addr# i# =
  let i2# = 2# *# i#
      x = indexOffAddr# addr# (i2# +# unInt index1)
      y = indexOffAddr# addr# (i2# +# unInt index0)
  in Ulid x y

{-# INLINE readOffAddr128# #-}
readOffAddr128# :: Addr# -> Int# -> State# s -> (# State# s, Ulid #)
readOffAddr128# addr# i# =
  \s0 -> case readOffAddr# addr# (i2# +# unInt index1) s0 of
    (# s1, x #) -> case readOffAddr# addr# (i2# +# unInt index0) s1 of
      (# s2, y #) -> (# s2, Ulid x y #)
  where i2# = 2# *# i#

{-# INLINE writeOffAddr128# #-}
writeOffAddr128# :: Addr# -> Int# -> Ulid -> State# s -> State# s
writeOffAddr128# addr# i# (Ulid a b) =
  \s0 -> case writeOffAddr# addr# (i2# +# unInt index1) a s0 of
    s1 -> case writeOffAddr# addr# (i2# +# unInt index0) b s1 of
      s2 -> s2
  where i2# = 2# *# i#

{-# INLINE setOffAddr128# #-}
setOffAddr128# :: Addr# -> Int# -> Int# -> Ulid -> State# s -> State# s
setOffAddr128# = defaultSetOffAddr#

-- -----------------------------------------------------------------------------
-- Other functions.

unInt :: Int -> Int#
unInt (I# i#) = i#

-- -----------------------------------------------------------------------------
-- Constants.

zeroWord128 :: Ulid
zeroWord128 = Ulid 0 0

oneWord128 :: Ulid
oneWord128 = Ulid 0 1

-- Use these indices to get the peek/poke ordering endian correct.
index0, index1 :: Int
#if WORDS_BIGENDIAN
index0 = 1
index1 = 0
#else
index0 = 0
index1 = 1
#endif
