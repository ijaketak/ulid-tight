module UlidSpec where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import qualified Data.Binary as B
import Data.Bits
import Data.Char (isAlphaNum)
import qualified Data.Hashable as H
import Data.List (nub, sort)
import qualified Data.Primitive.PrimArray as P
import qualified Data.Primitive.Ptr as P
import qualified Data.Text as T
import Foreign (allocaBytes)
import qualified Foreign.Storable as S
import System.Random (initStdGen, randomIO)
import System.Random.Stateful (newAtomicGenM)
import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Ulid

spec :: Spec
spec = do
  sg <- runIO initStdGen
  ag <- runIO $ newAtomicGenM sg
  let bitSizeUlid = finiteBitSize (undefined :: Ulid)
  describe "Ulid" $ do
    it "is lexicographically sortable" $ do
      u1 <- getUlid ag
      threadDelay 1500
      u2 <- getUlid ag
      threadDelay 1500
      u3 <- getUlid ag
      threadDelay 1500
      u4 <- getUlid ag
      sort [u2, u3, u1, u4] `shouldBe` [u1, u2, u3, u4]
    it "generates unique ulids" $ do
      let ops = 256
      us <- replicateM ops $ getUlid ag
      length (nub us) `shouldBe` ops
    it "generates random uniform ulids" $ do
      u1 <- randomIO :: IO Ulid
      u2 <- randomIO :: IO Ulid
      u1 `shouldNotBe` u2
    it "produces different hash for nonequal ulids" $ do
      u1 <- getUlid ag
      u2 <- getUlid ag
      let salt = 11113
      H.hashWithSalt salt u1 `shouldNotBe` H.hashWithSalt salt u2
  describe "showUlid" $ do
    it "length 26 chars" $ do
      u <- getUlid ag
      T.length (showUlid u) `shouldBe` 26
    it "contains no special chars" $ do
      u <- getUlid ag
      filter (not . isAlphaNum) (T.unpack $ showUlid u) `shouldBe` []
  describe "readUlid" $ do
    it "is symmetric to showUlid" $ do
      u <- getUlid ag
      readUlid (showUlid u) `shouldBe` Just u
    it "is case insensitive" $ do
      u <- getUlid ag
      readUlid (T.toLower $ showUlid u) `shouldBe` Just u
  describe "binary instances" $ do
    it "is symmetric" $ do
      u <- getUlid ag
      B.decode (B.encode u) `shouldBe` u
  describe "Bounded instance" $ do
    it "is ordered correctly" $ (minBound :: Ulid) < (maxBound :: Ulid)
  describe "Enum instance" $ do
    it "throw an error by succ of maxBound" $
      (return $! succ (maxBound :: Ulid)) `shouldThrow` anyErrorCall
    it "throw an error by pred of minBound" $
      (return $! pred (minBound :: Ulid)) `shouldThrow` anyErrorCall
  describe "Bits instance" $ do
    it "vanishes all bits by .&. minBound" $ do
      u <- getUlid ag
      u .&. minBound `shouldBe` minBound
    it "keeps all bits by .&. maxBound" $ do
      u <- getUlid ag
      u .&. maxBound `shouldBe` u
    it "keeps all bits by .|. minBound" $ do
      u <- getUlid ag
      u .|. minBound `shouldBe` u
    it "fills all bits by .|. maxBound" $ do
      u <- getUlid ag
      u .|. maxBound `shouldBe` maxBound
    it "vanishes all bits by xor with itself" $ do
      u <- getUlid ag
      u `xor` u `shouldBe` minBound
    it "restores by two xor" $ do
      u1 <- getUlid ag
      u2 <- getUlid ag
      (u1 `xor` u2) `xor` u2 `shouldBe` u1
    it "restores by two complement" $ do
      u <- getUlid ag
      complement (complement u) `shouldBe` u
    prop "rotateL means" $ \t -> do
      let s = t `mod` bitSizeUlid
      u <- getUlid ag
      (u `shiftL` s) .|. (u `shiftR` (bitSizeUlid - s)) `shouldBe` (u `rotateL` t)
    prop "rotateR means" $ \t -> do
      let s = t `mod` bitSizeUlid
      u <- getUlid ag
      (u `shiftR` s) .|. (u `shiftL` (bitSizeUlid - s)) `shouldBe` (u `rotateR` t)
    prop "shift holds for negate" $ \t -> do
      let s = negate t
      u <- getUlid ag
      u `shiftL` s `shouldBe` u `shiftR` t
    prop "rotate holds for negate" $ \t -> do
      let s = negate t
      u <- getUlid ag
      u `rotateL` s `shouldBe` u `rotateR` t
    it "popCount and complement" $ do
      u <- getUlid ag
      popCount u + popCount (complement u) `shouldBe` bitSizeUlid
  describe "Storable instance" $ do
    it "restores original value" $ do
      u <- getUlid ag
      r <- allocaBytes (S.sizeOf nil) $ \ptr -> do
        S.poke ptr u
        S.peek ptr
      r `shouldBe` u
    it "restores original value (ElemOff variant)" $ do
      u1 <- getUlid ag
      u2 <- getUlid ag
      (r1, r2) <- allocaBytes (2 * S.sizeOf nil) $ \ptr -> do
        S.pokeElemOff ptr 0 u1
        S.pokeElemOff ptr 1 u2
        (,) <$> S.peekElemOff ptr 0 <*> S.peekElemOff ptr 1
      (r1, r2) `shouldBe` (u1, u2)
  describe "Prim instance" $ do
    it "restores original value by to/from" $ do
      let ops = 256
      us <- replicateM ops $ getUlid ag
      us `shouldBe` P.primArrayToList (P.primArrayFromList us)
    prop "restores original value by read/write (PrimArray variant)" $ \i -> do
      let ops = 256
          j = i `mod` ops
      us <- replicateM ops $ getUlid ag
      u <- getUlid ag
      bs <- do
        let arr = P.primArrayFromList us
        marr <- P.unsafeThawPrimArray arr
        prev <- P.readPrimArray marr j
        P.writePrimArray marr j u
        cur <- P.readPrimArray marr j
        P.setPrimArray marr j 1 prev
        arr2 <- P.unsafeFreezePrimArray marr
        return [prev == us !! j, cur == u, arr == arr2]
      and bs `shouldBe` True
    it "restores original value by read/write (OffPtr variant)" $ do
      u1 <- getUlid ag
      u2 <- getUlid ag
      (r1, r2) <- allocaBytes (2 * S.sizeOf nil) $ \ptr -> do
        P.writeOffPtr ptr 0 u1
        P.writeOffPtr ptr 1 u2
        (,) <$> P.readOffPtr ptr 0 <*> P.readOffPtr ptr 1
      (r1, r2) `shouldBe` (u1, u2)
