{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LB
import Data.Functor.Identity

type role OffsetPtr representational
data OffsetPtr a = OffsetPtr { getOffset :: Int }

data Tree a f
  = Leaf a
  | Branch (f (Tree a f)) (f (Tree a f))

class R2F r2f where
  r2map :: Functor g => (forall a. f a -> g a) -> r2f f -> r2f g

instance R2F (Tree a) where
  r2map nat = \case
    Leaf a -> Leaf a
    Branch lf rf -> Branch (r2map nat <$> nat lf) (r2map nat <$> nat rf)

f1 :: Functor f2 => (f1 (Tree a f1) -> f2 (Tree a f1)) -> f1 (Tree a f1) -> f2 (Tree a f2)
f1 f o = flip fmap (f o) $ \case
  Leaf a -> Leaf a
  Branch lo ro -> Branch (f1 f lo) (f1 f ro)

class Inlineable f2 where
  to :: forall f. f2 f -> (Put, [f (f2 f)])
  from :: forall f. [f (f2 f)] -> Get (f2 f)

instance Binary a => Inlineable (Tree a) where
  to = \case
    Leaf a -> (putWord8 0 <> put a, [])
    Branch lf lr -> (putWord8 1, [lf, lr])
  from cs = getWord8 >>= \case
    0 -> do
      when (not $ null cs) $ fail "Didn't expect children"
      Leaf <$> get
    1 -> case cs of
      [lf, lr] -> return $ Branch lf lr
      _ -> fail "Wrong number of children"
    _ -> fail "Unknown constructor"

writeInlineable :: Inlineable f => Int -> f Identity -> Put
writeInlineable offset t = envelope <> mconcat cs
  where (p, cs') = to t
        cs = go (fromIntegral $ offset+curLength) (runIdentity <$> cs')
          where go _ [] = []
                go o (ch:rest) = let cb = writeInlineable (fromIntegral o) ch in cb: go (LB.length (runPut cb)+o) rest
        cls = init $ scanl (\sum child -> sum + (fromIntegral $ LB.length child)) 0 (runPut <$> cs)
        body = runPut p
        envelope = do
          putWord64be curLength
          putWord64be $ fromIntegral $ length cs'
          forM_ cls $ \cl -> putWord64be $ fromIntegral $ offset + cl + curLength
          p
        curLength :: Integral b => b
        curLength = fromIntegral $ LB.length body + 8 * (fromIntegral $ length cs') + 8 + 8

readPtr :: Inlineable f => OffsetPtr (f OffsetPtr) -> Indexing (f OffsetPtr)
readPtr (OffsetPtr offset) = runGet $ do
  skip offset
  envLength <- getWord64be
  numCs <- getWord64be
  cs <- replicateM (fromIntegral numCs) getWord64be
  from (OffsetPtr . (offset+) . fromIntegral <$> cs)


type Indexing = (->) (LB.ByteString)

readInlined :: Binary a => Indexing (Tree a Indexing)
readInlined = f1 readPtr $  OffsetPtr 0

--tree =

someFunc :: IO ()
someFunc = putStrLn "someFunc"
