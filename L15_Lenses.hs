{- HLINT ignore "Avoid lambda" -}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module L15_Lenses where

import Control.Category (Category (..))
import Prelude hiding ((.), id)
import Data.Functor.Identity (Identity (..))

-- how to modify datatypes?

data Texture = MkTexture
    { txPath :: FilePath
    , txSize :: (Int, Int)
    -- ,...
    }

data Item = MkItem
    { itTexture :: Texture
    , itStats :: (Float, Int, String)
    -- ,..
    }

data Player = MkPlayer
    { prItems :: [Item]
    , prName :: String
    -- ,..
    }

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ x : drop (i + 1) xs

-- player.items[i].texture.path = fp
setPathTextureItemPlayer :: Int -> FilePath -> Player -> Player
setPathTextureItemPlayer i fp player =
    let ithItem = prItems player !! i
     in player {
          prItems = replace i (ithItem {
                itTexture = (itTexture ithItem) {
                        txPath = fp
                    }
              }) (prItems player)
        }

data SimpleLens a b = MkSimpleLens
    { slGet :: a -> b
    , slSet :: a -> b -> a
    }

-- (a -> b) -> (b -> c) -> (a -> c)
stackLenses :: SimpleLens a b -> SimpleLens b c -> SimpleLens a c
stackLenses (MkSimpleLens gAB sAB) (MkSimpleLens gBC sBC) = MkSimpleLens
    { slGet = gBC . gAB
    , slSet = \a c ->
        let b = gAB a
            b' = sBC b c
         in sAB a b'
    }

instance Category SimpleLens where
    id = MkSimpleLens { slGet = id, slSet = \a a' -> a' }
    l1 . l2 = stackLenses l2 l1

playerItems :: SimpleLens Player [Item]
playerItems = MkSimpleLens prItems (\pr items -> pr { prItems = items })

ix :: Int -> SimpleLens [a] a
ix i = MkSimpleLens (!! i) (\xs x -> replace i x xs)

itemTexture :: SimpleLens Item Texture
itemTexture = MkSimpleLens itTexture (\it tx -> it { itTexture = tx })

texturePath :: SimpleLens Texture FilePath
texturePath = MkSimpleLens txPath (\tx pt -> tx { txPath = pt })

setPathTextureItemPlayer' :: Int -> FilePath -> Player -> Player
setPathTextureItemPlayer' i fp pr =
    slSet (texturePath . itemTexture . ix i . playerItems) pr fp

-- ^ data-lens

-- Van Laarhoven

-- get :: a -> b
-- set :: a -> b -> a
-- modify :: (b -> b) -> a -> a

modifyFromSL :: SimpleLens a b -> (b -> b) -> a -> a
modifyFromSL (MkSimpleLens g s) f a =
    let b = g a
        b' = f b
     in s a b'

type Modify0 a b = (b -> b) -> a -> a

type Set a b = a -> b -> a

setFromModify0 :: Modify0 a b -> Set a b
setFromModify0 m a b = m (const b) a

type Get a b = a -> b

getFromModify0 :: Modify0 a b -> Get a b
getFromModify0 = error "impossible" -- ?
-- doable in C++: catch b via exception
-- or: modify local closure

type ModifyF f a b = (b -> f b) -> a -> f a

setFromModify1 :: ModifyF Identity a b -> Set a b
setFromModify1 m a b = runIdentity $ m (\_ -> Identity b) a

newtype Const b x = MkConst { runConst :: b } deriving Functor

getFromModify1 :: ModifyF (Const b) a b -> Get a b
getFromModify1 m a = runConst (m MkConst a)

type Modify2 a b = forall f. (b -> f b) -> a -> f a

modifyPlayerItems :: Modify2 Player [Item]
modifyPlayerItems f p = error "Impossible"

type SimpleVL a b = forall f. Functor f => (b -> f b) -> (a -> f a)

playerItemsVL :: SimpleVL Player [Item]
playerItemsVL (update :: [Item] -> f [Item]) (player :: Player) =
    (\prItems' -> player { prItems = prItems' }) <$> update (prItems player)

ixVL :: Int -> SimpleVL [a] a
ixVL i up xs = (\x' -> replace i x' xs) <$> up (xs !! i)

vlFromSL :: SimpleLens a b -> SimpleVL a b
vlFromSL (MkSimpleLens g s) up a = s a <$> up (g a)

playerItemVL :: Int -> SimpleVL Player Item
playerItemVL i = playerItemsVL . ixVL i

-- lens

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- hierarchy of lens-like objects!
-- it's called optics
