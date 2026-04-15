{-# LANGUAGE BlockArguments #-}

module L13_ST where

import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.STRef (modifySTRef', newSTRef, readSTRef)

countTo10 :: Int
countTo10 = runST do
  var <- newSTRef 0
  for_ [1..10] \i -> do
    var `modifySTRef'` (+ i)
  readSTRef var

-- var' = unsafePerformIO $ stToIO (newSTRef 0)
-- countTo10' :: Int
-- countTo10' = runST do
--   var' `modifySTRef'` (+ 1)
--   readSTRef var'

-- var' :: STRef t Int
-- var' = runST (newSTRef 0)
-- countTo10' :: Int
-- countTo10' = runST do
--   var' `modifySTRef'` (+ 1)
--   readSTRef var'
