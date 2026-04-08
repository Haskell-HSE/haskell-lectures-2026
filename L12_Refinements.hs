{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}




module L12_Refinements where

import Refined (DivisibleBy, Positive, Refined, refineTH)

-- how tests look
--
-- // setup
-- a = new_...()
-- b = connection().open()..
--
-- // function being tested()
-- ...
--
-- // assertion check
-- assert_eq!(expected == actual)
-- ...

-- Hoare triple
-- {P} Q {R}
--
-- P -- preconditions (boolean function)
-- Q -- tested block of code
-- R -- postconditions (boolean function, too)

-- Types+functions are sorta Hoare triples, too
-- Q :: P -> R

-- Refinement types!
--
-- { t :: T | phi(t) }
--
-- { n :: N | n > 0 }
--
-- { xs :: [a] | length xs > 0 }
--
-- sort :: Ord a => {- xs :: -} [a] -> { ys :: [a] | isSorted ys && ys `isPerm` xs }
--
-- decrement :: { n :: N | n > 0 } -> N
--
-- (x, y) :: { (x, y) :: (N, N) | x > 0 \/ y > 0 }
-- decrement (x + y)
--
-- t :: { x :: T | phi(x) }    phi(t) -> psi(t) -- algorithmically undecidable;
-- --------------------------------------------    check has to be sound, but doesn't
--         t :: { x :: T | psi(x) }                have to be complete.

-- Refinement types in Haskell
-- * Liquid Haskell -- interprets comments as refinements
-- * `refined` library

ex1 :: Refined Positive Int
ex1 = $$(refineTH 1)

-- ex2 :: Refined Positive Int
-- ex2 = $$(refineTH 0)

ex3 :: Refined (DivisibleBy 13) Int
ex3 = $$(refineTH 39)











