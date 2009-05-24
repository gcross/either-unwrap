-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either.Unwrap
-- Copyright   :  (c) Gregory Crosswhite
-- License     :  BSD-style
-- 
-- Maintainer  :  gcross@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for probing and unwrapping values inside of Either.
--
-----------------------------------------------------------------------------

module Data.Either.Unwrap
    (    isLeft
    ,    isRight
    ,    fromLeft
    ,    fromRight
    ) where

-- ---------------------------------------------------------------------------
-- Functions over Either

-- |The 'isLeft' function returns 'True' iff its argument is of the form @Left _@.
isLeft           :: Either a b -> Bool
isLeft (Left _)  = True
isLeft _         = False

-- |The 'isRight' function returns 'True' iff its argument is of the form @Right _@.
isRight            :: Either a b -> Bool
isRight (Right _)  = True
isRight _          = False

-- | The 'fromLeft' function extracts the element out of a 'Left' and
-- throws an error if its argument take the form  @Right _@.
fromLeft           :: Either a b -> a
fromLeft (Right _) = error "Either.Unwrap.fromLeft: Argument takes form 'Right _'" -- yuck
fromLeft (Left x)  = x

-- | The 'fromRight' function extracts the element out of a 'Right' and
-- throws an error if its argument take the form  @Left _@.
fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x
