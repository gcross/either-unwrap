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
    ,    mapLeft
    ,    mapRight
    ,    onBoth
    ,    onLeft
    ,    onRight
    ,    eitherM
    ,    whenLeft
    ,    whenRight
    ,    unlessLeft
    ,    unlessRight
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

-- | The 'mapLeft' function applies a function to the left value in the either.
-- @mapLeft f@ equivalent to @either (Left . f) Right@.
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

-- | The 'mapRight' function applies a function to the right value in the either.
-- @mapRight f@ equivalent to @either Left (Right . f)@.
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f = either Left (Right . f)

-- | The 'onBoth' function takes two functions and applies the first if iff the value
-- takes the form 'Left _' and the second if the value takes the form 'Right _'.
onBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
onBoth f _ (Left x) = Left (f x)
onBoth _ f (Right x) = Right (f x)

-- | The 'onLeft' function takes a function and applies it to an Either value
-- iff the value takes the form 'Left _'.
onLeft :: (a -> c) -> Either a b -> Either c b
onLeft = (`onBoth` id)

-- | The 'onLeft' function takes a function and applies it to an Either value
-- iff the value takes the form 'Left _'.
onRight :: (b -> c) -> Either a b -> Either a c
onRight = (id `onBoth`)

-- | The 'eitherM' function takes an 'Either' value and two functions which return monads.
-- If the argument takes the form @Left _@ then the element within is passed to the first
-- function, otherwise the element within is passed to the second function. 
eitherM               :: Monad m => Either a b -> (a -> m c) -> (b -> m c) -> m c
eitherM (Left x)  f _ = f x
eitherM (Right x) _ f = f x

-- | The 'whenLeft' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @Left _@, otherwise
-- it does nothing.
whenLeft            :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _ _        = return ()

-- | The 'whenLeft' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @Right _@, otherwise
-- it does nothing.
whenRight             :: Monad m => Either a b -> (b -> m ()) -> m ()
whenRight (Right x) f = f x
whenRight _ _         = return ()

-- | A synonym of 'whenRight'.
unlessLeft :: Monad m => Either a b -> (b -> m ()) -> m ()
unlessLeft = whenRight

-- | A synonym of 'whenLeft'.
unlessRight :: Monad m => Either a b -> (a -> m ()) -> m ()
unlessRight = whenLeft
