module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Trans

import Data.Function
import Data.IORef

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Either.Unwrap

assertThrowsError :: a -> Assertion
assertThrowsError expr =
    try ( evaluate expr ) >>= handler
  where
    handler :: Either SomeException a -> Assertion
    handler (Left _) = return ()
    handler (Right _) = assertFailure $ "Error was not thrown!"

test_isLeft_left = assertEqual "Is the correct value returned?" True (isLeft (Left ()))
test_isLeft_right = assertEqual "Is the correct value returned?" False (isLeft (Right ()))

test_isRight_left = assertEqual "Is the correct value returned?" False (isRight (Left ()))
test_isRight_right = assertEqual "Is the correct value returned?" True (isRight (Right ()))

test_fromLeft_left = assertEqual "Is the correct value returned?" () (fromLeft (Left ()))
test_fromLeft_right = assertThrowsError . fromLeft $ (Right ())

test_fromRight_left = assertEqual "Is the correct value returned?" () (fromRight (Right ()))
test_fromRight_right = assertThrowsError . fromRight $ (Left ())

test_eitherM_left = eitherM (Left ()) return (\_ -> assertFailure "eitherM chose the wrong monad!")
test_eitherM_right = eitherM (Right ()) (\_ -> assertFailure "eitherM chose the wrong monad!") return

test_whenLeft_left = do
    ref <- liftIO $ newIORef False
    whenLeft (Left ref) $ (flip modifyIORef) not
    readIORef ref >>= (flip unless $ assertFailure "The wrong choice was made about whether to execute the monad!")
test_whenLeft_right = do
    ref <- liftIO $ newIORef True
    whenLeft (Right ref) $ (flip modifyIORef) not
    readIORef ref >>= (flip unless $ assertFailure "The wrong choice was made about whether to execute the monad!")

test_whenRight_left = do
    ref <- liftIO $ newIORef True
    whenRight (Left ref) $ (flip modifyIORef) not
    readIORef ref >>= (flip unless $ assertFailure "The wrong choice was made about whether to execute the monad!")
test_whenRight_right = do
    ref <- liftIO $ newIORef False
    whenRight (Right ref) $ (flip modifyIORef) not
    readIORef ref >>= (flip unless $ assertFailure "The wrong choice was made about whether to execute the monad!")

tests =
    [    testGroup "isX"
         [    testGroup "isLeft"
              [    testCase "Left" test_isLeft_left
              ,    testCase "Right" test_isLeft_right
              ]
         ,    testGroup "isRight"
              [    testCase "Left" test_isRight_left
              ,    testCase "Right" test_isRight_right
              ]
         ]
    ,    testGroup "fromX"
         [    testGroup "fromLeft"
              [    testCase "Left" test_fromLeft_left
              ,    testCase "Right" test_fromLeft_right
              ]
         ,    testGroup "fromRight"
              [    testCase "Left" test_fromRight_left
              ,    testCase "Right" test_fromRight_right
              ]
         ]
    ,    testGroup "eitherM"
         [    testCase "Left" test_eitherM_left
         ,    testCase "Right" test_eitherM_right
         ]
    ,    testGroup "whenX"
         [    testGroup "whenLeft"
              [    testCase "Left" test_whenLeft_left
              ,    testCase "Right" test_whenLeft_right
              ]
         ,    testGroup "whenRight"
              [    testCase "Left" test_whenRight_left
              ,    testCase "Right" test_whenRight_right
              ]
         ]
    ]

main = defaultMain tests
