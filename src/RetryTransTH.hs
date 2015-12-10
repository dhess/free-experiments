{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.Free.TH
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.Foldable as F
import Text.Read (readMaybe)

-- | A data type representing basic commands for a retriable eDSL.
data RetryF next where
  Output    :: String -> next -> RetryF next
  Input     :: Read a => (a -> next) -> RetryF next
  WithRetry :: RetryT m a -> (a -> next) -> RetryF next
  Retry     :: RetryF next

-- | Unfortunately this Functor instance cannot yet be derived
-- automatically by GHC.
instance Functor RetryF where
  fmap f (Output s x) = Output s (f x)
  fmap f (Input g) = Input (f . g)
  fmap f (WithRetry block g) = WithRetry block (f . g)
  fmap _ Retry = Retry

-- | The monad for a retriable eDSL.
type RetryT = FreeT RetryF

-- | Simple output command.
makeFreeCon 'Output

-- | Get anything readable from input.
makeFreeCon 'Input

-- | Force retry command (retries innermost retriable block).
makeFreeCon 'Retry

makeFreeCon_ 'WithRetry
-- | Run a retryable block.
withRetry :: MonadFree RetryF m =>
             RetryT m a  -- ^ Computation to retry.
          -> m a      -- ^ Computation that retries until succeeds.

-- | We can run a retriable program in any MonadIO.
runRetryT :: MonadIO m => RetryT m a -> m a
runRetryT = iterT run
  where
    run :: MonadIO m => RetryF (m a) -> m a

    run (Output s next) = do
      liftIO $ putStrLn s
      next

    run (Input next) = do
      s <- liftIO getLine
      case readMaybe s of
        Just x  -> next x
        Nothing -> fail "invalid input"

    run (WithRetry block next) = do
      -- Here we use
      -- runRetryT :: MonadIO m => RetryT m a -> MaybeT (m a)
      -- to control failure with MaybeT.
      -- We repeatedly run retriable block until we get it to work.
      --Just x <- runMaybeT . F.msum $ repeat (runRetryT block)
      x <- runRetryT block
      next x

    run Retry = fail "forced retry"

-- | Sample program.
-- test :: RetryT IO ()
-- test = do
--   n <- withRetry $ do
--     output "Enter any positive number: "
--     n <- input
--     when (n <= 0) $ do
--       output "The number should be positive."
--       retry
--     return n
--   output $ "You've just entered " ++ show (n :: Int)

-- main :: IO ()
-- main = runRetryT test

main :: IO ()
main = return ()

