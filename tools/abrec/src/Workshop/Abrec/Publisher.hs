{- | Publishing in the background on one worker.

A change requested while a render runs marks the site dirty, so the worker
renders once more afterwards; any number of requests during one render cause
exactly one more.
-}
module Workshop.Abrec.Publisher (
    Publisher,
    PublishState (..),
    startPublisher,
    requestPublish,
    publishState,
    awaitIdle,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TVar, atomically, check, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forever, void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)

data PublishState
    = NeverPublished
    | Publishing
    | Published UTCTime
    | PublishFailed UTCTime Text
    deriving (Eq, Show)

data Publisher = Publisher
    { publisherState :: TVar PublishState
    , publisherDirty :: TVar Bool
    , publisherBusy :: TVar Bool
    }

-- | Start the worker; it renders whenever a publish is requested.
startPublisher :: IO (Either Text ()) -> IO Publisher
startPublisher render = do
    publisher <- Publisher <$> newTVarIO NeverPublished <*> newTVarIO False <*> newTVarIO False
    void . forkIO . forever $ do
        atomically $ do
            readTVar (publisherDirty publisher) >>= check
            writeTVar (publisherDirty publisher) False
            writeTVar (publisherBusy publisher) True
            writeTVar (publisherState publisher) Publishing
        result <- try render
        now <- getCurrentTime
        atomically $ do
            writeTVar (publisherState publisher) $ case result of
                Right (Right ()) -> Published now
                Right (Left err) -> PublishFailed now err
                Left (err :: SomeException) -> PublishFailed now (T.pack (displayException err))
            writeTVar (publisherBusy publisher) False
    pure publisher

requestPublish :: Publisher -> IO ()
requestPublish publisher = atomically (writeTVar (publisherDirty publisher) True)

publishState :: Publisher -> IO PublishState
publishState = readTVarIO . publisherState

-- | Wait until no render is running or requested.
awaitIdle :: Publisher -> IO ()
awaitIdle publisher = atomically (idle >>= check)
  where
    idle :: STM Bool
    idle = do
        dirty <- readTVar (publisherDirty publisher)
        busy <- readTVar (publisherBusy publisher)
        pure (not dirty && not busy)
