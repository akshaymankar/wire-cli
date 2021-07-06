-- | This module tries to provide a reusable API to enqueue work for background
-- workers and polling for it to finish. This is required because GTK is not
-- thread safe and GTK objects must never be interacted with outside the main
-- thread.
module Wire.GUI.Wait where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UnagiNB
import Control.Monad (unless, void, when)
import Data.Functor (($>))
import Data.Maybe (isNothing)
import qualified GI.GLib as GLib
import Polysemy
import Wire.CLI.Error (WireCLIError)
import qualified Wire.GUI.Worker as Worker

type NoMoreWrites = ()

startWaitLoop :: forall a. (a -> IO Bool) -> UnagiNB.Element NoMoreWrites -> UnagiNB.Stream a -> IO ()
startWaitLoop f noMoreWrites = idleAdd_ . go
  where
    go :: UnagiNB.Stream a -> IO Bool
    go str = do
      mbNext <- UnagiNB.tryReadNext str
      case mbNext of
        UnagiNB.Next x str' -> do
          continue <- f x
          when continue $
            idleAdd_ $ go str'
          pure False
        UnagiNB.Pending -> do
          mbNMW <- UnagiNB.tryRead noMoreWrites
          unless (isNothing mbNMW) $
            putStrLn "Channel not active, stopping wait loop"
          pure $ isNothing mbNMW

    idleAdd_ :: IO Bool -> IO ()
    idleAdd_ = void . GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE

-- TODO: Test
queueActionAndStartWaitLoop ::
  -- | work chan
  Unagi.InChan Worker.Work ->
  -- | This function takes 'UnagiNB.InChan' to produce an action, this action is
  -- enqueued. The input channel should be used to emit events for the wait
  -- loop.
  --
  -- N.B. This function runs in another thread so should never use any Gtk
  -- objects.
  (UnagiNB.InChan a -> Sem Worker.AllEffects b) ->
  -- | This function takes 'UnagiNB.InChan' to produce a handler to be called
  -- after completion of the action. This can also use the same input channel to
  -- emit events for the wait loop.
  --
  -- This function runs in another thread so should never use any Gtk
  -- objects.
  (UnagiNB.InChan a -> Either WireCLIError b -> IO ()) ->
  -- | This function is passed to the wait loop and gets called whenever an
  -- event is written to the input channel from the action or the completion
  -- handler. This should return 'False' to stop wait loop. This function is
  -- executed in the main thread so it can interact with GTK objects.
  (a -> IO Bool) ->
  IO ()
queueActionAndStartWaitLoop workChan action completionHandler poll = do
  (iEventChan, oEventChan) <- UnagiNB.newChan
  -- This shouldn't be required if 'UnagiNB.isActive' returns false when worker
  -- finishes, but it doesn't seem to work like that.
  (iNoMoreWrites, oNoMoreWrites) <- UnagiNB.newChan

  let completionHandlerWithNMW res = do
        completionHandler iEventChan res
        UnagiNB.writeChan iNoMoreWrites ()
  Worker.queueAction workChan (action iEventChan) completionHandlerWithNMW

  [updateStream] <- UnagiNB.streamChan 1 oEventChan
  nmw <- UnagiNB.tryReadChan oNoMoreWrites
  startWaitLoop poll nmw updateStream

queueActionWithWaitLoopSimple ::
  -- | Work channel
  Unagi.InChan Worker.Work ->
  -- | Action to execute. Executed in a background thread should never interact
  -- with GTK objects.
  Sem Worker.AllEffects a ->
  -- | Handler for result of the action, executed in the main thread so it can
  -- interact with GTK objects.
  (Either WireCLIError a -> IO ()) ->
  IO ()
queueActionWithWaitLoopSimple workChan action completionHandler =
  queueActionAndStartWaitLoop workChan (const action) UnagiNB.writeChan (\e -> completionHandler e $> False)
