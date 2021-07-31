{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GI.Gio.ListModel.SeqStore where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.GI.Base (GValue, newObject)
import Data.GI.Base.BasicTypes
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.GI.Gio.ListModel.CustomStore (CustomStore (..), CustomStoreImpl (..), customStoreGetPrivate, customStoreNew)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word16)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import GI.Gio.Interfaces.ListModel (IsListModel, ListModel, listModelItemsChanged)

newtype SeqStore a = SeqStore (ManagedPtr (CustomStore (IORef (Seq a)) a))

instance TypedObject (SeqStore a) where
  glibType = glibType @ListModel

instance GObject (SeqStore a)

instance HasParentTypes (SeqStore a)

type instance ParentTypes (SeqStore a) = '[ListModel]

seqStoreNew :: [a] -> IO (SeqStore a)
seqStoreNew list = do
  listRef <- newIORef $ Seq.fromList list
  let getLength = fromIntegral . Seq.length <$> readIORef listRef
      getNthItem n = Seq.lookup (fromIntegral n) <$> readIORef listRef
      con (CustomStore ptr) = SeqStore ptr
  customStoreNew listRef CustomStoreImpl {..} con

replaceList :: MonadIO m => SeqStore a -> [a] -> m ()
replaceList store@(SeqStore customStorePtr) newList = liftIO $ do
  priv <- customStoreGetPrivate (CustomStore customStorePtr)
  oldSeq <- readIORef priv
  let newSeq = Seq.fromList newList
  writeIORef priv newSeq
  listModelItemsChanged store 0 (fromIntegral $ Seq.length oldSeq) (fromIntegral $ Seq.length newSeq)

lookup :: MonadIO m => SeqStore a -> Int -> m (Maybe a)
lookup store n = Seq.lookup n <$> getSeq store

getSeq :: MonadIO m => SeqStore a -> m (Seq a)
getSeq store@(SeqStore customStorePtr) =
  liftIO $ readIORef =<< customStoreGetPrivate (CustomStore customStorePtr)
