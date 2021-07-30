{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GI.Gio.ListModel.CustomStore where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.GI.Base (GValue, newObject, withManagedPtr)
import Data.GI.Base.BasicTypes
import Data.GI.Base.Overloading (HasParentTypes, ParentTypes)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Word (Word16)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import GI.GObject (Object)
import GI.Gio.Interfaces.ListModel (IsListModel, ListModel)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

newtype CustomStore private a = CustomStore (ManagedPtr (CustomStore private a))

instance HasParentTypes (CustomStore private row)

type instance ParentTypes (CustomStore private row) = '[ListModel]

instance TypedObject (CustomStore private a) where
  glibType = glibType @ListModel

instance GObject (CustomStore private a)

data CustomStoreImpl (model :: Type -> Type) a = CustomStoreImpl
  { getLength :: IO Word16,
    getNthItem :: Word16 -> IO (Maybe a)
  }

customStoreNew :: MonadIO m => private -> CustomStoreImpl model a -> (CustomStore private a -> model a) -> m (model a)
customStoreNew private impl con = liftIO $ do
  privPtr <- newStablePtr private
  implPtr <- newStablePtr impl
  storePtr <- giGioHsListStoreNew implPtr privPtr
  con <$> newObject CustomStore storePtr

foreign import ccall "GiGioHsListStore.h gi_gio_hs_list_store_new"
  giGioHsListStoreNew ::
    StablePtr (CustomStoreImpl model a) ->
    StablePtr private ->
    IO (Ptr (CustomStore private a))

customStoreGetPrivate :: MonadIO m => CustomStore private a -> m private
customStoreGetPrivate store =
  liftIO $ withManagedPtr store giGioHsListStoreGetPriv >>= deRefStablePtr

foreign import ccall "giGioHsListStore.hs gi_gio_hs_list_store_get_priv"
  giGioHsListStoreGetPriv :: Ptr (CustomStore private a) -> IO (StablePtr private)

listModelGetNItems_static :: StablePtr (CustomStoreImpl model a) -> IO CUInt
listModelGetNItems_static storePtr = do
  impl <- deRefStablePtr storePtr
  fmap fromIntegral . getLength $ impl

foreign export ccall "gi_gio_hs_list_store_get_n_items_impl"
  listModelGetNItems_static :: StablePtr (CustomStoreImpl model a) -> IO CUInt

listModelGetItem_static :: StablePtr (CustomStoreImpl model a) -> CUInt -> IO (Ptr ())
listModelGetItem_static storePtr cpos = do
  impl <- deRefStablePtr storePtr
  let pos = fromIntegral cpos
  maybeItem <- getNthItem impl pos
  case maybeItem of
    Just item -> giGioHsListItemNew =<< newStablePtr item
    Nothing -> pure nullPtr

foreign export ccall "gi_gio_hs_list_store_get_item_impl"
  listModelGetItem_static :: StablePtr (CustomStoreImpl model a) -> CUInt -> IO (Ptr ())

foreign import ccall "GiGioHsListItem.h gi_gio_hs_list_item_new"
  giGioHsListItemNew :: StablePtr a -> IO (Ptr ())

listModelGetItemType_static :: StablePtr (CustomStoreImpl model a) -> IO CGType
listModelGetItemType_static _ = listItemGetType

foreign export ccall "gi_gio_hs_list_store_get_item_type_impl"
  listModelGetItemType_static :: StablePtr (CustomStoreImpl model a) -> IO CGType

foreign import ccall "GiGioHsListItem.h gi_gio_hs_list_item_get_type"
  listItemGetType :: IO CGType

newtype CustomStoreItem a = CustomStoreItem (ManagedPtr (CustomStoreItem a))

instance HasParentTypes (CustomStoreItem a)

type instance ParentTypes (CustomStoreItem a) = '[Object]

instance TypedObject (CustomStoreItem a) where
  glibType = GType <$> listItemGetType

instance GObject (CustomStoreItem a)

deRefCustomStoreItem :: MonadIO m => CustomStoreItem a -> m a
deRefCustomStoreItem item =
  liftIO $ withManagedPtr item giGioHsListItemGetItem >>= deRefStablePtr

foreign import ccall "GiGioHsListItem.h gi_gio_hs_list_item_get_item"
  giGioHsListItemGetItem :: Ptr (CustomStoreItem a) -> IO (StablePtr a)
