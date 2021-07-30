#ifndef __GIO_GIO_HS_LIST_STORE_H__
#define __GIO_GIO_HS_LIST_STORE_H__

#include <gio/gio.h>

#include "Data/GI/Gio/ListModel/CustomStore_stub.h"

G_BEGIN_DECLS

#define GIO_GIO_HS_TYPE_LIST_STORE                 (gi_gio_hs_list_store_get_type ())
#define GIO_GIO_HS_LIST_STORE(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), GIO_GIO_HS_TYPE_LIST_STORE, GiGioHsListStore))
#define GIO_GIO_HS_LIST_STORE_CLASS(klass)         (G_TYPE_CHECK_CLASS_CAST ((klass), GIO_GIO_HS_TYPE_LIST_STORE, GiGioHsListStoreClass))
#define GIO_GIO_HS_IS_LIST_STORE(obj)              (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GIO_GIO_HS_TYPE_LIST_STORE))
#define GIO_GIO_HS_IS_LIST_STORE_CLASS(klass)      (G_TYPE_CHECK_CLASS_TYPE ((klass), GIO_GIO_HS_TYPE_LIST_STORE))
#define GIO_GIO_HS_LIST_STORE_GET_CLASS(obj)       (G_TYPE_INSTANCE_GET_CLASS ((obj), GIO_GIO_HS_TYPE_LIST_STORE, GiGioHsListStoreClass))

typedef struct _GiGioHsListStore       GiGioHsListStore;
typedef struct _GiGioHsListStoreClass  GiGioHsListStoreClass;

struct _GiGioHsListStore
{
  GObject parent;

  /*< private >*/
  HsStablePtr     impl;        /* a StablePtr CustomStore */
  HsStablePtr     priv;        /* a StablePtr to private data */

  gint            stamp;       /* Random integer to check whether an iter belongs to our model */
};

struct _GiGioHsListStoreClass
{
  GObjectClass parent_class;
};

GType             gi_gio_hs_list_store_get_type (void) G_GNUC_CONST;
GiGioHsListStore *gi_gio_hs_list_store_new (HsStablePtr, HsStablePtr);
HsStablePtr       gi_gio_hs_list_store_get_impl  (GiGioHsListStore *);
HsStablePtr       gi_gio_hs_list_store_get_priv  (GiGioHsListStore *);
gint              gi_gio_hs_list_store_get_stamp (GiGioHsListStore *);
void              gi_gio_hs_list_store_increment_stamp (GiGioHsListStore *);

G_END_DECLS

#endif /* __GIO_GIO_HS_LIST_STORE_H__ */
