#ifndef __GIO_GIO_HS_LIST_ITEM_H__
#define __GIO_GIO_HS_LIST_ITEM_H__

#include <gio/gio.h>

#include "Data/GI/Gio/ListModel/CustomStore_stub.h"

G_BEGIN_DECLS

#define GIO_GIO_HS_TYPE_LIST_ITEM                 (gi_gio_hs_list_item_get_type ())
#define GIO_GIO_HS_LIST_ITEM(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), GIO_GIO_HS_TYPE_LIST_ITEM, GiGioHsListItem))
#define GIO_GIO_HS_LIST_ITEM_CLASS(klass)         (G_TYPE_CHECK_CLASS_CAST ((klass), GIO_GIO_HS_TYPE_LIST_ITEM, GiGioHsListItemClass))
#define GIO_GIO_HS_IS_LIST_ITEM(obj)              (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GIO_GIO_HS_TYPE_LIST_ITEM))
#define GIO_GIO_HS_IS_LIST_ITEM_CLASS(klass)      (G_TYPE_CHECK_CLASS_TYPE ((klass), GIO_GIO_HS_TYPE_LIST_ITEM))
#define GIO_GIO_HS_LIST_ITEM_GET_CLASS(obj)       (G_TYPE_INSTANCE_GET_CLASS ((obj), GIO_GIO_HS_TYPE_LIST_ITEM, GiGioHsListItemClass))

typedef struct _GiGioHsListItem       GiGioHsListItem;
typedef struct _GiGioHsListItemClass  GiGioHsListItemClass;

struct _GiGioHsListItem
{
  GObject parent;

  /*< private >*/
  HsStablePtr item;        /* a StablePtr for Item */
};

struct _GiGioHsListItemClass
{
  GObjectClass parent_class;
};


GType            gi_gio_hs_list_item_get_type (void) G_GNUC_CONST;
GiGioHsListItem *gi_gio_hs_list_item_new (HsStablePtr);
HsStablePtr      gi_gio_hs_list_item_get_item  (GiGioHsListItem *);

G_END_DECLS

#endif /* __GIO_GIO_HS_LIST_ITEM_H__ */
