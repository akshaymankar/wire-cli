#include "GiGioHsListStore.h"

#define DEBUG

#ifdef DEBUG
#define WHEN_DEBUG(a) a
#else
#define WHEN_DEBUG(a)
#endif

static void     gi_gio_hs_list_store_init            (GiGioHsListStore      *store);
static void     gi_gio_hs_list_store_class_init      (GiGioHsListStoreClass *klass);
static void     gi_gio_hs_list_store_list_model_init (GListModelInterface *iface);
static void     gi_gio_hs_list_store_finalize        (GObject             *object);
static GType    gi_gio_hs_list_store_get_item_type   (GListModel          *list);
static guint    gi_gio_hs_list_store_get_n_items     (GListModel          *list);
static gpointer gi_gio_hs_list_store_get_item        (GListModel          *list,
                                                      guint               position);
static GObjectClass *parent_class = NULL;


/**
 *
 *  gi_gio_hs_list_store_get_type: here we register our new type and its
 *                                 interfaces with the type system.
 *
 **/
GType
gi_gio_hs_list_store_get_type (void)
{
  static GType gi_gio_hs_list_store_type = 0;

  if (!gi_gio_hs_list_store_type)
  {
    static const GTypeInfo gi_gio_hs_list_store_info =
    {
      sizeof (GiGioHsListStoreClass),
      NULL,              /* base_init */
      NULL,              /* base_finalize */
      (GClassInitFunc) gi_gio_hs_list_store_class_init,
      NULL,              /* class finalize */
      NULL,              /* class_data */
      sizeof (GiGioHsListStore),
      0,                 /* n_preallocs */
      (GInstanceInitFunc) gi_gio_hs_list_store_init
    };

    static const GInterfaceInfo list_model_info =
    {
      (GInterfaceInitFunc) gi_gio_hs_list_store_list_model_init,
      NULL,
      NULL
    };

    gi_gio_hs_list_store_type = g_type_register_static (G_TYPE_OBJECT, "GiGioHsListStore",
                                                        &gi_gio_hs_list_store_info,
                                                        (GTypeFlags) 0);

    g_type_add_interface_static (gi_gio_hs_list_store_type, G_TYPE_LIST_MODEL,
                                 &list_model_info);

  }

  return gi_gio_hs_list_store_type;
}


/**
 *
 *  gi_gio_hs_list_store_class_init: more boilerplate GObject/GType stuff. Init
 *                                   callback for the type system, called once
 *                                   when our new class is created.
 *
 **/
static void
gi_gio_hs_list_store_class_init (GiGioHsListStoreClass *class)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_store_class_init\t\t(%p)\n", class));
  GObjectClass *object_class;

  parent_class = g_type_class_peek_parent (class);
  object_class = (GObjectClass*) class;

  object_class->finalize = gi_gio_hs_list_store_finalize;
}

/**
 *
 *  gi_gio_hs_list_store_list_model_init: init callback for the interface
 *                                        registration in
 *                                        gi_gio_hs_list_store_get_type. Here we
 *                                        override the GListModelInterface
 *                                        functions that we implement.
 *
 **/
static void
gi_gio_hs_list_store_list_model_init (GListModelInterface *iface)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_store_list_model_init\t(%p)\n", iface));
  iface -> get_item_type = gi_gio_hs_list_store_get_item_type;
  iface -> get_n_items   = gi_gio_hs_list_store_get_n_items;
  iface -> get_item      = gi_gio_hs_list_store_get_item;
}

/**
 *
 *  gi_gio_hs_list_store_init: this is called everytime a new custom list object
 *                             instance is created (we do that in
 *                             gi_gio_hs_list_store_new). Initialise the list
 *                             structure's fields here.
 *
 **/
static void
gi_gio_hs_list_store_init (GiGioHsListStore *store)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_store_init\t\t(%p)\n", store));

  store->stamp = g_random_int();  /* Random int to check whether an iter belongs to our model */
}


/**
 *
 *  gi_gio_hs_list_store_finalize: this is called just before a custom list is
 *                                 destroyed. Free dynamically allocated memory
 *                                 here.
 *
 **/
static void
gi_gio_hs_list_store_finalize (GObject *object)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_store_finalize\t(%p)\n", object));
  GiGioHsListStore *store = (GiGioHsListStore *) object;
  g_return_if_fail(GIO_GIO_HS_IS_LIST_STORE (object));

  /* free all memory used by the store */
  hs_free_stable_ptr(store->impl);
  hs_free_stable_ptr(store->priv);

  /* must chain up - finalize parent */
  (* parent_class->finalize) (object);
}

/**
 *
 *  gi_gio_hs_list_store_get_n_items: tells the rest of the world how many items
 *                                    are in the store.
 *
 **/
static guint
gi_gio_hs_list_store_get_n_items (GListModel *list)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_store_get_n_items\t(%p)\n", list));
  GiGioHsListStore *store = (GiGioHsListStore *) list;
  g_return_val_if_fail (GIO_GIO_HS_IS_LIST_STORE (list), 0);

  // TODO: Export this from haskell
  gint result = gi_gio_hs_list_store_get_n_items_impl(store->impl);
  WHEN_DEBUG(g_debug("return  gi_gio_hs_list_store_get_n_items\t=%d\n", result));
  return result;
}

/**
 *
 *  gi_gio_hs_list_store_get_item: tells the rest of the world what item exists
 *                                 at a given position in the store.
 *
 **/
static gpointer
gi_gio_hs_list_store_get_item (GListModel          *list,
                               guint               position)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_store_get_item\t(%p, %d)\n", list, position));
  GiGioHsListStore *store = (GiGioHsListStore *) list;
  g_return_val_if_fail (GIO_GIO_HS_IS_LIST_STORE (list), 0);

  // TODO: Export this from haskell
  gpointer result = gi_gio_hs_list_store_get_item_impl(store->impl, position);
  WHEN_DEBUG(g_debug("return  gi_gio_hs_list_store_get_item\t=%p\n", result));
  return result;
}


/**
 *
 *  gi_gio_hs_list_store_get_item_type: tells the rest of the world which type
 *                                      of data an exported model contains
 *
 **/
static GType
gi_gio_hs_list_store_get_item_type (GListModel *list)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_store_get_item_type\t(%p)\n", list));
  GiGioHsListStore *store = (GiGioHsListStore *) list;
  g_return_val_if_fail (GIO_GIO_HS_IS_LIST_STORE (list), G_TYPE_INVALID);

  // TODO: Export this from haskell
  GType result = gi_gio_hs_list_store_get_item_type_impl(store->impl);
  WHEN_DEBUG(g_debug("return  gi_gio_hs_list_store_get_item_type\t=%s\n", g_type_name(result)));
  return result;
}


/**
 *
 *  gi_gio_hs_list_store_new: Create a new custom list model which delegates to
 *                            a Haskell implementation.
 *
 **/
GiGioHsListStore *
gi_gio_hs_list_store_new (HsStablePtr impl, HsStablePtr priv)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_store_new\t\t(%p)\n", impl));
  GiGioHsListStore *newstore = (GiGioHsListStore*) g_object_new (GIO_GIO_HS_TYPE_LIST_STORE, NULL);

  newstore->impl = impl;
  newstore->priv = priv;

  WHEN_DEBUG(g_debug("return  gi_gio_hs_list_store_new\t\t=%p\n", newstore));
  return newstore;
}

HsStablePtr gi_gio_hs_list_store_get_impl (GiGioHsListStore *store)
{
  g_return_val_if_fail(GIO_GIO_HS_IS_LIST_STORE(store), NULL);
  return store->impl;
}

HsStablePtr gi_gio_hs_list_store_get_priv (GiGioHsListStore *store)
{
  g_return_val_if_fail(GIO_GIO_HS_IS_LIST_STORE(store), NULL);
  return store->priv;
}

gint
gi_gio_hs_list_store_get_stamp (GiGioHsListStore *store)
{
  g_return_val_if_fail(GIO_GIO_HS_IS_LIST_STORE(store), 0);
  return store->stamp;
}

void
gi_gio_hs_list_store_increment_stamp (GiGioHsListStore *store)
{
  g_return_if_fail(GIO_GIO_HS_IS_LIST_STORE(store));
  do
    {
      store->stamp++;
    }
  while (store->stamp == 0);
}
