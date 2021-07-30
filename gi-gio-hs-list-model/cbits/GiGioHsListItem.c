#include "GiGioHsListItem.h"

#define DEBUG

#ifdef DEBUG
#define WHEN_DEBUG(a) a
#else
#define WHEN_DEBUG(a)
#endif

static void gi_gio_hs_list_item_class_init (GiGioHsListItemClass *class);
static void gi_gio_hs_list_item_finalize   (GObject *object);

static GObjectClass *parent_class = NULL;

/**
 *
 *  gi_gio_hs_list_item_get_type: here we register our new type and its
 *                                interfaces with the type system.
 *
 **/
GType
gi_gio_hs_list_item_get_type (void)
{
  static GType gi_gio_hs_list_item_type = 0;

  if (!gi_gio_hs_list_item_type)
  {
    static const GTypeInfo gi_gio_hs_list_item_info =
    {
      sizeof (GiGioHsListItemClass),
      NULL,              /* base_init */
      NULL,              /* base_finalize */
      (GClassInitFunc) gi_gio_hs_list_item_class_init,
      NULL,              /* class finalize */
      NULL,              /* class_data */
      sizeof (GiGioHsListItem),
      0,                 /* n_preallocs */
      NULL               /* instance_init */
    };

    gi_gio_hs_list_item_type = g_type_register_static (G_TYPE_OBJECT, "GiGioHsListItem",
                                                       &gi_gio_hs_list_item_info,
                                                       (GTypeFlags) 0);
  }

  return gi_gio_hs_list_item_type;
}

/**
 *
 *  gi_gio_hs_list_item_class_init: more boilerplate GObject/GType stuff. Init
 *                                  callback for the type system, called once
 *                                  when our new class is created.
 *
 **/
static void
gi_gio_hs_list_item_class_init (GiGioHsListItemClass *class)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_item_class_init\t\t(%p)\n", class));
  GObjectClass *object_class;

  parent_class = g_type_class_peek_parent (class);
  object_class = (GObjectClass*) class;

  object_class->finalize = gi_gio_hs_list_item_finalize;
}

/**
 *
 *  gi_gio_hs_list_item_finalize: this is called just before a custom list is
 *                                destroyed. Free dynamically allocated memory
 *                                here.
 *
 **/
static void
gi_gio_hs_list_item_finalize (GObject *object)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_item_finalize\t(%p)\n", object));
  GiGioHsListItem *item = (GiGioHsListItem *) object;
  g_return_if_fail(GIO_GIO_HS_IS_LIST_ITEM (object));

  /* free all memory used by the item */
  hs_free_stable_ptr(item->item);

  /* must chain up - finalize parent */
  (* parent_class->finalize) (object);
}

/**
 *
 *  gi_gio_hs_list_item_new: Create a new custom list model which delegates to
 *                           a Haskell implementation.
 *
 **/
GiGioHsListItem *
gi_gio_hs_list_item_new (HsStablePtr item)
{
  WHEN_DEBUG(g_debug("calling gi_gio_hs_list_item_new\t\t(%p)\n", item));
  GiGioHsListItem *newitem = (GiGioHsListItem*) g_object_new (GIO_GIO_HS_TYPE_LIST_ITEM, NULL);

  newitem->item = item;

  WHEN_DEBUG(g_debug("return  gi_gio_hs_list_item_new\t\t=%p\n", newitem));
  return newitem;
}

HsStablePtr gi_gio_hs_list_item_get_item (GiGioHsListItem *item)
{
  g_return_val_if_fail(GIO_GIO_HS_IS_LIST_ITEM(item), NULL);
  return item->item;
}
