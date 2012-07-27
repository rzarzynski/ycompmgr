/*
 * ycompmgr - the composite manager for X11.
 *
 * Copyright © 2012 Radoslaw Adam Zarzynski
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *
 * Compilation:
 *   gcc ycompmgr.c -o ycompmgr -lX11 -lXcomposite -lXfixes
 *                  -lcairo -lXdamage -lXext -lm
 *                  -std=c1x -O2 -fplan9-extensions
 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/extensions/Xcomposite.h>
#include <X11/extensions/Xdamage.h>
#include <X11/extensions/damagewire.h>
#include <X11/extensions/Xfixes.h>
#include <X11/extensions/shapeconst.h>
#include <X11/extensions/Xdbe.h>

#include <cairo/cairo.h>
#include <cairo/cairo-xlib.h>

#include <err.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <stdint.h>
#include <unistd.h>
#include <getopt.h>

#define ARRAY_LENGTH(array)         (sizeof(array) / sizeof(array[0]))
#define IF_DEBUG(x)

#define ATOM_WIN_TYPE               "_NET_WM_WINDOW_TYPE"
#define ATOM_WIN_TYPE_DESKTOP       "_NET_WM_WINDOW_TYPE_DESKTOP"
#define ATOM_WIN_TYPE_DOCK          "_NET_WM_WINDOW_TYPE_DOCK"
#define ATOM_WIN_TYPE_NORMAL        "_NET_WM_WINDOW_TYPE_NORMAL"
#define ATOM_WIN_OPACITY            "_NET_WM_WINDOW_OPACITY"

#define ATOM_WIN_BGRND_XROOTPMAP    "_XROOTPMAP_ID"
#define ATOM_WIN_BGRND_XSETROOT     "_XSETROOT_ID"

#ifndef M_PI
# define M_PI                       3.14159265358979323846
#endif

#define XERR_IGNORE_FAULT(expression)                       \
({                                                          \
    xerr_ignore(XNextRequest(display));                     \
    (expression);                                           \
})

typedef union {
    Window              wid;
    int                 i;
    unsigned int        ui;
    short               s;
    void                *v;
} list_cb_arg_t;

typedef struct _list_elem {
    struct _list_elem   *next;
    struct _list_elem   *prev;
} list_elem_t;

typedef struct {
    list_elem_t         *head;
    list_elem_t         *tail;
    size_t              len;
} list_t;

typedef bool (*list_cb_t)(list_elem_t *, list_cb_arg_t *);

typedef union {
    XRectangle          r;
    struct {
        short           x, y;
        unsigned short  w, h;
    };
} ease_xrect_t;

typedef struct {
    cairo_surface_t     *surf;
    ease_xrect_t        geom;
} surf_geom_t;

typedef struct {
    struct _list_elem;
    ease_xrect_t;

    Window              wid;
    Window              above;
    Damage              damage;
    Screen              *screen;
    Visual              *vis;
    Atom                type;
    int                 state;
    int                 b;

    union {
        surf_geom_t     content_sag;
        cairo_surface_t *content;
    };
 
    union {
        surf_geom_t     shadow_sag;
        cairo_surface_t *shadow;
    };
} win_t;

typedef int (*xerr_handler_t)(Display *, XErrorEvent *);

static struct {
    xerr_handler_t      real_handler;
    unsigned long       seq;
    bool                valid;
} xerr_data = { NULL, 0, false };

static union {
    struct {
        Atom            win_type;
        Atom            win_type_dtop;
        Atom            win_type_dock;
        Atom            win_type_norm;
        Atom            win_opacity;
    };
    Atom                as_array[5];
} atoms = { None };

static struct {
    short               shdw_x_offset;
    short               shdw_y_offset;
    unsigned short      shdw_radius;
    float               shdw_opacity;
} opts = { 10, 10, 20, 0.5 };

static Display          *display;
static win_t            *root;
static list_t           win_list;
static cairo_surface_t  *cs          = NULL;
static cairo_surface_t  *cs_alp      = NULL;
static XserverRegion    dmg_glbl_reg = None;

bool list_cb_cmp_win (list_elem_t *, list_cb_arg_t *);
bool list_cb_print_item (list_elem_t *, list_cb_arg_t *);
static void win_map (Window const);
static void win_unmap (Window const);


static bool list_init (list_t * const l)
{
    l->head = l->tail = NULL;
    l->len  = 0;

    return true;
}

static inline list_elem_t *list_get_tail (list_t const * const l)
{
    return l->tail;
}

static bool list_append (list_t * const l, list_elem_t * const e)
{
    e->prev = e->next = NULL;

    if (l->tail) {
        l->tail->next = e;
        e->prev = l->tail;
        l->tail = e;
    }
    else
        l->head = l->tail = e;

    return true;
}

static bool list_delete (list_t * const l, list_elem_t * const e)
{
    if (!e || !l || !l->head || !l->tail)
        return false;

    if (e->prev)
        e->prev->next = e->next;
    else
        l->head = e->next;

    if (e->next)
        e->next->prev = e->prev;
    else
        l->tail = e->prev;

    return true;
}

static bool list_insert (list_t * const l, list_elem_t * p, list_elem_t * e)
{
    e->prev = p;
    if (p) {
        e->next = p->next;

        if (p->next)
            p->next->prev = e;
        else
            l->tail = e;

        p->next = e;
    }
    else {
        e->next = l->head;
        if (l->head)
            l->head->prev = e;
        l->head = e;
    }

    return true;
}

static list_elem_t * list_find (list_t const * const l, list_cb_t cb, list_cb_arg_t *cb_arg)
{
    for (list_elem_t *e = l->head; e; e = e->next)
        if (cb(e, cb_arg))
            return e;

    return NULL;
}

static bool list_foreach (list_t const * const l, list_cb_t cb, list_cb_arg_t *cb_arg)
{
    bool ret = true;

    for (list_elem_t *e = l->tail; e; e = e->prev)
        if (!cb(e, cb_arg))
            ret = false;

    return ret;
}

static void dmg_glbl_add (XserverRegion r)
{
    if (dmg_glbl_reg) {
        XFixesUnionRegion(display, dmg_glbl_reg, r, dmg_glbl_reg);
        XFixesDestroyRegion(display, r);
    }
    else
        dmg_glbl_reg = r;
}

static int xerr_wrapped_handler (Display *err_dpy, XErrorEvent *err_ev)
{
    if (xerr_data.valid && xerr_data.seq == err_ev->serial) {
        xerr_data.valid = false;
        return 0;
    }

    return xerr_data.real_handler(err_dpy, err_ev);
}

static void xerr_ignore (unsigned long const seq)
{
    xerr_data.valid = true;
    xerr_data.seq   = seq;

    if (!xerr_data.real_handler)
        xerr_data.real_handler = XSetErrorHandler(xerr_wrapped_handler);
}

static inline bool win_want_shadow (win_t const * const w)
{
    return (w->wid != root->wid && w->type != atoms.win_type_dock);
}

static inline win_t * win_find(Window const wid)
{
    return ((win_t *)list_find(&win_list, list_cb_cmp_win,
            (void *)&wid));
}

static inline bool is_visible (win_t const * const w)
{
    return (w->state == IsViewable &&
            (w->x + w->w) >= 0 && (w->y + w->h) >= 0 && w->x < root->w && w->y < root->h);
}

static inline bool win_size_changed (
        win_t           const * const w,
        XConfigureEvent const * const ev)
{
    return (w->w != ev->width || w->h != ev->height
            || w->b != ev->border_width);
}

static inline XRectangle wutls_border_ext (win_t const * const w)
{
    return (XRectangle) {
        w->x,
        w->y,
        w->w + 2 * w->b,
        w->h + 2 * w->b
    };
}

static inline XRectangle wutls_shadow_ext (win_t const * const w)
{
    unsigned int const r_offset = (int)ceil(3 * opts.shdw_radius + 1) & ~1;
    return (XRectangle) {
        w->x + opts.shdw_x_offset - (r_offset >> 1),
        w->y + opts.shdw_y_offset - (r_offset >> 1),
        w->w + r_offset,
        w->h + r_offset
    };
}

/* Performs a simple 2D Gaussian blur of radius @radius on surface @surface. */
void blur_image_surface (cairo_surface_t * const surface, short const r)
{
    float kernel[2 * r + 1];
    int const size = ARRAY_LENGTH (kernel);
    int const half = size / 2;

    union raw_pixel_data {
        struct {
            uint8_t b; /* Order correct only for little endian. */
            uint8_t g;
            uint8_t r;
            uint8_t a;
        };
        uint32_t pix;
        uint8_t subpix[4];
    } * restrict s, * restrict d;

    if (cairo_surface_status(surface))
        return;

    unsigned int const width  = cairo_image_surface_get_width(surface);
    unsigned int const height = cairo_image_surface_get_height(surface);

    cairo_surface_t * const tmp = cairo_image_surface_create(
            CAIRO_FORMAT_ARGB32, width, height);

    if (cairo_image_surface_get_format(surface) != CAIRO_FORMAT_ARGB32 || !tmp)
        return;

    unsigned int const src_stride = cairo_image_surface_get_stride(surface);
    unsigned int const dst_stride = cairo_image_surface_get_stride(tmp);
    uint8_t * const restrict  src = cairo_image_surface_get_data(surface);
    uint8_t * const restrict  dst = cairo_image_surface_get_data(tmp);

    /* Prepare the kernel matrix. */
    {
        float sum = 0;

        for (int i = 0; i < size; i++) {
            const float f = i - half;
            sum += kernel[i] = (1 / sqrt(2 * M_PI * r)) * exp(- (f * f) / (2 * r * r));
        }

        for (int i = 0; i < size; i++)
            kernel[i] /= sum;
    }

    /* Horizontally blur from surface -> tmp */
    for (int i = 0; i < height; i++) {
	    s = (union raw_pixel_data *) (src + i * src_stride);
	    d = (union raw_pixel_data *) (dst + i * dst_stride);

	    for (int j = 0; j < width; j++) {
            if ((r << 2) < j && j < width - (r) >> 2) {
                d[j].a = s[j].a;
                continue;
            }

            float x = 0;
            for (int k = 0; k < size; k++) {
                if (j - half + k >= 0 && j - half + k < width)
                    x += s[j - half + k].a * kernel[k];
            }
            d[j].a = (uint8_t)x;
        }
    }

    /* Then vertically blur from tmp -> surface */
    for (int i = 0; i < height; i++) {
        s = (union raw_pixel_data *) (dst + i * dst_stride);
        d = (union raw_pixel_data *) (src + i * src_stride);

        for (int j = 0; j < width; j++) {
            if ((r << 2) <= i && i < width - (r) >> 2) {
                d[j].a = s[j].a;
                continue;
            }

            float x = 0;
            for (int k = 0; k < size; k++) {
                if (i - half + k >= 0 && i - half + k < height) {
                    s = (union raw_pixel_data *) (dst + (i - half + k) * dst_stride);
                    x += s[j].a * kernel[k];
                }
            }
            d[j].a = (uint8_t)x;
        }
    }

    cairo_surface_destroy(tmp);
    cairo_surface_mark_dirty(surface);
}

static inline cairo_surface_t *wutls_shadow_make (win_t const * const w)
{
    int const center = (int)ceil(3 * opts.shdw_radius + 1) >> 1;

    XRectangle sr = wutls_shadow_ext(w);
    XRectangle br = wutls_border_ext(w);

    cairo_surface_t *shdw_surf = cairo_image_surface_create(
            CAIRO_FORMAT_ARGB32, sr.width, sr.height);

    cairo_t *c = cairo_create(shdw_surf);
    cairo_rectangle(c, center, center, br.width, br.height);
    cairo_set_source_rgba(c, 0, 0, 0, opts.shdw_opacity);
    cairo_fill(c);
    cairo_destroy(c);

    blur_image_surface(shdw_surf, opts.shdw_radius);

    return shdw_surf;
}

static inline void win_this_damage (win_t * const w)
{
    XRectangle r = wutls_border_ext(w);
    XserverRegion w_reg = XFixesCreateRegion(display, &r, 1);

    dmg_glbl_add(w_reg);

    // FIXME: possible bug in win_unmap connected with this check
    if (w->shadow) {
        XRectangle r_s = wutls_shadow_ext(w);
        dmg_glbl_add(XFixesCreateRegion(display, &r_s, 1));
    }
}

static inline Atom win_props_det (Window const wid)
{
    Atom ret_atm = atoms.win_type_norm;
    Atom actual_type;
    int actual_format;
    unsigned long nitems, bytes_after;
    Atom *prop;

    int const ret = XGetWindowProperty(display, wid, atoms.win_type, 0L, 1L, False,
            XA_ATOM,
            &actual_type,
            &actual_format,
            &nitems,
            &bytes_after,
            (unsigned char **)&prop);

    if (ret == Success) {
        if (actual_type != None)
            ret_atm = *prop;

        XFree(prop);
    }

    return ret_atm;
}

static win_t * win_add (Window const wid, bool const existing)
{
    XWindowAttributes attr;

    if (XERR_IGNORE_FAULT(XGetWindowAttributes(display, wid, &attr))) {
        win_t * const w = malloc(sizeof(win_t));
        if (!w)
            errx(EXIT_FAILURE, "Cannot allocate memory\n");
        memset(w, 0, sizeof(win_t));

        w->wid    = wid;
        w->screen = attr.screen;
        w->state  = attr.map_state;
        w->vis    = attr.visual;
        w->type   = win_props_det(wid);

        w->b = attr.border_width;
        w->x = attr.x;
        w->y = attr.y;
        w->w = attr.width;
        w->h = attr.height;

        w->shadow_sag.geom.r  = wutls_shadow_ext(w);
        w->content_sag.geom.r = wutls_border_ext(w);

        list_append(&win_list, w);

        if (existing && w->state == IsViewable)
            win_map(wid);

        return w;
    }

    return NULL;
}

static void win_del (Window const wid)
{
    win_t * const w = win_find(wid);

    if (w) {
        if (w->content) {
            XFreePixmap(display, cairo_xlib_surface_get_drawable(w->content));
            cairo_surface_destroy(w->content);
        }

        if (w->shadow)
            cairo_surface_destroy(w->shadow);

        if (w->damage)
            XERR_IGNORE_FAULT(XDamageDestroy(display, w->damage));

        win_this_damage(w);
        list_delete(&win_list, (list_elem_t *)w);
        free(w);
    }
}

static Pixmap root_get_pixmap (Window const rw)
{
    Pixmap ret_pm = None;
    char const * const bgrnd_props[] = {
        ATOM_WIN_BGRND_XROOTPMAP,
        ATOM_WIN_BGRND_XSETROOT
    };

    for (size_t i = 0; i < ARRAY_LENGTH(bgrnd_props) && ret_pm == None; i++) {
        Atom name_atom = XInternAtom(display, bgrnd_props[i], False);

        Atom actual_type;
        int actual_format;
        unsigned long nitems, bytes_after;
        Pixmap *prop;

        int const ret = XGetWindowProperty(display, rw, name_atom, 0L, 1L, False,
                XA_PIXMAP,
                &actual_type,
                &actual_format,
                &nitems,
                &bytes_after,
                (unsigned char **)&prop);

        if (ret == Success) {
            if (actual_type != None)
                ret_pm = *prop;

            XFree(prop);
        }
    }

    return ret_pm;
}

static inline win_t * win_add_root (Window const rw)
{
    win_t * const w = win_add(rw, false);
    
    if (w) {
        w->state = IsViewable;

        Pixmap pm = root_get_pixmap(rw);
        if (pm)
            w->content = cairo_xlib_surface_create(display, pm, w->vis, w->w, w->h);
    }

    return w;
}

static void win_config (XConfigureEvent const * const ev)
{
    win_t * const w = win_find(ev->window);

    if (w) {
        win_this_damage(w);

        w->x = ev->x;
        w->y = ev->y;

        if (win_size_changed(w, ev)) {
            w->w = ev->width;
            w->h = ev->height;
            w->b = ev->border_width;

            if (w->content) {
                XRectangle const b_extd = wutls_border_ext(w);
                XFreePixmap(display, cairo_xlib_surface_get_drawable(w->content));
                cairo_xlib_surface_set_drawable(w->content,
                        XCompositeNameWindowPixmap(display, w->wid),
                        b_extd.width,
                        b_extd.height);
            }

            if (w->shadow) {
                cairo_surface_destroy(w->shadow);
                w->shadow = wutls_shadow_make(w);
            }
        }

        /* Restacking the root window (item with null predecessor) is a pure nonsens. */
        win_t *win_abv = win_find(ev->above);
        if (w->prev && ev->above != ((win_t *)w->prev)->wid && win_abv) {
            list_delete(&win_list, w);
            list_insert(&win_list, win_abv, w);

            w->above = ev->above;
        }

        w->shadow_sag.geom.r  = wutls_shadow_ext(w);
        w->content_sag.geom.r = wutls_border_ext(w);
        win_this_damage(w);
    }
}

static void win_map (Window const wid)
{
    win_t * const w = win_find(wid);

    if (w) {
        w->state = IsViewable;

        if (!w->damage)
            w->damage = XDamageCreate(display, wid, XDamageReportNonEmpty);

        XRectangle const b_extd = wutls_border_ext(w);
        if (w->content) {
            XFreePixmap(display, cairo_xlib_surface_get_drawable(w->content));

            cairo_xlib_surface_set_drawable(w->content,
                    XCompositeNameWindowPixmap(display, w->wid),
                    b_extd.width,
                    b_extd.height);
        }
        else {
            w->content = cairo_xlib_surface_create(display,
                    XCompositeNameWindowPixmap(display, w->wid),
                    w->vis,
                    b_extd.width,
                    b_extd.height);
        }

        if (win_want_shadow(w) && !w->shadow)
            w->shadow = wutls_shadow_make(w);

        win_this_damage(w);
    }
}

static void win_unmap (Window const wid)
{
    win_t * const w = win_find(wid);

    if (w) {
        w->state = IsUnmapped;

        if (w->damage) {
            XERR_IGNORE_FAULT(XDamageDestroy(display, w->damage));
            w->damage = None;
        }

        win_this_damage(w);
    }
}

static inline void root_paint (
        cairo_surface_t  * const restrict dst,
        cairo_surface_t  * const restrict src, 
        XRectangle const * const restrict r,
        size_t const                      nrects,
        unsigned int                      x,
        unsigned int                      y)
{
    cairo_t * const cr = cairo_create(dst);

    cairo_set_source_surface(cr, src, x, y);
    cairo_set_operator(cr, CAIRO_OPERATOR_OVER);

    for (size_t i = 0; i < nrects; i++)
        cairo_rectangle(cr, r[i].x, r[i].y, r[i].width, r[i].height);

    cairo_fill(cr);
    cairo_destroy(cr);
}

bool list_cb_cmp_win (list_elem_t *e, list_cb_arg_t *arg)
{
    win_t const * const w = (win_t const * const)e;

    return (w->wid == arg->wid);
}

static inline XserverRegion paint_do_dirty_work (
        cairo_surface_t * const dst_surf,
        cairo_operator_t  const op,
        surf_geom_t     * const sag)
{
    int nrects;

    XserverRegion const w_reg = XFixesCreateRegion(display, &(sag->geom.r), 1);
    XFixesIntersectRegion(display, w_reg, dmg_glbl_reg, w_reg);
    XRectangle * const r = XFixesFetchRegion(display, w_reg, &nrects);

    if (nrects > 0) {
        cairo_t * const cr = cairo_create(dst_surf);
        cairo_set_source_surface(cr, sag->surf, sag->geom.x, sag->geom.y);

        for (size_t i = 0; i < nrects; i++)
            cairo_rectangle(cr, r[i].x, r[i].y, r[i].width, r[i].height);

        cairo_set_operator(cr, op);
        cairo_fill(cr);
        cairo_destroy(cr);
    }

    XFree(r);
    return w_reg;
}

bool list_cb_paint_item (list_elem_t * const e, list_cb_arg_t *unsed_arg)
{
    win_t * const w = (win_t * const)e;

    if (w && is_visible(w)) {
        if (w->content) {
            XserverRegion const w_reg = paint_do_dirty_work(cs,
                    CAIRO_OPERATOR_SOURCE, &(w->content_sag));
            XFixesSubtractRegion(display, dmg_glbl_reg, dmg_glbl_reg, w_reg);
            XFixesDestroyRegion(display, w_reg);
        }

        if (w->shadow) {
            XserverRegion const w_reg = paint_do_dirty_work(cs_alp,
                    CAIRO_OPERATOR_DEST_OVER, &(w->shadow_sag));
            XFixesDestroyRegion(display, w_reg);
        }
    }

    return true;
}

bool list_cb_print_item (list_elem_t *e, list_cb_arg_t *unsed_arg)
{
    win_t const * const w = (win_t *)e;
    char *win_name;

    XFetchName(display, w->wid, &win_name);
    printf("Window id 0x%x [%s]\n", (int)w->wid, win_name);

    XFree(win_name);

    return true;
}

int main (int argc, char *argv[])
{
    int opt;
    while ((opt = getopt(argc, argv, "x:y:r:o:h")) != -1) {
        switch (opt) {
            case 'x':
                opts.shdw_x_offset = atoi(optarg);
                break;
            case 'y':
                opts.shdw_y_offset = atoi(optarg);
                break;
            case 'r':
                opts.shdw_radius   = abs(atoi(optarg));
                break;
            case 'o':
                opts.shdw_opacity  = fmax(0.0, fmin(1.0, atof(optarg)));
                break;
            case 'h':
            default:
                errx(EXIT_FAILURE, "Usage: %s [-x | -y offset] "
                        "[-r radius] [-o opacity]", argv[0]);
        }
    }

    display = XOpenDisplay(NULL);
    if (display == NULL)
        errx(EXIT_FAILURE, "Cannot open display\n");

    int comp_opcode, comp_ev, comp_err;
    if (!XQueryExtension (display, COMPOSITE_NAME, &comp_opcode, &comp_ev, &comp_err))
        errx(EXIT_FAILURE, "No composite extension\n");

    int damage_event, damage_error;
    if (!XDamageQueryExtension (display, &damage_event, &damage_error))
        errx(EXIT_FAILURE, "No damage extension\n");

    /* Get atoms for specified properties. */
    {
        char * names[] = {
            ATOM_WIN_TYPE,
            ATOM_WIN_TYPE_DESKTOP,
            ATOM_WIN_TYPE_DOCK,
            ATOM_WIN_TYPE_NORMAL,
            ATOM_WIN_OPACITY
        };
        XInternAtoms(display, names, ARRAY_LENGTH(atoms.as_array), True, atoms.as_array);
    }

    list_init(&win_list);
    root = win_add_root(DefaultRootWindow(display));

    XCompositeRedirectSubwindows(display, root->wid, CompositeRedirectManual);
    {
        Window *children = NULL;
        Window root_ret;
        Window parent_ret;
        unsigned int nchildren;

        XSelectInput(display, root->wid, SubstructureNotifyMask |
                ExposureMask                                    |
                StructureNotifyMask                             |
                PropertyChangeMask);

        XQueryTree(display, root->wid, &root_ret, &parent_ret,
                &children, &nchildren);

        for (unsigned int i = 0; i < nchildren; i++)
            win_add(children[i], true); /* True means "add an existing window". */

        if (children)
            XFree(children);

        IF_DEBUG(list_foreach(&win_list, list_cb_print_item, NULL));
    }
    
    Window overlay = XCompositeGetOverlayWindow(display, root->wid);
    {
        XserverRegion const region = XFixesCreateRegion(display, NULL, 0);
 
        XFixesSetWindowShapeRegion(display, overlay, ShapeBounding, 0, 0, 0);
        XFixesSetWindowShapeRegion(display, overlay, ShapeInput, 0, 0, region);

        XFixesDestroyRegion(display, region);
    }

    /* event loop */
    while (true) {
        XEvent event;
        XNextEvent(display, &event);

        switch (event.type) {
            case CreateNotify:
                if (event.xcreatewindow.window == overlay) {
                    cs = cairo_xlib_surface_create(display,
                            XdbeAllocateBackBufferName(display, overlay, XdbeUndefined),
                            DefaultVisual(display, DefaultScreen(display)), root->w, root->h);

                    XserverRegion const root_reg = XFixesCreateRegion(display, &(root->r), 1);
                    dmg_glbl_add(root_reg);
                }
                else
                    win_add(event.xcreatewindow.window, false);
                break;
            case ConfigureNotify:
                win_config(&event.xconfigure);
                break;
            case DestroyNotify:
                win_del(event.xdestroywindow.window);
                break;
            case MapNotify:
                if (event.xmap.window != overlay)
                    win_map(event.xmap.window);
                break;
            case UnmapNotify:
                if (event.xunmap.window != overlay)
                    win_unmap(event.xunmap.window);
                break;
            case ReparentNotify:
            case CirculateNotify:
            case Expose:
                break;
            case PropertyNotify:
                break;
            default:
                if (damage_event + XDamageNotify) {
                    XDamageNotifyEvent const * const dmg_ev = (XDamageNotifyEvent *)&event;
                    win_t const * const w = win_find(dmg_ev->drawable);

                    if (w && w->damage) {
                        XserverRegion parts = XFixesCreateRegion(display, NULL, 0);
                        XDamageSubtract(display, w->damage, None, parts);
                        XFixesTranslateRegion(display, parts, w->x + w->b, w->y + w->b);
                        dmg_glbl_add(parts);
                    }
                }
                break;
        }

        /* Repaint all windows if needed and the event queue is empty. */
        if (!QLength(display) && dmg_glbl_reg && cs) {
            cs_alp = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, root->w, root->h);

            int nrects;
            XRectangle * const r = XFixesFetchRegion(display, dmg_glbl_reg, &nrects);

            list_foreach(&win_list, list_cb_paint_item, NULL);
            root_paint(cs, cs_alp, r, nrects, 0, 0);

            XdbeSwapInfo si = { overlay, XdbeUndefined };
            XdbeSwapBuffers(display, &si, 1);

            XFree(r);
            cairo_surface_destroy(cs_alp);
            XFixesDestroyRegion(display, dmg_glbl_reg);
            dmg_glbl_reg = None;
        }
    }

    XCloseDisplay(display);

    return EXIT_SUCCESS;
}
