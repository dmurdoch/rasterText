#define R_NO_REMAP
#include <fontconfig/fontconfig.h>
#include <pango/pangocairo.h>  // Main PangoCairo integration
#include <pango/pango.h>       // Pango layout and font handling
#include <cairo.h>             // Base Cairo surface and context functions
#include <stdlib.h>            // For malloc/free if you allocate buffers
#include <R.h>
#include <Rinternals.h>

/* The version number will be an integer, changing only
 * when the C API changes incompatibly
 */

void R_init_rasterText(DllInfo *dll);

int API_version(void);

SEXP measure_textR(SEXP texts, SEXP family, SEXP font,
                  SEXP fontfile, SEXP size);

SEXP draw_text_to_rasterR(SEXP x, SEXP y, SEXP texts,
                         SEXP family, SEXP font,
                         SEXP fontfile, SEXP size,
                         SEXP monochrome,
                         SEXP color,
                         SEXP width, SEXP height);

SEXP pack_text_R(SEXP texts, SEXP measure, SEXP width);

/* This is currently a copy of
 * cairo_text_extents_t, followed by ascent and descent from cairo_font_extents_t
 */

typedef struct text_extents
{
  double height, width,
         x_advance, x_bearing,
         y_advance, y_bearing,
         ascent, descent,
         baseline;
} text_extents_t;

#define EXTENT_SIZE 9

typedef struct text_placement
{
  double x, y;
} text_placement_t;

/* NB:  This function fills `result` in the
 * order a transposed R matrix would use, i.e.
 * all values for the first text, then all for
 * the second, etc. */
text_extents_t* measure_text(int n,
                 const char *text[],
                 const char *family,
                 int font,
                 const char *fontfile,
                 double size,
                 text_extents_t *result);

int pack_text(int n, const char *texts[],
              text_extents_t* measures, text_placement_t *placement, int width);

int get_buffer_stride(int width, Rboolean mono);

void draw_text_to_buffer(int n,
                         const text_placement_t *xy,
                         const char *text[],
                         const char *family, int font,
                         const char *fontfile, double size,
                         Rboolean mono,
                         int *color,
                         int width, int height, int stride,
                         unsigned char *buffer);

int API_version(void) {
  return 7;
}

/* In this one, keys is an array of char pointers,
 * all of which share the same characteristics. If the keys are equal,
 * the bitmap can be reused. There
 * are n elements in the array */

/* returns the height that has been used, and writes
 * x and y into placement.  Uses greedy packing. */

/* EXTRA is the extra buffering on each side of each
 * string */
#define EXTRA 1
int pack_text(int n, const char *keys[], text_extents_t* measures,
              text_placement_t* placement, int width) {
  int *used = calloc(n, sizeof(int));
  int usedwidth = EXTRA, usedheight = EXTRA, maxheight=0;
  for (int i=0; i<n; i++) {
    for (int j=i; j<n; j++) {
      if (!used[j]) {
        if (usedwidth + measures[j].width + EXTRA > width) {
          if (j > i) continue;
          /* go to next line */
          usedheight = usedheight + maxheight + EXTRA;
          usedwidth = EXTRA;
        }
        placement[j].x = usedwidth - measures[j].x_bearing;
        placement[j].y = usedheight - measures[j].y_bearing - measures[j].ascent;
        usedwidth += measures[j].width + EXTRA;
        if (measures[j].height > maxheight) maxheight = measures[j].height;
        used[j] = 1;
        for (int k=j+1; k < n; k++)
          if (!used[k] && !strcmp(keys[j], keys[k])) {
            placement[k].x = placement[j].x;
            placement[k].y = placement[j].y;
            used[k] = 1;
          }
      }
    }
  }
  free(used);
  return usedheight + maxheight + EXTRA;
}

SEXP pack_textR(SEXP keys, SEXP measure, SEXP width) {
  int n = Rf_length(keys), height = 0;
  SEXP placement;
  PROTECT(placement = Rf_allocVector(REALSXP, 2*n));
  if (n > 0) {
    const char *key0[n];
    text_extents_t text_extents[n];
    text_placement_t text_placement[n];
    for (int i = 0; i < n; i++) {
      key0[i] = CHAR(STRING_ELT(keys, i));
      text_extents[i].height = REAL(measure)[EXTENT_SIZE*i];
      text_extents[i].width = REAL(measure)[EXTENT_SIZE*i+1];
      text_extents[i].x_advance = REAL(measure)[EXTENT_SIZE*i+2];
      text_extents[i].x_bearing = REAL(measure)[EXTENT_SIZE*i+3];
      text_extents[i].y_advance = REAL(measure)[EXTENT_SIZE*i+4];
      text_extents[i].y_bearing = REAL(measure)[EXTENT_SIZE*i+5];
      text_extents[i].ascent = REAL(measure)[EXTENT_SIZE*i+6];
      text_extents[i].descent = REAL(measure)[EXTENT_SIZE*i+7];
    }
    height = pack_text(n, key0,
                           text_extents,
                           text_placement,
                           INTEGER(width)[0]);
    for (int i = 0; i < n; i++) {
      REAL(placement)[i] = text_placement[i].x;
      REAL(placement)[i+n] = text_placement[i].y;
    }
  }
  Rf_setAttrib(placement, Rf_install("width"), width);
  Rf_setAttrib(placement, Rf_install("height"), Rf_ScalarInteger(height));
  UNPROTECT(1);
  return placement;
}

PangoFontDescription * getFontDesc(
    const char *family,
    int font,
    const char *fontfile,
    double size
)
{
  char *family_name = NULL;
  if (!fontfile || !strlen(fontfile))
    family_name = strdup(family);
  else {
    FcConfigAppFontAddFile(FcConfigGetCurrent(), (const FcChar8 *)fontfile);

    FcPattern *pattern = FcFreeTypeQuery((const FcChar8 *)fontfile, 0, NULL, NULL);
    if (pattern) {
      FcChar8 *fc_family;
      if (FcPatternGetString(pattern, FC_FAMILY, 0, &fc_family) == FcResultMatch) {
        family_name = strdup((char *)fc_family);
      }
      FcPatternDestroy(pattern);
    }
    if (!family_name)
      family_name = strdup("sans");
  }
  PangoFontDescription *desc = pango_font_description_new();
  pango_font_description_set_family(desc, family_name);
  pango_font_description_set_style(desc, font < 3 ? PANGO_STYLE_NORMAL : PANGO_STYLE_ITALIC);
  pango_font_description_set_weight(desc, font % 2 == 1 ? PANGO_WEIGHT_NORMAL : PANGO_WEIGHT_BOLD);
  pango_font_description_set_size(desc, size*PANGO_SCALE);
  free(family_name);
  return desc;
}

text_extents_t* measure_text(int n, const char *texts[], /* must be UTF-8! */
          const char *family,
          int font,
          const char *fontfile,
          double size,
          text_extents_t *measures) {
  if (!n)
    return measures;

  cairo_surface_t *temp_surface = cairo_recording_surface_create(CAIRO_CONTENT_COLOR_ALPHA, NULL);
  cairo_t *cr = cairo_create(temp_surface);

  // Create Pango layout
  PangoLayout *layout = pango_cairo_create_layout(cr);
  PangoContext *context = pango_layout_get_context(layout);
  // CRITICAL: Must match the measurement DPI (72.0)
  pango_cairo_context_set_resolution(context, 72.0);
  // Notify the layout that the context (DPI) has changed
  pango_layout_context_changed(layout);
  PangoFontDescription *desc = getFontDesc(family, font, fontfile, size);

  pango_layout_set_font_description(layout, desc);

  for (int i=0; i < n; i++)
  {
    pango_layout_set_text(layout, texts[i], -1);

    // Get Font Metrics (Ascent/Descent)
    PangoContext *context = pango_layout_get_context(layout);
    PangoFontMetrics *font_metrics = pango_context_get_metrics(context, desc, NULL);

    // Get Font Metrics (Ascent/Descent)
    measures[i].ascent = round(pango_font_metrics_get_ascent(font_metrics) / (double)PANGO_SCALE);
    measures[i].descent = round(pango_font_metrics_get_descent(font_metrics) / (double)PANGO_SCALE);
    pango_font_metrics_unref(font_metrics);

    // Get Logical/Ink Extents (Width/Height/Bearing)
    PangoRectangle ink_rect, logical_rect;
    pango_layout_get_extents(layout, &ink_rect, &logical_rect);

    measures[i].x_bearing = round(ink_rect.x / (double)PANGO_SCALE);
    measures[i].y_bearing = round(ink_rect.y / (double)PANGO_SCALE - measures[i].ascent);
    measures[i].width     = round(ink_rect.width / (double)PANGO_SCALE);
    measures[i].height    = round(ink_rect.height / (double)PANGO_SCALE);
    measures[i].x_advance = round(logical_rect.width / (double)PANGO_SCALE);
    measures[i].y_advance = 0;

    PangoLayoutIter *iter = pango_layout_get_iter(layout);
    measures[i].baseline = pango_layout_iter_get_baseline(iter) / PANGO_SCALE;
    pango_layout_iter_free(iter);
  }
  // Cleanup
  pango_font_description_free(desc);
  g_object_unref(layout);
  cairo_destroy(cr);
  cairo_surface_destroy(temp_surface);
  return measures + n;
}

SEXP measure_textR(SEXP texts, SEXP family, SEXP font,
                   SEXP fontfile, SEXP size) {

  /* texts must be UTF8 */
  SEXP result;
  int n = Rf_length(texts), done = 0;
  PROTECT(result = Rf_allocVector(REALSXP, n*EXTENT_SIZE));
  if (n > 0) {
    text_extents_t text_extents[n],
                   *res = text_extents;
    SEXP family0, fontfile0;
    int font0;
    double size0;
    Rboolean useFreetype = !Rf_isNull(fontfile);
    for (int i=0; i <= n; i++) {
      if(i == 0 || i == n ||
         STRING_ELT(family, i) != family0 ||
         INTEGER(font)[i] != font0 ||
         REAL(size)[i] != size0 ||
         (useFreetype && STRING_ELT(fontfile, i) != fontfile0)) {
        /* make the call for the previous combination */
        if (i > done) {
          const char *text0[i - done];
          for (int j = 0; j < i - done; j++)
            text0[j] = CHAR(STRING_ELT(texts, j + done));

          res = measure_text(i - done, text0, CHAR(family0),
                             font0,
                             useFreetype ? CHAR(fontfile0) : NULL,
                             size0, res);
          done = i;
        }
        if (i < n) {
          family0 = STRING_ELT(family, i);
          font0 = INTEGER(font)[i];
          size0 = REAL(size)[i];
          if (useFreetype)
            fontfile0 = STRING_ELT(fontfile, i);
        }
      }
    }
    for (int i=0; i < n; i++) {
      REAL(result)[EXTENT_SIZE*i + 0] = text_extents[i].height;
      REAL(result)[EXTENT_SIZE*i + 1] = text_extents[i].width;
      REAL(result)[EXTENT_SIZE*i + 2] = text_extents[i].x_advance;
      REAL(result)[EXTENT_SIZE*i + 3] = text_extents[i].x_bearing;
      REAL(result)[EXTENT_SIZE*i + 4] = text_extents[i].y_advance;
      REAL(result)[EXTENT_SIZE*i + 5] = text_extents[i].y_bearing;
      REAL(result)[EXTENT_SIZE*i + 6] = text_extents[i].ascent;
      REAL(result)[EXTENT_SIZE*i + 7] = text_extents[i].descent;
      REAL(result)[EXTENT_SIZE*i + 8] = text_extents[i].baseline;
    }
  }
  UNPROTECT(1);
  return result;
}


int get_buffer_stride(int width, Rboolean mono) {
  return cairo_format_stride_for_width(mono ? CAIRO_FORMAT_A8 : CAIRO_FORMAT_ARGB32, width);
}

/* draws alpha values to existing buffer of unsigned char. */

void draw_text_to_buffer(int n, const text_placement_t *xy,
                         const char *text[],
                         const char *family, int font,
                         const char *fontfile, double size,
                         Rboolean mono,
                         int *color,
                         int width, int height, int stride,
                         unsigned char *buffer) {

  // Cairo A8 format: 1 byte per pixel (Alpha channel only)
  // Cairo ARGB32 format:  alpha in high 8 bits, RGB
  // in the rest, pre-multiplied alpha
  cairo_surface_t *surface = cairo_image_surface_create_for_data(
    buffer,
    mono ? CAIRO_FORMAT_A8 : CAIRO_FORMAT_ARGB32,
    width, height, stride);
  cairo_t *cr = cairo_create(surface);

  PangoLayout *layout = pango_cairo_create_layout(cr);
  PangoContext *context = pango_layout_get_context(layout);
  // CRITICAL: Must match the measurement DPI (72.0)
  pango_cairo_context_set_resolution(context, 72.0);
  // Notify the layout that the context (DPI) has changed
  pango_layout_context_changed(layout);
  PangoFontDescription *desc = getFontDesc(family, font, fontfile, size);
  pango_layout_set_font_description(layout, desc);

  if (mono)
    cairo_set_source_rgba(cr, 1, 1, 1, 1);
  else
    cairo_set_source_rgba(cr, color[0]/255.0, color[1]/255.0,
                              color[2]/255.0, color[3]/255.0);
  for (int i = 0; i < n; i++) {
    pango_layout_set_text(layout, text[i], -1);

    // Pango renders at the baseline by default in many contexts, but
    // pango_cairo_show_layout renders with the top-left of the logical
    // box at the current point.
    cairo_move_to(cr, xy[i].x, xy[i].y);
    pango_cairo_show_layout(cr, layout);
  }
  pango_font_description_free(desc);
  g_object_unref(layout);
  cairo_destroy(cr);
  cairo_surface_destroy(surface);
}

SEXP draw_text_to_rasterR(SEXP x, SEXP y, SEXP texts,
                         SEXP family, SEXP font,
                         SEXP fontfile, SEXP size,
                         SEXP monochrome,
                         SEXP color,
                         SEXP width, SEXP height) {

  /* texts must be UTF8 */
  int n = Rf_length(texts), m;
  Rboolean mono = Rf_asBool(monochrome);
  const char *text0[n], *family0[n], *fontfile0[n];
  int font0[n];
  double size0[n];
  for (int i = 0; i < n; i++) {
    text0[i] = CHAR(STRING_ELT(texts, i));
    if ((m = Rf_length(family)) > 0)
      family0[i] = CHAR(STRING_ELT(family, i % m ));
    else
      family0[i] = 0;
    if ((m = Rf_length(fontfile)) > 0)
      fontfile0[i] = CHAR(STRING_ELT(fontfile, i % m));
    else
      fontfile0[i] = 0;
    if ((m = Rf_length(font)) > 0)
      font0[i] = INTEGER(font)[i % m];
    else
      font0[i] = 1;
    if ((m = Rf_length(size)) > 0)
      size0[i] = REAL(size)[i % m];
    else
      size0[i] = 1.0;
  }

  int w = INTEGER(width)[0], h = INTEGER(height)[0],
      stride = get_buffer_stride(w, mono);
  unsigned char *buffer = calloc(stride*h, sizeof(unsigned char));

  double *x0 = REAL(x), *y0 = REAL(y);
  text_placement_t xy[n];
  for (int i=0; i < n; i++) {
    xy[i].x = x0[i];
    xy[i].y = y0[i];
  }
  int done = 0;
  for (int i = 1; i <= n; i++) {
    if (i == n ||
        size0[i] != size0[i - 1] ||
        family0[i] != family0[i - 1] ||
        font0[i] != font0[i - 1] ||
        fontfile0[i] != fontfile0[i - 1]) {
      draw_text_to_buffer(i - done,
                          xy + done,
                          text0 + done,
                          family0[done],
                          font0[done],
                          fontfile0[done],
                          size0[done],
                          mono,
                          INTEGER(color),
                          w, h, stride,
                          buffer);
        done = i;
    }
  }

  SEXP result;
  PROTECT(result = Rf_allocVector(INTSXP, mono ? w*h : w*h*4));
  int *res = INTEGER(result);
  int k = 0;
  for (int i = 0; i < h; i++) {
    if (mono) {
      unsigned char *row = buffer + i*stride;
      for (int j = 0; j < w; j++)
        res[k++] = row[j];
    } else {
      uint32_t *row32 = (uint32_t*)(buffer + i*stride);
      for (int j = 0; j < w; j++) {
        uint32_t* argb = row32 + j;
        uint8_t a = (*argb) >> 24;
        if (a == 0) {
          res[k++] = 0;
          res[k++] = 0;
          res[k++] = 0;
          res[k++] = 0;
        } else {
          uint8_t
                r = ((((*argb) >> 16) & 0xFF) * 255) / a,
                g = ((((*argb) >> 8) & 0xFF) * 255) / a,
                b = (((*argb) & 0xFF) * 255) / a;
          res[k++] = r;
          res[k++] = g;
          res[k++] = b;
          res[k++] = a;
        }
      }
    }
  }
  free(buffer);

  UNPROTECT(1);
  return result;
}

#define STRINGIZE(x) #x
#define RNAME(name) STRINGIZE(C_##name)
#define CALLDEF(name, n) {RNAME(name), (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
  CALLDEF(measure_textR, 5),
  CALLDEF(pack_textR, 3),
  CALLDEF(draw_text_to_rasterR, 10),
  {NULL, NULL, 0}
};

void R_init_rasterText(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);

  R_RegisterCCallable("rasterText", "API_version",
                      (DL_FUNC)API_version);
  R_RegisterCCallable("rasterText", "measure_text", (DL_FUNC)measure_text);
  R_RegisterCCallable("rasterText", "get_buffer_stride", (DL_FUNC)get_buffer_stride);
  R_RegisterCCallable("rasterText", "pack_text", (DL_FUNC)pack_text);
  R_RegisterCCallable("rasterText", "draw_text_to_buffer",
                      (DL_FUNC)draw_text_to_buffer);
}
