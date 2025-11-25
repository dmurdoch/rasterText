#define R_NO_REMAP
#include "cconfig.h"
#include <cairo.h>
#include <cairo-ft.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include <R.h>
#include <Rinternals.h>

/* The version number will be an integer, changing only
 * when the C API changes
 */

void R_init_rasterText(DllInfo *dll);

int API_version(void);

SEXP measure_text(SEXP texts, SEXP family, SEXP font,
                  SEXP fontfile, SEXP size);

SEXP draw_text_to_raster(SEXP x, SEXP y, SEXP texts,
                         SEXP rgba, SEXP family, SEXP font,
                         SEXP fontfile, SEXP size,
                         SEXP width, SEXP height);

static void select_font_face(cairo_t *cr, const char *family, int font, double size) {
  cairo_font_slant_t slants[] = {CAIRO_FONT_SLANT_NORMAL,
                                 CAIRO_FONT_SLANT_NORMAL,
                                 CAIRO_FONT_SLANT_ITALIC,
                                 CAIRO_FONT_SLANT_ITALIC};
  cairo_font_weight_t weights[] = {CAIRO_FONT_WEIGHT_NORMAL,
                                   CAIRO_FONT_WEIGHT_BOLD,
                                   CAIRO_FONT_WEIGHT_NORMAL,
                                   CAIRO_FONT_WEIGHT_BOLD};
  cairo_select_font_face(cr,
                         family,
                         slants[font],
                         weights[font]);

  cairo_set_font_size(cr, size);
}

static void select_font_file(cairo_t *cr, FT_Library ft,
                             const char *fontfile, double size) {
  // Load a font face
  FT_Face face;
  if (FT_New_Face(ft, fontfile, 0, &face)) {
    Rprintf("Could not open font file: %s\n", fontfile);
    FT_Done_FreeType(ft);
    return;
  }

  FT_Set_Pixel_Sizes(face, 0, (unsigned int)size);

  cairo_font_face_t *cface = cairo_ft_font_face_create_for_ft_face(face, 0);
  cairo_set_font_face(cr, cface);
  cairo_set_font_size(cr, size);
}

int API_version(void) {
  return 1;
}

SEXP measure_text(SEXP texts, SEXP family, SEXP font,
                  SEXP fontfile, SEXP size) {

  /* texts must be UTF8 */
  SEXP result;
  int n = Rf_length(texts), m = 6;
  PROTECT(result = Rf_allocVector(REALSXP, n*m));
  double *res = REAL(result);

  cairo_surface_t *surface;
  cairo_t *cr;
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 100, 100);
  cr = cairo_create (surface);

  FT_Library ft;
  Rboolean useFreetype = !Rf_isNull(fontfile) &&
                         !FT_Init_FreeType(&ft);

  cairo_text_extents_t te;
  for (int i=0; i < n; i++) {
    if (!useFreetype) {
      if (i == 0
            || STRING_ELT(family, i) != STRING_ELT(family, i - 1)
            || INTEGER(font)[i] != INTEGER(font)[i - 1]
            || REAL(size)[i] != REAL(size)[i - 1])
            select_font_face(cr, CHAR(STRING_ELT(family, i)),
                             INTEGER(font)[i] - 1, REAL(size)[i]);
    } else {
      if (i == 0
            || STRING_ELT(fontfile, i) != STRING_ELT(fontfile, i - 1)
            || REAL(size)[i] != REAL(size)[i - 1])
            select_font_file(cr, ft, CHAR(STRING_ELT(fontfile, i)),
                             REAL(size)[i]);
    }
    cairo_text_extents(cr, CHAR(STRING_ELT(texts, i)), &te);
    res[i]       = te.x_bearing;
    res[i + n]   = te.y_bearing;
    res[i + 2*n] = te.width;
    res[i + 3*n] = te.height;
    res[i + 4*n] = te.x_advance;
    res[i + 5*n] = te.y_advance;
  }
  cairo_status_t status = cairo_status(cr);
  if (status != CAIRO_STATUS_SUCCESS)
    Rprintf("measure_text error: %s\n", cairo_status_to_string(status));

  if (!Rf_isNull(fontfile))
    FT_Done_FreeType(ft);

  cairo_destroy(cr);
  cairo_surface_destroy(surface);
  UNPROTECT(1);
  return result;
}

SEXP draw_text_to_raster(SEXP x, SEXP y, SEXP texts,
                         SEXP rgba, SEXP family, SEXP font,
                         SEXP fontfile, SEXP size,
                         SEXP width, SEXP height) {

  /* texts must be UTF8 */
  int n = Rf_length(texts);

  cairo_surface_t *surface;
  cairo_t *cr;
  int w = INTEGER(width)[0], h = INTEGER(height)[0];
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                        w, h);
  cr = cairo_create (surface);

  FT_Library ft;
  Rboolean useFreetype = !Rf_isNull(fontfile) &&
    !FT_Init_FreeType(&ft);

  double *x0 = REAL(x), *y0 = REAL(y);

  for (int i = 0; i < n; i++) {
    if (!useFreetype) {
      if (i == 0
            || STRING_ELT(family, i) != STRING_ELT(family, i - 1)
            || INTEGER(font)[i] != INTEGER(font)[i - 1]
            || REAL(size)[i] != REAL(size)[i - 1])
        select_font_face(cr, CHAR(STRING_ELT(family, i)),
                       INTEGER(font)[i] - 1, REAL(size)[i]);
    } else {
      if (i == 0
            || STRING_ELT(fontfile, i) != STRING_ELT(fontfile, i - 1)
            || REAL(size)[i] != REAL(size)[i - 1])
        select_font_file(cr, ft, CHAR(STRING_ELT(fontfile, i)),
                       REAL(size)[i]);
    }
    cairo_move_to(cr, x0[i], y0[i]);
    double *c = REAL(rgba) + 4*i;
    cairo_set_source_rgba(cr, c[0], c[1], c[2], c[3]);
    cairo_show_text(cr, CHAR(STRING_ELT(texts, i)));
  }
  cairo_surface_flush(surface);
  int stride = cairo_image_surface_get_stride(surface);
  unsigned char *data = cairo_image_surface_get_data(surface);
  SEXP result;
  PROTECT(result = Rf_allocVector(INTSXP, w*h));
  int *res = INTEGER(result);
  int k = 0;
  for (int i = 0; i < h; i++) {
    uint32_t *row = (uint32_t*)(data + i*stride);
    for (int j = 0; j < w; j++)
      res[k++] = row[j] >> 24;
  }
  cairo_status_t status = cairo_status(cr);
  if (status != CAIRO_STATUS_SUCCESS)
    Rprintf("draw_text error: %s\n", cairo_status_to_string(status));

  if (!Rf_isNull(fontfile))
    FT_Done_FreeType(ft);

  cairo_destroy(cr);
  cairo_surface_destroy(surface);

  UNPROTECT(1);
  return result;
}

#define STRINGIZE(x) #x
#define RNAME(name) STRINGIZE(C_##name)
#define CALLDEF(name, n) {RNAME(name), (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
  CALLDEF(measure_text, 5),
  CALLDEF(draw_text_to_raster, 10),
  {NULL, NULL, 0}
};

void R_init_rasterText(DllInfo *dll)
{
  Rprintf("initializing rasterText\n");
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);

  R_RegisterCCallable("rasterText", "API_version",
                      (DL_FUNC)API_version);
  R_RegisterCCallable("rasterText", "measure_text", (DL_FUNC)measure_text);
  R_RegisterCCallable("rasterText", "draw_text_to_raster",
                      (DL_FUNC)draw_text_to_raster);
}
