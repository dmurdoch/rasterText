#define R_NO_REMAP
#include "cconfig.h"
#include <cairo.h>
#include <R.h>
#include <Rinternals.h>

SEXP measure_text(SEXP texts, SEXP family, SEXP font, SEXP size);

SEXP draw_text_to_raster(SEXP x, SEXP y, SEXP texts,
                         SEXP rgba, SEXP family, SEXP font, SEXP size,
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


SEXP measure_text(SEXP texts, SEXP family, SEXP font, SEXP size) {
  /* texts must be UTF8 */
  SEXP result;
  int n = Rf_length(texts), m = 6;
  PROTECT(result = Rf_allocVector(REALSXP, n*m));
  double *res = REAL(result);

  cairo_surface_t *surface;
  cairo_t *cr;
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 100, 100);
  cr = cairo_create (surface);


  select_font_face(cr, CHAR(STRING_ELT(family, 0)),
                   INTEGER(font)[0] - 1, REAL(size)[0]);

  cairo_text_extents_t te;
  for (int i=0; i < n; i++) {
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
  cairo_destroy(cr);
  cairo_surface_destroy(surface);
  UNPROTECT(1);
  return result;
}

SEXP draw_text_to_raster(SEXP x, SEXP y, SEXP texts,
                         SEXP rgba, SEXP family, SEXP font, SEXP size,
                         SEXP width, SEXP height) {
  /* texts must be UTF8 */
  int n = Rf_length(texts);

  cairo_surface_t *surface;
  cairo_t *cr;
  int w = INTEGER(width)[0], h = INTEGER(height)[0];
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                        w, h);
  cr = cairo_create (surface);

  select_font_face(cr, CHAR(STRING_ELT(family, 0)),
                   INTEGER(font)[0] - 1, REAL(size)[0]);
  double *c = REAL(rgba), *x0 = REAL(x), *y0 = REAL(y);
  cairo_set_source_rgba(cr, c[0], c[1], c[2], c[3]);
  for (int i = 0; i < n; i++) {
    cairo_move_to(cr, x0[i], y0[i]);
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
  cairo_destroy(cr);
  cairo_surface_destroy(surface);

  UNPROTECT(1);
  return result;
}
