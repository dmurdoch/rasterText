#define R_NO_REMAP
#include "cconfig.h"
#include <cairo.h>
#include <R.h>
#include <Rinternals.h>

SEXP measure_text(SEXP texts);

SEXP measure_text(SEXP texts) {
  /* texts must be UTF8 */
  SEXP result;
  int n = Rf_length(texts), m = 6;
  PROTECT(result = Rf_allocVector(REALSXP, n*m));
  double *res = REAL(result);

  cairo_surface_t *surface;
  cairo_t *cr;
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 100, 100);
  cr = cairo_create (surface);

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
  UNPROTECT(1);
  return result;
}
