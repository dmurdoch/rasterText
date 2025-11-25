#define R_NO_REMAP
#include "cconfig.h"
#include <cairo.h>
#include <cairo-ft.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include <R.h>
#include <Rinternals.h>

/* The version number will be an integer, changing only
 * when the C API changes incompatibly
 */

void R_init_rasterText(DllInfo *dll);

int API_version(void);

SEXP measure_textR(SEXP texts, SEXP family, SEXP font,
                  SEXP fontfile, SEXP size);

SEXP draw_text_to_raster(SEXP x, SEXP y, SEXP texts,
                         SEXP rgba, SEXP family, SEXP font,
                         SEXP fontfile, SEXP size,
                         SEXP width, SEXP height);

double* measure_text(int n,
                 const char *text[],
                 const char *family,
                 int font,
                 const char *fontfile,
                 double size,
                 double *result);

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

/* In this one, texts is an array of char pointers,
 * all of which share the same characteristics.  There
 * are n elements in the array */

double* measure_text(int n, const char *texts[], /* must be UTF-8! */
                  const char *family,
                  int font,
                  const char *fontfile,
                  double size,
                  double *result) {

  cairo_surface_t *surface;
  cairo_t *cr;
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 100, 100);
  cr = cairo_create (surface);

  FT_Library ft;
  int useFreetype = fontfile && !FT_Init_FreeType(&ft);

  cairo_text_extents_t te;

  if (!useFreetype)
    select_font_face(cr, family, font - 1, size);
  else
    select_font_file(cr, ft, fontfile, size);
  for (int i=0; i < n; i++) {
    cairo_text_extents(cr, texts[i], &te);
    *result++ = te.x_bearing;
    *result++ = te.y_bearing;
    *result++ = te.width;
    *result++ = te.height;
    *result++ = te.x_advance;
    *result++ = te.y_advance;
  }

  cairo_status_t status = cairo_status(cr);
  if (status != CAIRO_STATUS_SUCCESS)
    Rprintf("measure_text error: %s\n", cairo_status_to_string(status));

  if (fontfile)
    FT_Done_FreeType(ft);

  cairo_destroy(cr);
  cairo_surface_destroy(surface);
  return result;
}

SEXP measure_textR(SEXP texts, SEXP family, SEXP font,
                   SEXP fontfile, SEXP size) {

  /* texts must be UTF8 */
  SEXP result;
  int n = Rf_length(texts), m = 6, done = 0;
  PROTECT(result = Rf_allocVector(REALSXP, n*m));
  double *res = n > 0 ? REAL(result) : NULL;

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
      family0 = STRING_ELT(family, i);
      font0 = INTEGER(font)[i];
      size0 = REAL(size)[i];
      if (useFreetype)
        fontfile0 = STRING_ELT(fontfile, i);
    }
  }
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
  CALLDEF(measure_textR, 5),
  CALLDEF(draw_text_to_raster, 10),
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
  R_RegisterCCallable("rasterText", "measure_textR", (DL_FUNC)measure_textR);
  R_RegisterCCallable("rasterText", "draw_text_to_raster",
                      (DL_FUNC)draw_text_to_raster);
}
