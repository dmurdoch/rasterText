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

SEXP draw_text_to_rasterR(SEXP x, SEXP y, SEXP texts,
                         SEXP family, SEXP font,
                         SEXP fontfile, SEXP size,
                         SEXP width, SEXP height);

SEXP pack_text_R(SEXP texts, SEXP measure, SEXP width);

/* NB:  This function fills `result` in the
 * order a transposed R matrix would use, i.e.
 * all values for the first text, then all for
 * the second, etc. */
double* measure_text(int n,
                 const char *text[],
                 const char *family,
                 int font,
                 const char *fontfile,
                 double size,
                 double *result);

int pack_text(int n, const char *texts[],
              double *measures, double *placement, int width);

int get_buffer_stride(int width);

void draw_text_to_buffer(int n, double *x, double *y, const char *text[],
                         const char *family, int font,
                         const char *fontfile, double size,
                         int width, int height, int stride,
                         unsigned char *buffer);

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
  return 2;
}

/* In this one, texts is an array of char pointers,
 * all of which share the same characteristics.  There
 * are n elements in the array */

#define MT_NCOLS 6

#define MT_X_BEARING(i) measures[MT_NCOLS*i + 0]
#define MT_Y_BEARING(i) measures[MT_NCOLS*i + 1]
#define MT_WIDTH(i) measures[MT_NCOLS*i + 2]
#define MT_HEIGHT(i) measures[MT_NCOLS*i + 3]
#define MT_X_ADVANCE(i) measures[MT_NCOLS*i + 4]
#define MT_Y_ADVANCE(i) measures[MT_NCOLS*i + 5]

#define PT_X(i) placement[i]
#define PT_Y(i) placement[i + n]

/* returns the height that has been used, and writes
 * x and y into measures.  Uses greedy packing. */

/* EXTRA is the extra buffering on each side of each
 * string */
#define EXTRA 1
int pack_text(int n, const char *texts[], double* measures, double* placement, int width) {
  int *used = calloc(n, sizeof(int));
  int usedwidth = EXTRA, usedheight = EXTRA, maxheight=0;
  for (int i=0; i<n; i++) {
    for (int j=i; j<n; j++) {
      if (!used[j]) {
        if (usedwidth + MT_WIDTH(j) + EXTRA > width) {
          if (j > i) continue;
          /* go to next line */
          usedheight = usedheight + maxheight + EXTRA;
          usedwidth = EXTRA;
        }
        PT_X(j) = usedwidth - MT_X_BEARING(j);
        PT_Y(j) = usedheight - MT_Y_BEARING(j);
        usedwidth += MT_WIDTH(j) + EXTRA;
        if (MT_HEIGHT(j) > maxheight) maxheight = MT_HEIGHT(j);
        used[j] = 1;
        for (int k=j+1; k < n; k++)
          if (!used[k] && !strcmp(texts[j], texts[k])) {
            PT_X(k) = PT_X(j);
            PT_Y(k) = PT_Y(j);
            used[k] = 1;
          }
      }
    }
  }
  free(used);
  return usedheight + maxheight + EXTRA;
}

SEXP pack_textR(SEXP texts, SEXP measure, SEXP width) {
  int n = Rf_length(texts);
  SEXP placement;
  PROTECT(placement = Rf_allocVector(REALSXP, 2*n));
  const char *text0[n];
  for (int i = 0; i < n; i++)
    text0[i] = CHAR(STRING_ELT(texts, i));
  int height = pack_text(n, text0, REAL(measure), REAL(placement), INTEGER(width)[0]);
  Rf_setAttrib(placement, Rf_install("width"), width);
  Rf_setAttrib(placement, Rf_install("height"), Rf_ScalarInteger(height));
  UNPROTECT(1);
  return placement;
}

double* measure_text(int n, const char *texts[], /* must be UTF-8! */
                  const char *family,
                  int font,
                  const char *fontfile,
                  double size,
                  double *measures) {

  cairo_surface_t *surface;
  cairo_t *cr;
  surface = cairo_image_surface_create (CAIRO_FORMAT_A8, 100, 100);
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
    MT_X_BEARING(i) = te.x_bearing;
    MT_Y_BEARING(i) = te.y_bearing;
    MT_WIDTH(i) = te.width;
    MT_HEIGHT(i) = te.height;
    MT_X_ADVANCE(i) = te.x_advance;
    MT_Y_ADVANCE(i) = te.y_advance;
  }

  cairo_status_t status = cairo_status(cr);
  if (status != CAIRO_STATUS_SUCCESS)
    Rprintf("measure_text error: %s\n", cairo_status_to_string(status));

  if (fontfile)
    FT_Done_FreeType(ft);

  cairo_destroy(cr);
  cairo_surface_destroy(surface);
  return measures + MT_NCOLS*n;
}

SEXP measure_textR(SEXP texts, SEXP family, SEXP font,
                   SEXP fontfile, SEXP size) {

  /* texts must be UTF8 */
  SEXP result;
  int n = Rf_length(texts), m = MT_NCOLS, done = 0;
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


int get_buffer_stride(int width) {
  return cairo_format_stride_for_width(CAIRO_FORMAT_A8, width);
}

/* draws alpha values to existing buffer of unsigned char. */

void draw_text_to_buffer(int n, double *x, double *y, const char *text[],
                         const char *family, int font,
                         const char *fontfile, double size,
                         int width, int height, int stride,
                         unsigned char *buffer) {

  /* texts must be UTF8 */
  cairo_surface_t *surface;
  cairo_t *cr;
  surface = cairo_image_surface_create_for_data(buffer, CAIRO_FORMAT_A8,
                                        width, height, stride);
  cr = cairo_create (surface);

  FT_Library ft;
  int useFreetype = fontfile && !FT_Init_FreeType(&ft);

  if (!useFreetype)
    select_font_face(cr, family, font - 1, size);
  else
    select_font_file(cr, ft, fontfile, size);
  for (int i = 0; i < n; i++) {
      cairo_move_to(cr, x[i], y[i]);
      cairo_show_text(cr, text[i]);
    }
    cairo_surface_flush(surface);
    cairo_status_t status = cairo_status(cr);
    if (status != CAIRO_STATUS_SUCCESS)
      Rprintf("draw_text error: %s\n", cairo_status_to_string(status));

    if (fontfile)
      FT_Done_FreeType(ft);

    cairo_destroy(cr);
    cairo_surface_destroy(surface);
}

SEXP draw_text_to_rasterR(SEXP x, SEXP y, SEXP texts,
                         SEXP family, SEXP font,
                         SEXP fontfile, SEXP size,
                         SEXP width, SEXP height) {

  /* texts must be UTF8 */
  int n = Rf_length(texts), m;

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
      stride = get_buffer_stride(w);

  unsigned char *buffer = calloc(stride*h, sizeof(unsigned char));

  double *x0 = REAL(x), *y0 = REAL(y);
  int done = 0;
  for (int i = 1; i <= n; i++) {
    if (i == n ||
        size0[i] != size0[i - 1] ||
        family0[i] != family0[i - 1] ||
        font0[i] != font0[i - 1] ||
        fontfile0[i] != fontfile0[i - 1]) {
      draw_text_to_buffer(i - done, x0 + done, y0 + done, text0 + done,
                          family0[done], font0[done],
                          fontfile0[done], size0[done],
                          w, h, stride,
                          buffer);
        done = i;
    }
  }

  SEXP result;
  PROTECT(result = Rf_allocVector(INTSXP, w*h));
  int *res = INTEGER(result);
  int k = 0;
  for (int i = 0; i < h; i++) {
    unsigned char *row = buffer + i*stride;
    for (int j = 0; j < w; j++)
      res[k++] = row[j];
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
  CALLDEF(draw_text_to_rasterR, 9),
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
