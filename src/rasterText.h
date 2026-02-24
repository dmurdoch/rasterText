#ifndef TEXTSET_H
#define TEXTSET_H

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

int get_buffer_stride(int width, int mono);

void draw_text_to_buffer(int n,
                         const text_placement_t *xy,
                         const char *text[],
                                         const char *family, int font,
                                         const char *fontfile, double size,
                                         int mono,
                                         int *color,
                                         int width, int height, int stride,
                                         unsigned char *buffer);

#endif // RASTERTEXT_H
