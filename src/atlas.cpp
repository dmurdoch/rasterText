#include "atlas.h"
#include "R.h"
#include "rasterText.h"
#include <fontconfig/fontconfig.h>
#include <pango/pangocairo.h>  // Main PangoCairo integration
#include <pango/pango.h>       // Pango layout and font handling
#include <cairo.h>             // Base Cairo surface and context functions

using namespace textRaster;

PangoFont* getPangoFont(Font_record& font);

Glyph_record::Glyph_record(const Glyph_record& prev) :
  atlas(prev.atlas),
  glyph(prev.glyph),
  fontnum(prev.fontnum),
  color(prev.color),
  x_atlas(prev.x_atlas),
  y_atlas(prev.y_atlas),
  width(prev.width),
  height(prev.height),
  x(prev.x),
  y(prev.y){
  setUV();
};

void Glyph_record::setUV() {
  u[0] = u[3] = x_atlas;
  u[1] = u[2] = x_atlas + width;
  v[0] = v[1] = y_atlas;
  v[2] = v[3] = y_atlas + height;
}

void Glyph_record::Rprint(bool verbose) {
  Rprintf("%5zu %5x %5d %5d %5d %5d %5d %5d %8x\n",
          fontnum, glyph, x_atlas, y_atlas,
          width, height,
          x, y, color);
}

String_record::String_record(Glyph_atlas& in_atlas,
              const char* in_text,
              size_t in_fontnum,
              int in_color) :
  atlas(&in_atlas),
  text(in_text),
  fontnum(in_fontnum),
  color(in_color) {}

void String_record::Rprint(bool verbose) {
  Rprintf("\"%s\"\n  Font %zu\n  Glyph     xofs     yofs\n", text.c_str(), fontnum);
  if (verbose)
    for (int i=0; i < glyphnum.size(); i++)
      Rprintf("  %5zu %8.2f %8.2f\n",
            glyphnum[i], x_offset[i], y_offset[i]);
}

Glyph_atlas::Glyph_atlas(int in_width, int in_height, bool in_mono) :
  width(in_width),
  height(in_height),
  mono(in_mono),
  buffer_generation(0),
  has_new_glyphs(false),
  buffer(width*height*(mono ? 1 : 4), 0),
  last_x(0),
  last_y(0),
  row_height(0),
  context(nullptr)
{
}

Glyph_atlas::~Glyph_atlas() {
  clearContext();
}

size_t Glyph_atlas::find_glyph(uint32_t glyph, size_t fontnum, int color) {
  for (size_t i=0; i < glyphs.size(); i++)
    if (glyphs[i].glyph == glyph &&
        glyphs[i].fontnum  == fontnum &&
        (mono || glyphs[i].color == color))
      return i;
    Glyph_record g(*this, glyph, fontnum, color);
    return add_glyph(g);
}

size_t Glyph_atlas::add_glyph(Glyph_record& g) {
  /* FIXME:  if the new row height is bigger than
   * the old one, it may be more efficient to go to
   * a new line (wasting space at the end of the current
   * one) instead of increasing the row height (wasting
   * space above all the other glyphs)
   */
  if (!has_new_glyphs) {
    prev_last_x = last_x;
    prev_last_y = last_y;
  }
  if (g.width + 2 > width ||
      (last_x + g.width + 2 > width &&
      last_y + row_height + g.height + 2 > height) ||
      last_y + g.height + 2 > height)
    expand_atlas(g);

  if (last_x + g.width + 2 > width) {
    last_x = 0;
    last_y += row_height + 2;
    row_height = g.height;
  } else
    row_height = std::max(row_height, g.height);

  draw_glyph_to_buffer(g, last_x + 1, last_y + 1);
  g.x_atlas = last_x + 1;
  g.y_atlas = last_y + 1;
  last_x += g.width + 2;
  has_new_glyphs = true;
  glyphs.push_back(g);
  return glyphs.size()-1;
}

void Glyph_atlas::expand_atlas(Glyph_record& g) {
  clearContext();
  int new_width = width, new_height = height;
  while (g.width + 2 > new_width ||
         g.height + 2 > new_height) {
    new_width *= 2;
    new_height *= 2;
  }
  if ((last_x + g.width + 2 > new_width &&
      last_y + row_height + g.height + 2 > new_height) ||
      last_y + g.height + 2 > new_height) {
    new_width *= 2;
    new_height *= 2;
  }
  if (new_width > width) {
    std::vector<unsigned char> old_buffer = buffer;
    int old_width = width, old_height = height;
    buffer.assign(new_width*new_height*(mono ? 1 : 4), 0);
    width = new_width;
    height = new_height;
    last_x = last_y = row_height = 0;
    buffer_generation++;
    copy_glyphs_to_buffer(old_width, old_height, old_buffer);
  }
  width = new_width;
  height = new_height;
}

void Glyph_atlas::copy_glyphs_to_buffer(int old_width,
                                        int old_height,
                                        std::vector<unsigned char> old_buffer) {
  int pixelsize = mono ? 1 : 4;
  for (int i=0; i < glyphs.size(); i++) {
    Glyph_record& g = glyphs[i];
    if (g.width + 2 > width ||
        (last_x + g.width + 2 > width &&
        last_y + g.height + 2 > height))
      Rf_error("Cannot copy glyphs:  not enough space!");

    if (last_x + g.width + 2 > width) {
      last_x = 0;
      last_y += row_height + 2;
      row_height = g.height;
    } else
      row_height = std::max(row_height, g.height);

    for (int j=0; j<g.height; j++) {
      int old_start = (old_width*(g.y_atlas + j) + g.x_atlas)*pixelsize,
        new_start = (width*(last_y + 1 + j) + last_x + 1)*pixelsize;
      std::copy(old_buffer.begin() + old_start,
                old_buffer.begin() + old_start + g.width*pixelsize,
                buffer.begin() + new_start);
    }
    g.x_atlas = last_x + 1;
    g.y_atlas = last_y + 1;
    g.setUV();
    last_x += g.width + 2;
  }
  has_new_glyphs = true;
}

void test_atlas(Glyph_atlas& atlas, const char* text, int rfont, int size, int color = 0xFF);

extern "C" {
  SEXP test_atlasR(SEXP text);

  SEXP test_atlasR(SEXP text) {
    Glyph_atlas atlas(8, 8, false);
    for (int i=0; i < Rf_length(text); i++)
      test_atlas(atlas, CHAR(STRING_ELT(text, i)), i%4 + 1, 12+i, 0xFF0000FF);
    atlas.Rprint(false);
    SEXP result;
    int w = atlas.width, h = atlas.height,
      pixelsize = atlas.mono ? 1 : 4,
      stride = w*pixelsize;
    if (atlas.mono)
      PROTECT(result = Rf_allocMatrix(INTSXP, w, h));
    else
      PROTECT(result = Rf_alloc3DArray(INTSXP, 4, h, w));
    int *res = INTEGER(result);
    int k = 0;
    for (int i = 0; i < h; i++) {
      if (atlas.mono) {
        unsigned char *row = atlas.buffer.data() + i*stride;
        for (int j = 0; j < w; j++)
          res[k++] = row[j];
      } else {
        uint32_t *row32 = (uint32_t*)(atlas.buffer.data() + i*stride);
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
    UNPROTECT(1);
    return result;
  }
}

void test_atlas(Glyph_atlas& atlas, const char* text, int rfont, int size, int color) {
    Rprintf("testing with text=%s\n", text);
    void *font = atlas.getFont("serif", rfont, nullptr, size);
    size_t fontnum = atlas.find_font(font);
    atlas.find_string(text, fontnum, color);
  }


/*********************************************
 Pango-specific functions below here
 **********************************************/

struct Pango_Cairo_Context {
  PangoContext* pcontext;
  cairo_surface_t* csurface;
  cairo_t* ccontext;
};

Pango_Cairo_Context* getContexts(Glyph_atlas& atlas);

void clearContexts(Glyph_atlas& atlas);

PangoContext* getPangoContext(Glyph_atlas& atlas);

cairo_surface_t* getCairoSurface(Glyph_atlas& atlas);

cairo_t* getCairoContext(Glyph_atlas& atlas);

PangoFontDescription* getPangoFontDesc(Font_record& font);

PangoFont* getPangoFont(Font_record& font);

void Glyph_atlas::initContext() {
  if (context)
    clearContext();
  Pango_Cairo_Context* contexts = new Pango_Cairo_Context{nullptr, nullptr, nullptr};
  context = static_cast<void*>(contexts);
}

void Glyph_atlas::clearContext() {
  clearContexts(*this);
  delete getContexts(*this);
  context = nullptr;
}

Pango_Cairo_Context* getContexts(Glyph_atlas& atlas) {
  if (!atlas.context) {
    atlas.initContext();
  }
  return static_cast<Pango_Cairo_Context*>(atlas.context);
}

void clearContexts(Glyph_atlas& atlas) {
  for (int i=0; i < atlas.fonts.size(); i++) {
    if (atlas.fonts[i].font) {
      g_object_unref(atlas.fonts[i].font);
      atlas.fonts[i].font = nullptr;
    }
  }
  auto contexts = getContexts(atlas);
  if (contexts->pcontext) {
    g_object_unref(contexts->pcontext);
    contexts->pcontext = nullptr;
  }
  if (contexts->ccontext) {
    cairo_destroy(contexts->ccontext);
    contexts->ccontext = nullptr;
  }
  if (contexts->csurface) {
    cairo_surface_destroy(contexts->csurface);
    contexts->csurface = nullptr;
  }
}

Font_record::Font_record(Glyph_atlas& in_atlas, void* in_font) :
  atlas(&in_atlas),
  font(nullptr)
{
  setFont(in_font);
}


void Font_record::setFont(void* new_font) {
  if (!font && new_font) {
    PangoFont* pfont = static_cast<PangoFont*>(new_font);
    PangoFontDescription* desc = pango_font_describe(pfont);
    hash = pango_font_description_hash(desc);
    char * descstring = pango_font_description_to_string(desc);
    description = descstring;
    g_free(static_cast<gpointer>(descstring));
  }
  font = new_font;
}

void Font_record::Rprint(bool verbose) {
  Rprintf("%8x %s\n", hash, description.c_str());
}

void* Glyph_atlas::getFont(const char *family, int font,
                           const char *fontfile, double size) {
  PangoFontDescription* desc = getFontDesc(
      family, font, fontfile, size);

  PangoContext* context = getPangoContext(*this);
  PangoFont* pfont = pango_context_load_font(context, desc);
  return static_cast<void*>(pfont);

}

cairo_surface_t* getCairoSurface(Glyph_atlas& atlas) {
  auto contexts = getContexts(atlas);
  if (!contexts->csurface) {
    int stride = atlas.width*(atlas.mono ? 1 : 4);
    contexts->csurface = cairo_image_surface_create_for_data(
      atlas.buffer.data(),
      atlas.mono ? CAIRO_FORMAT_A8 : CAIRO_FORMAT_ARGB32,
      atlas.width,
      atlas.height,
      stride
    );
  }
  return contexts->csurface;
}

cairo_t* getCairoContext(Glyph_atlas& atlas) {
  auto contexts = getContexts(atlas);
  if (!contexts->ccontext) {
    auto surface = getCairoSurface(atlas);
    contexts->ccontext = cairo_create(surface);
  }
  return contexts->ccontext;
}

PangoContext* getPangoContext(Glyph_atlas& atlas) {
  auto contexts = getContexts(atlas);
  if (!contexts->pcontext) {
    auto ccontext = getCairoContext(atlas);

    contexts->pcontext = pango_cairo_create_context(ccontext);
  }
  return contexts->pcontext;
}

PangoFont* getPangoFont(Font_record& font) {

  if (font.font) return static_cast<PangoFont*>(font.font);

  PangoContext* context = getPangoContext(*(font.atlas));

  PangoFontDescription* desc = getPangoFontDesc(font);

  PangoFont* result = pango_context_load_font(context, desc);

  pango_font_description_free(desc);

  font.font = (void *)result;

  return result;
}

PangoFontDescription* getPangoFontDesc(Font_record& font) {
  return pango_font_description_from_string(font.description.c_str());
}

Font_record::~Font_record() {
}

Glyph_record::Glyph_record(Glyph_atlas& in_atlas,
                           uint32_t in_glyph, size_t in_font, int in_color) :
  atlas(&in_atlas),
  glyph(in_glyph),
  fontnum(in_font),
  color(in_color),
  x_atlas(0),
  y_atlas(0){
  PangoRectangle ink_rect;
  pango_font_get_glyph_extents(getPangoFont(atlas->fonts[fontnum]), glyph, &ink_rect, NULL);
  width = PANGO_PIXELS_CEIL(ink_rect.width);
  height = PANGO_PIXELS_CEIL(ink_rect.height);

  x = PANGO_PIXELS(ink_rect.x);
  y = PANGO_PIXELS(ink_rect.y);

  // Draw to x_atlas + Harfbuzz x_offset + x;
};

void Glyph_atlas::clear_font_cache() {
  for (int i=0; i < fonts.size(); i++)
    fonts[i].font = nullptr;
}

size_t Glyph_atlas::find_font(void* font) {
  for (size_t i=0; i < fonts.size(); i++)
    if (font == fonts[i].font)
      return i;

  PangoFont* pfont = static_cast<PangoFont*>(font);
  PangoFontDescription* desc = pango_font_describe(pfont);
  unsigned int hash = pango_font_description_hash(desc);
  char * descstring;
  for (size_t i = 0; i < fonts.size(); i++)
    if (hash == fonts[i].hash) {
      descstring = pango_font_description_to_string(desc);
      if (fonts[i].description == descstring) {
        pango_font_description_free(desc);
        g_free(static_cast<gpointer>(descstring));
        fonts[i].font = font;
        return i;
      }
      g_free(static_cast<gpointer>(descstring));
    }
  pango_font_description_free(desc);
  Font_record f(*this, font);
  fonts.push_back(f);
  return fonts.size()-1;
}

size_t Glyph_atlas::find_string(const char *text, size_t fontnum, int color) {
  for (size_t i=0; i < strings.size(); i++)
    if (strings[i].text == text &&
        strings[i].fontnum == fontnum &&
        (mono || strings[i].color == color))
      return i;
  return add_string(text, fontnum, color);
}

size_t Glyph_atlas::add_string(const char *text, size_t fontnum,
                             int color) {

  String_record string(*this, text, fontnum,
                       color);
  float x = 0.0, y = 0.0; /* current position */

  PangoContext *context = getPangoContext(*this);

  PangoFontDescription* desc = getPangoFontDesc(fonts[fontnum]);

  pango_context_set_font_description(context, desc);

  GList *items = pango_itemize(context, text, 0, strlen(text), NULL, NULL);

  for (GList *l = items; l != NULL; l = l->next) {
    PangoItem *item = (PangoItem *)l->data;
    PangoFont *pfont = item->analysis.font;
    int fontnum = find_font((void *) pfont);

    hb_font_t *hb_font = pango_font_get_hb_font(pfont);

    // Prepare HarfBuzz Buffer
    hb_buffer_t *hb_buffer = hb_buffer_create();
    hb_buffer_add_utf8(hb_buffer, text, -1, item->offset, item->length);
    hb_buffer_guess_segment_properties(hb_buffer);

    // Shape!
    hb_shape(hb_font, hb_buffer, NULL, 0);

    // Extract Glyph Data
    unsigned int glyph_count;
    hb_glyph_info_t *hb_info = hb_buffer_get_glyph_infos(hb_buffer, &glyph_count);
    hb_glyph_position_t *hb_pos = hb_buffer_get_glyph_positions(hb_buffer, &glyph_count);
    for (size_t i = 0; i < glyph_count; i++) {
      size_t glyphnum = find_glyph(hb_info[i].codepoint, fontnum, color);

      string.glyphnum.push_back(glyphnum);

      string.x_offset.push_back((x + hb_pos[i].x_offset)/PANGO_SCALE);
      string.y_offset.push_back((y + hb_pos[i].y_offset)/PANGO_SCALE);

      x += hb_pos[i].x_advance;
      y += hb_pos[i].y_advance;
    }

    // Clean up per item

    hb_buffer_destroy(hb_buffer);
    pango_item_free(item);
  }
  g_list_free(items);
  strings.push_back(string);
  return strings.size()-1;
}

void Glyph_atlas::draw_glyph_to_buffer(Glyph_record& g, int x, int y) {

  cairo_t* cr = getCairoContext(*this);

  PangoGlyphInfo pg;
  pg.glyph = g.glyph;
  pg.geometry.width = 0;
  pg.geometry.x_offset = 0;
  pg.geometry.y_offset = 0;

  PangoGlyphString* gs = pango_glyph_string_new();
  pango_glyph_string_set_size(gs, 1);
  gs->glyphs[0] = pg;

  double x_pos = -g.x;
  double y_pos = -g.y;

  if (mono)
    cairo_set_source_rgba(cr, 1.0, 1.0, 1.0, 1.0); // Set "Alpha" to 1.0
  else {
    double red = (g.color >> 24)/255.0,
           green = (g.color >> 16 & 0xFF)/255.0,
           blue = (g.color >> 8 & 0xFF)/255.0,
           alpha = (g.color & 0xFF)/255.0;
    cairo_set_source_rgba(cr, red, green, blue, alpha);
  }
  cairo_move_to(cr, x + x_pos, y + y_pos);
  PangoFont* font = getPangoFont(g.atlas->fonts[g.fontnum]);
  pango_cairo_show_glyph_string(cr, font, gs);

  pango_glyph_string_free(gs);
}

void Glyph_atlas::Rprint(bool verbose) {
  Rprintf("Atlas with %zu fonts %zu glyphs %zu strings, buffer %d x %d\n", fonts.size(), glyphs.size(), strings.size(), width, height);
  if (verbose) {
    Rprintf("Fonts:\n");
    for (size_t i=0; i<fonts.size(); i++) {
      Rprintf("%5zu ", i);
      fonts[i].Rprint();
    }
    Rprintf("Glyphs:\n  num  font glyph x_atl y_atl width    ht     x     y\n");
    for (size_t i=0; i<glyphs.size(); i++) {
      Rprintf("%5zu ", i);
      glyphs[i].Rprint();
    }
    Rprintf("Strings:\n");
    for (size_t i=0; i < strings.size(); i++) {
      Rprintf("%zu: ", i);
      strings[i].Rprint();
    }
  }
}
