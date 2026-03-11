/*********************************************
 textshaping/systemfont-specific functions are in this file
 **********************************************/

#include "atlas.h"
#include "R.h"                 // for debugging
#include "rasterText.h"
#include <textshaping.h>
#include <systemfonts-ft.h>
#include <numeric>
#include <stdio.h>
#include <functional>

static const bool ft_compat = systemfonts::ver2::check_ft_version();

using namespace textRaster;

void Glyph_atlas::initContext() {
  if (!ft_compat)
    Rprintf("Freetype version mismatch!\n");
  if (context)
    clearContext();
}

void Glyph_atlas::clearContext() {
  context = nullptr;
}

void decodeDescription(const std::string description, Font_record& fr);

std::string encodeDescription(const char* file,
                              int index,
                              double size);

void decodeDescription(const std::string description, Font_record& fr) {
  int fileStart = 0;
  int count = sscanf(description.c_str(),
             "s=%lf i=%d %n",
             &(fr.size),
             &(fr.index),
             &fileStart);
  if (count < 2)
    Rf_error("Can't decode description.");

  fr.file = description.c_str() + fileStart;
}

std::string encodeDescription(const char* file,
                              int index,
                              double size) {
  std::string f = file;
  std::vector<char> descbuf(20+f.size());
  snprintf(descbuf.data(), descbuf.size()-1,
           "s=%f i=%d %s",
           size,
           index,
           f.c_str());
  std::string result = descbuf.data();
  return result;
}

void Font_record::setFont(void* new_font, const char* new_description) {
  if (new_font) {
    Rf_error("new_font not supported");
  } else if (new_description) {
    description = new_description;
    decodeDescription(description.c_str(), *this);
  }
  hash = std::hash<std::string>{}(description);
}

std::string Glyph_atlas::getFont(const char *family, int font,
                           const char *fontfile, double size) {
  if (fontfile)
    Rf_error("font files not supported yet.");
  double italic = (font == 3 || font == 4) ? 1.0 : 0.0,
      weight = (font == 2 || font == 4) ? 700.0 : 400.0,
      width = 5.0;

  FontSettings2 fs = systemfonts::ver2::locate_font(family, italic, weight, width, nullptr, nullptr, 0);
  std::string result = encodeDescription(fs.file, fs.index, size);
  return result;
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
  // PangoRectangle ink_rect;
  // pango_font_get_glyph_extents(getPangoFont(atlas->fonts[fontnum]), glyph, &ink_rect, NULL);
  // width = PANGO_PIXELS_CEIL(ink_rect.width);
  // height = PANGO_PIXELS_CEIL(ink_rect.height);
  //
  // x = ink_rect.x/PANGO_SCALE;
  // y = ink_rect.y/PANGO_SCALE;

  // Draw to x_atlas + Harfbuzz x_offset + x;
  Font_record& font = atlas->fonts[fontnum];
  int success;
  FT_Face face = get_cached_face(font.file.c_str(), font.index, font.size, 96.0, &success);
  if (success != 0) {
    Rf_error("Unable to load font file %s\n", font.file.c_str());
  }

  FT_Load_Glyph(face, glyph, FT_LOAD_DEFAULT);
  FT_Glyph_Metrics metrics = face->glyph->metrics;
  width = std::ceil(metrics.width/64.0);
  height = std::ceil(metrics.height/64.0);
  x = metrics.horiBearingX/64.0 + width - metrics.width/64.0;
  y = -(metrics.horiBearingY/64.0 + height - metrics.height/64.0);
  FT_Done_Face(face);
};

size_t Glyph_atlas::find_font(std::string desc) {
  Font_record f(*this, nullptr, desc.c_str());
  for (size_t i=0; i < fonts.size(); i++)
    if (f.file == fonts[i].file &&
        f.index == fonts[i].index)
      return i;

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
  Font_record& font = fonts[fontnum];
  std::vector<textshaping::Point> loc;
  std::vector<uint32_t> id;
  std::vector<int> cluster;
  std::vector<unsigned int> fontlist;
  std::vector<FontSettings> fallbacks;
  std::vector<double> fallback_scaling;
  FontSettings fs(font.file.c_str(), font.index,
                  nullptr, 0);
  int success = string_shape(
      text,
      fs,
      font.size,
      96,
      loc,
      id,
      cluster,
      fontlist,
      fallbacks,
      fallback_scaling
  );
  for (size_t i = 0; i < id.size(); i++) {
    std::string fontdesc = encodeDescription(
      fallbacks[fontlist[i]].file,
      fallbacks[fontlist[i]].index,
      font.size);
    size_t fontnum = find_font(fontdesc);
    size_t glyphnum = find_glyph(id[i], fontnum, color);
    string.glyphnum.push_back(glyphnum);
    string.x_offset.push_back(loc[i].x);
    string.y_offset.push_back(loc[i].y);
  }
  strings.push_back(string);
  return strings.size()-1;
}

#include <Rinternals.h> // for SEXP etc. below

void Glyph_atlas::draw_glyph_to_buffer(Glyph_record& g, int x, int y) {
  Font_record& font = fonts[g.fontnum];
  SEXP raster =
    get_glyph_raster(g.glyph, font.file.c_str(), font.index,
                     font.size, 96.0,  g.color),
    dim = Rf_getAttrib(raster, R_DimSymbol);
  int h = INTEGER_ELT(dim, 0),
      w = INTEGER_ELT(dim, 1);
  for (int i=0; i < h; i++) {
    for (int j=0; j < w; j++) {
      buffer[(y+i)*width + x+j] = INTEGER_ELT(raster, i*w + j);
    }
  }
 }
