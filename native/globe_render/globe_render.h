typedef struct {
  unsigned char r, g, b;
} RGBPixel;

void globe_render(
  int win_w,
  int win_h,
  int rotation_lon_deg,
  int rotation_lat_deg,
  int world_width,
  int world_height,
  const int *altitudes, // world_width * world_height
  int max_land_height,
  RGBPixel *output_buffer // pre-allocated with win_w * win_h
);
