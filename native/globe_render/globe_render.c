#include <math.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct {
  uint8_t r, g, b;
} RGBPixel;

static float clamp(float x, float min_val, float max_val) {
  if (x < min_val) return min_val;
  if (x > max_val) return max_val;
  return x;
}

// Simple linear gradient: blue (0.0) to green (1.0)
static void interpolate_gradient(float t, uint8_t *r, uint8_t *g, uint8_t *b) {
  *r = (uint8_t)(0 * (1 - t) + 0 * t);
  *g = (uint8_t)(0 * (1 - t) + 255 * t);
  *b = (uint8_t)(255 * (1 - t) + 0 * t);
}

#include <omp.h>  // Add this include at the top

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
) {
  const float pi = M_PI;
  float rot_lon = rotation_lon_deg * pi / 180.0f;
  float rot_lat = rotation_lat_deg * pi / 180.0f;
  int center_x = win_w / 2;
  int center_y = win_h / 2;
  int radius = (int)(win_h / 2.2f);

  float cos_lat = cosf(rot_lat);
  float sin_lat = sinf(rot_lat);
  float cos_lon = cosf(rot_lon);
  float sin_lon = sinf(rot_lon);

  // Parallelize the outer loop over dy
  #pragma omp parallel for
  for (int dy = -radius; dy <= radius; dy++) {
    for (int dx = -radius; dx <= radius; dx++) {
      float fx = (float)dx / radius;
      float fy = -(float)dy / radius;
      float r2 = fx * fx + fy * fy;
      if (r2 > 1.0f) continue;

      float fz = sqrtf(1.0f - r2);
      float x0 = fx, y0 = fy, z0 = fz;

      float x2 = x0 * cos_lon + y0 * sin_lat * sin_lon + z0 * cos_lat * sin_lon;
      float y2 = y0 * cos_lat - z0 * sin_lat;
      float z2 = -x0 * sin_lon + y0 * sin_lat * cos_lon + z0 * cos_lat * cos_lon;

      float lon = atan2f(x2, z2);
      float lat = asinf(-y2);

      float x_frac = lon / (2.0f * pi) + 0.5f;
      float y_frac = lat / pi + 0.5f;

      int wx = ((int)(x_frac * world_width)) % world_width;
      int wy = ((int)(y_frac * world_height)) % world_height;
      if (wx < 0) wx += world_width;
      if (wy < 0) wy += world_height;

      int index = wy * world_width + wx;
      int altitude = altitudes[index];

      float norm_alt = clamp((float)altitude / (float)max_land_height, 0.0f, 1.0f);
      uint8_t r, g, b;
      interpolate_gradient(norm_alt, &r, &g, &b);

      int screen_x = center_x + dx;
      int screen_y = center_y + dy;
      if (screen_x >= 0 && screen_x < win_w && screen_y >= 0 && screen_y < win_h) {
        output_buffer[screen_y * win_w + screen_x] = (RGBPixel){r, g, b};
      }
    }
  }
}
