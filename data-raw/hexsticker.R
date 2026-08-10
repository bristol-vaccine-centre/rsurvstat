# 1. Install and load the required packages
if (!requireNamespace("hexSticker", quietly = TRUE)) install.packages("hexSticker")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(ggplot2)
library(hexSticker)

# 2. Define official-style RKI corporate colors
rki_dark_blue = "#003F7A"
rki_light_blue = "#5B92E5"

# 3. Replicate the RKI stylized wave logo using ggplot2
# Creating data for the intersecting waves and bounding vertical lines
x_wave = seq(-0.6, 0.6, length.out = 100)
wave_data = data.frame(
  x  = c(x_wave, x_wave),
  y  = c(0.18 * sin(2 * pi * x_wave + 0.4), -0.18 * sin(2 * pi * x_wave - 0.4)),
  id = c(rep("wave1", 100), rep("wave2", 100))
)

logo_plot = ggplot() +
  # Draw the background waves
  geom_line(
    data = subset(wave_data, id == "wave1"),
    aes(x = x, y = y),
    color = rki_light_blue,
    linewidth = 1.8,
    alpha = 0.8
  ) +
  geom_line(
    data = subset(wave_data, id == "wave2"),
    aes(x = x, y = y),
    color = rki_dark_blue,
    linewidth = 2.2
  ) +
  # Draw the two iconic vertical framing lines from the RKI logo
  geom_segment(
    aes(x = -0.28, xend = -0.28, y = -0.22, yend = 0.35),
    color = rki_dark_blue,
    linewidth = 2.2
  ) +
  geom_segment(
    aes(x = 0.28, xend = 0.28, y = -0.35, yend = 0.22),
    color = rki_dark_blue,
    linewidth = 2.2
  ) +
  # Clean up coordinates and formatting
  coord_fixed(ratio = 1) +
  xlim(-1, 1) +
  ylim(-1, 1) +
  theme_void()

# 4. Generate the final R package hex sticker
sticker(
  subplot = logo_plot,
  package = "rsurvstat", # Your package name here
  p_size = 20, # Text font size
  p_color = rki_dark_blue, # Text color
  p_y = 0.5, # Position text perfectly below the logo
  s_x = 1.0,
  s_y = 1.15, # Logo placement coordinates
  s_width = 2,
  s_height = 2, # Logo scaling sizes
  h_fill = "#FFFFFF", # Clean white inner sticker background
  h_color = rki_dark_blue, # Deep blue border
  h_size = 1.8, # Thickness of the sticker border
  filename = "rsurvstat_hex.png" # Output file name
)
