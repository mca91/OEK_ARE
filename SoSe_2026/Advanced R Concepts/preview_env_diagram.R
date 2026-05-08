library(ggplot2)
library(here)

make_circle <- function(r, n = 500) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(x = r * cos(theta), y = r * sin(theta))
}

r_outer <- c(5.0, 4.0, 3.0, 2.0, 1.0)
r_inner <- c(4.0, 3.0, 2.0, 1.0, 0.0)
r_mid   <- (r_outer + r_inner) / 2   # 4.5, 3.5, 2.5, 1.5, 0.5

fills  <- c("#f0f0f0", "#d4e4f7", "#7aaed4", "#004c93", "#1e6eb5")
labels <- c("R_EmptyEnv", "base", "pkgs / search path", ".GlobalEnv", "f()'s exec. env")
tcols  <- c("#444444", "#004c93", "#002d6e", "#ffffff", "#ffffff")

p <- ggplot() +
  theme_void() +
  coord_equal(xlim = c(-5.5, 5.5), ylim = c(-6.5, 5.8), clip = "off")

for (i in seq_along(r_outer)) {
  p <- p + geom_polygon(data = make_circle(r_outer[i]), aes(x = x, y = y),
                        fill = fills[i], color = "white", linewidth = 1.5)
}

for (i in seq_along(r_outer)) {
  p <- p + annotate("label",
                    x = 0, y = r_mid[i],
                    label = labels[i],
                    fill  = fills[i], color = tcols[i],
                    fontface = "bold", size = 4.2,
                    label.size = 0,
                    label.padding = unit(0.18, "lines"))
}

p <- p +
  annotate("segment", x = 0, xend = 0, y = -1.15, yend = -5.2,
           arrow = arrow(length = unit(0.28, "cm"), ends = "last", type = "closed"),
           color = "#555555", linewidth = 0.8) +
  annotate("text", x = 0.28, y = -3.15,
           label = "lookup\nwalks up",
           size = 3.8, color = "#555555", hjust = 0, lineheight = 0.9,
           fontface = "italic")

out <- here("SoSe_2026/Advanced R Concepts/env_diagram_preview.png")
ggsave(out, plot = p, width = 6, height = 6, dpi = 200, bg = "white")
message("Saved: ", out)
