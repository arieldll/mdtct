# ============================
# 1. DATA VECTORS (milliseconds)
# ============================

todos10 <- c(
  19052,19050,19046,19048,19049,19047,19043,19055,19044,19047,
  19044,19048,19044,19045,14548,19043,19040,14541,19044,19043,
  19043,19042,19041,19042,19043,19041,19041,19043,19039,19040
)

six_continents <- c(
  15045,15042,13500,15041,15043,15040,15040,15040,15042,15044,
  15040,15040,14537,15039,14400,15042,15038,15038,15044,15040,
  15042,15040,15039,14538,15041,15043,15044,15040,15040,15043
)

americas <- c(
  10042,9537,10038,10041,10039,10041,10042,10041,10039,9540,
  10039,10038,10043,10038,10043,11000,10039,10040,10038,10040,
  10040,10039,10041,10045,10041,9540,10045,10041,10043,10041
)

brazil <- c(
  9043,9041,9041,9042,9045,9042,9044,9044,9044,9043,
  9040,9045,9045,9044,9046,9042,9043,9043,9039,9041,
  9044,9042,8540,9042,9042,9042,9043,8544,9040,9046
)

# ============================
# 2. STATISTICS FUNCTION
# ============================

stats <- function(v) {
  c(
    mean = mean(v),
    sd = sd(v),
    median = median(v),
    min = min(v),
    max = max(v),
    n = length(v)
  )
}

# ============================
# 3. COMPUTE STATISTICS
# ============================

df_stats <- data.frame(
  group = c("Brazil", "Americas", "6 Continents", "All 10"),
  rbind(
    stats(brazil),
    stats(americas),
    stats(six_continents),
    stats(todos10)
  )
)

# ============================
# 4. PREPARE DATA FOR PLOT
# ============================

library(ggplot2)

df_stats$group <- factor(
  df_stats$group,
  levels = c("Brazil", "Americas", "6 Continents", "All 10"),
  labels = c("Brazil", "Americas", "One per continent", "All servers")
)

# ============================
# 5. CREATE PLOT (Y AXIS IN SECONDS)
# ============================

p <- ggplot(df_stats, aes(x = group, y = mean / 1000)) +
  geom_col(
    width = 0.3,
    fill = "#2b88d8"
  ) +
  geom_errorbar(
    aes(
      ymin = (mean - sd) / 1000,
      ymax = (mean + sd) / 1000
    ),
    width = 0.16,
    linewidth = 0.85,
    color = "black"
  ) +
  labs(
    x = "Scenario",
    y = "End-to-end Oracle response time (s)"
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid.major = element_line(
      linetype = "dotted",
      linewidth = 0.35,
      color = "grey70"
    ),
    panel.grid.minor = element_line(
      linetype = "dotted",
      linewidth = 0.25,
      color = "grey85"
    ),
    axis.title = element_text(face = "plain"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(2.5, "mm"),
    panel.border = element_rect(
      color = "black",
      linewidth = 1.1
    ),
    legend.position = "none"
  )

print(p)

# ============================
# 6. SAVE PLOT AS PDF
# ============================

ggsave(
  filename = "latency_comparison_seconds.pdf",
  plot = p,
  device = cairo_pdf,
  width = 8,
  height = 5,
  dpi = 300
)
