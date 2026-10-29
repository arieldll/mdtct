############################################################
# T_closure – Multi-Domain Trust Closure Time
# Adapted to the provided style template (green bars)
############################################################

# ============================
# 1. DATA VECTORS (seconds)
# ============================

brazil <- rep(5, 30)

americas <- rep(5, 30)

one_per_continent <- c(
  15, 10, 10,
  rep(5, 27)
)

all_servers <- c(
  10,10,10,10,10,10,10,10,10,10,
  5,10,5,10,10,10,10,5,10,5,
  10,10,10,5,10,10,5,5,10,5
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
  group = c("Brazil", "Americas", "One per continent", "All servers"),
  rbind(
    stats(brazil),
    stats(americas),
    stats(one_per_continent),
    stats(all_servers)
  )
)

# ============================
# 4. PREPARE DATA FOR PLOT
# ============================

library(ggplot2)

df_stats$group <- factor(
  df_stats$group,
  levels = c("Brazil", "Americas", "One per continent", "All servers"),
  labels = c("Brazil", "Americas", "One per continent", "All servers")
)

# ============================
# 5. CREATE PLOT (Y AXIS IN SECONDS)
# ============================

p <- ggplot(df_stats, aes(x = group, y = mean)) +
  geom_col(
    width = 0.3,
    fill = "#2b88d8"
  ) +
  geom_errorbar(
    aes(
      ymin = mean - sd,
      ymax = mean + sd
    ),
    width = 0.16,
    linewidth = 0.85,
    color = "black"
  ) +
  labs(
    x = "Scenario",
    y = "Time (s)"
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
  filename = "t_closure_comparison_seconds.pdf",
  plot = p,
  device = cairo_pdf,
  width = 8,
  height = 5,
  dpi = 300
)

############################################################
# End of file
############################################################
