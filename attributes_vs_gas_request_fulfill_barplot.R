############################################################
# Attributes vs Gas Consumption (Request vs Fulfillment)
# Adapted to the provided style template
# Y axis in thousands (k) - NO BAR OUTLINES
# EXTRA SPACE BELOW (between bars and panel border)
############################################################

# ============================
# 1. CLEAR ENVIRONMENT
# ============================

rm(list = ls())
graphics.off()

# ============================
# 2. DATA VECTORS
# ============================

attributes <- c(1, 2, 5, 10)

gas_request <- c(135183, 135205, 135205, 135256)
gas_fulfillment <- c(68748, 92503, 97301, 108003)

# ============================
# 3. PREPARE DATA FOR PLOT
# ============================

library(ggplot2)

df <- data.frame(
  Attributes = factor(rep(attributes, times = 2), levels = attributes),
  GasUsedK   = c(gas_request, gas_fulfillment) / 1000,  # k
  Phase      = factor(
    rep(c("Request", "Fulfillment"), each = length(attributes)),
    levels = c("Request", "Fulfillment")
  )
)

# ============================
# 4. CREATE PLOT (Y AXIS IN k)
# ============================

p <- ggplot(df, aes(x = Attributes, y = GasUsedK, fill = Phase)) +
  geom_col(
    position = position_dodge(width = 0.55),
    width = 0.45,
    color = NA
  ) +
  scale_fill_manual(
    values = c(
      "Request" = "#2b88d8",
      "Fulfillment" = "#d73027"
    )
  ) +
  scale_y_continuous(
    # bottom space is the FIRST value (mult = c(bottom, top))
    expand = expansion(mult = c(0.06, 0.06))
  ) +
  labs(
    x = "Number of Slice Attributes",
    y = "Gas consumption per transaction x1000"
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
    axis.text.y = element_text(size = 14, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(2.5, "mm"),
    panel.border = element_rect(
      color = "black",
      linewidth = 1.1
    ),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, color = "black"),
    legend.position = "top"
  )

print(p)

# ============================
# 5. SAVE PLOT AS PDF
# ============================

ggsave(
  filename = "attributes_vs_gas_request_fulfillment.pdf",
  plot = p,
  device = cairo_pdf,
  width = 8,
  height = 5,
  dpi = 300
)

############################################################
# End of file
############################################################
