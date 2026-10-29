############################################################
# Attributes vs Gas Consumption (Fulfillment Transaction)
# Bar chart - Fulfillment only (red)
############################################################

# 1. Clear environment
rm(list = ls())
graphics.off()

# 2. Input data
attributes <- c(1, 2, 5, 10)
gas_used   <- c(68748, 92503, 97301, 108003)

df <- data.frame(
  Attributes = factor(attributes),
  GasUsed = gas_used
)

# 3. Descriptive statistics
mean_gas <- mean(df$GasUsed)
sd_gas   <- sd(df$GasUsed)
min_gas  <- min(df$GasUsed)
max_gas  <- max(df$GasUsed)

cat("Descriptive statistics (Fulfillment transaction):\n")
cat("Mean gas:", mean_gas, "\n")
cat("Standard deviation:", sd_gas, "\n")
cat("Min gas:", min_gas, "\n")
cat("Max gas:", max_gas, "\n")

# 4. Load library
library(ggplot2)

# 5. Create bar plot (red)
plot_gas <- ggplot(df, aes(x = Attributes, y = GasUsed, fill = "Fulfillment")) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(
    values = c("Fulfillment" = "#d73027")  # vermelho
  ) +
  guides(fill = "none") +
  labs(
    title = "Gas Consumption of the Fulfillment Transaction",
    subtitle = "Impact of the Number of Slice Attributes Returned by the Oracle",
    x = "Number of Slice Attributes",
    y = "Gas Used (Fulfillment)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title    = element_text(size = 11),
    axis.text     = element_text(size = 10)
  )

# 6. Display plot
print(plot_gas)

# 7. Save plot to PDF
output_path <- "attributes_vs_gas_fulfill_barplot.pdf"

ggsave(
  filename = output_path,
  plot = plot_gas,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

cat("Plot saved to:", output_path, "\n")
############################################################
# End of file
############################################################
