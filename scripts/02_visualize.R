# 3. Visualize the data
# Create a bar chart grouping samples, using Date for different colors
ggplot(results, aes(x = Sample, y = pfu_per_ug_arms, fill = as.character(Date))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  
  # Add the sample standard deviation error bars
  geom_errorbar(
    aes(ymin = pfu_per_ug_arms - sd_pfu_per_ug, 
        ymax = pfu_per_ug_arms + sd_pfu_per_ug),
    position = position_dodge(width = 0.8), 
    width = 0.25
  ) +
  
  # Adjust labels and styling
  labs(
    title = "Phage Packaging Efficiency",
    x = "Packaging Sample",
    y = "Yield (pfu / µg arms)",
    fill = "Experiment Date"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank() # Cleans up vertical grid lines
  )