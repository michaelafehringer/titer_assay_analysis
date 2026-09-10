# 3. Visualize the data grouped by Insert
plot_pfuperug <- ggplot(results, aes(x = as.factor(packaging_lot), y = pfu_per_ug_arms, fill = date)) + 
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") + 
  
  # Add the sample standard deviation error bars
  geom_errorbar(
    aes(ymin = pfu_per_ug_arms - sd_pfu_per_ug, 
        ymax = pfu_per_ug_arms + sd_pfu_per_ug),
    position = position_dodge(width = 0.8), 
    width = 0.25
  ) + 
  
  # Create a separate panel for each Insert
  # scales = "free" ensures panels only show the samples that belong to that insert
  facet_wrap(~ Insert, scales = "free") + 
  
  # Adjust labels and styling
  labs(
    title = "Phage Packaging Efficiency by Insert",
    x = "Packaging sample",
    y = "Yield (pfu / µg arms)",
    fill = "Experiment Date"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank(), # Cleans up vertical grid lines
    strip.text = element_text(face = "bold", size = 12) # Makes facet labels bold
  )

print(plot_pfuperug)

plotpfuperml <- ggplot(results, aes(x = as.factor(packaging_lot), y = mean_pfu_ml, fill = date)) + 
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") + 
  
  # Add the sample standard deviation error bars for pfu/ml
  geom_errorbar(
    aes(ymin = mean_pfu_ml - sd_pfu_ml, 
        ymax = mean_pfu_ml + sd_pfu_ml),
    position = position_dodge(width = 0.8), 
    width = 0.25
  ) + 
  
  # Create a separate panel for each Insert
  facet_wrap(~ Insert, scales = "free") + 
  
  # Adjust labels and styling
  labs(
    title = "Phage Packaging Titers by Insert",
    x = "Packaging sample",
    y = "Titer (pfu / ml)",
    fill = "Experiment Date"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank(), 
    strip.text = element_text(face = "bold", size = 12) 
  )

print(plotpfuperml)

# 2. Prepare the data for the table
# Selecting only the relevant columns to keep the table clean and readable
table_data <- results %>%
  select(Insert, packaging_lot, date, pfu_per_ug_arms, sd_pfu_per_ug) %>%
  # Optional: Round numeric values for cleaner display
  mutate(
    pfu_per_ug_arms = round(pfu_per_ug_arms, 2),
    sd_pfu_per_ug = round(sd_pfu_per_ug, 2)
  )

# # Convert the dataframe to a graphical table object
# table_grob <- tableGrob(table_data, rows = NULL) # rows = NULL hides row numbers
# 
# # 3. Arrange the plot and the table together
# # heights = c(3, 1) means the plot takes up 3/4 of the height, and the table 1/4
# grid.arrange(p, table_grob, nrow = 2, heights = c(3, 1))


#save the table
final_df <- results %>% 
  select(sample, Insert, packaging_lot, mean_pfu_ml, sd_pfu_ml, total_phages_320ul, pfu_per_ug_arms) %>% 
  rename(pfu_ml = mean_pfu_ml) %>% 
  mutate(pfu_per_ug_arms = formatC(pfu_per_ug_arms, format = "e", digits = 2))
