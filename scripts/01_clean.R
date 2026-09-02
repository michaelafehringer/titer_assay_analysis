
# 1. Read the raw data
# Make sure your file is in your working directory or provide the full path
relative_path <- "vogl_lab/02_microbial_library/01_Ongoing/02_production/Experiments/all_titer_assays_data/" 
df <- read_csv(onedrive_path, relative_path, "titer_results.csv")

results <- df %>%
  # 1. Group by Dilution to evaluate if the specific replicate set passes
  group_by(Date, Sample, Dilution) %>%
  mutate(valid_dilution = all(Plaques >= 7 & Plaques <= 100)) %>%
  
  # 2. Group by Sample to see if the sample has ANY valid dilutions at all
  group_by(Date, Sample) %>%
  mutate(sample_has_valid = any(valid_dilution)) %>%
  
  # 3. Filter: Keep if the dilution is valid OR if the sample has no valid options
  filter(valid_dilution | !sample_has_valid) %>%
  
  # 4. Add a Caveat column to flag the forced inclusions
  mutate(Caveat = ifelse(!sample_has_valid, "Warning: Suboptimal counts used", "None")) %>%
  
  # 5. Calculate individual replicate titers
  mutate(pfu_ml = Plaques * (1000 / Vol_Plated_ul) * (10^abs(Dilution))) %>%
  
  # 6. Summarize the final sample metrics (including the Caveat column so it's retained)
  group_by(Date, Sample, Caveat) %>%
  summarise(
    mean_pfu_ml = mean(pfu_ml, na.rm = TRUE),
    sd_pfu_ml   = sd(pfu_ml, na.rm = TRUE),
    n_reps      = n(),
    .groups     = "drop"
  ) %>%
  
  # 7. Calculate the downstream yields
  mutate(
    pfu_ul             = mean_pfu_ml / 1000,
    total_phages_320ul = pfu_ul * 320,
    pfu_per_ug_arms    = total_phages_320ul / 0.5,
    sd_pfu_per_ug      = (sd_pfu_ml / 1000) * 320 / 0.5
  )

# Print the final calculated table to the console
print(results)
