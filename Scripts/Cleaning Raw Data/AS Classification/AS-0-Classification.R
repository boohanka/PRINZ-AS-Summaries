

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_as_skills_step_1 <- read_xlsx("Data 2025/Raw/AS/Green AS Skills Step 1.xlsx")
df_as_skills_step_2 <- read_xlsx("Data 2025/Raw/AS/Green AS Skills Step 2.xlsx")

# ============================================================================#
# ============================================================================#
# ==== # ================ Inspect Classification - Run 1 =============== # ====
# ============================================================================#
# ============================================================================#

# Get green classifications

s1_green_classes <- df_as_skills_step_1 %>% pull(climate_related) %>% unique()
s1_green_classes

# Obtain summary:

s1_class_count <- df_as_skills_step_1 %>% group_by(climate_related) %>% 
  summarise(num_skills = n(), .groups = "drop")

# Inspect climate:

df_climate_skills <- df_as_skills_step_1 %>% filter(climate_related == "Climate-specific")

# Green non-climate:

df_green_skills <- df_as_skills_step_1 %>% filter(climate_related == "Green non-climate")

# ============================================================================#
# ============================================================================#
# ==== # ================ Inspect Classification - Run 2 =============== # ====
# ============================================================================#
# ============================================================================#

df_num_skills <- df_as_skills_step_2 %>% select(standard_reference, skill_id) %>% 
  distinct() %>% nrow()

df_skills <- df_as_skills_step_2 %>% 
  group_by(standard_reference, skill_id) %>% 
  slice_max(runs_agreeing, with_ties = TRUE) %>% 
  ungroup()

df_num_skills <- df_skills %>% select(standard_reference, skill_id) %>% 
  distinct() %>% nrow()


# Check error cases

df_as_skills_step_2 %>% pull(climate_related) %>% unique()

df_failed_raw <- df_as_skills_step_2 %>% filter(climate_related == "Error: Processing failed")
df_failed_max <- df_skills %>% filter(climate_related == "Error: Processing failed")

# Remove "Not-climate-nor-green-related", "Error: Processing failed"

cols_to_exclude <- c("Not-climate-nor-green-related", "Error: Processing failed")

df_skills %<>% 
  filter(!climate_related %in% cols_to_exclude)

# Take max

df_skills %<>%
  group_by(standard_reference, skill_id) %>% 
  slice_max(runs_agreeing, with_ties = FALSE) %>% 
  ungroup()

df_skills$climate_related %>% unique()

# Low-carbon classification?

low_carbon_classes <- c("Decarbonization-buildings", "Decarbonization-transport",
                        "Decarbonization-energy", "Decarbonization-industry",
                        "Climate-general", "Climate-CCS")

df_low_carbon_skills <- df_skills %>% 
  filter(climate_related %in% low_carbon_classes)


# ============================================================================#
# ============================================================================#
# ==== # ==================== Extract Classification =================== # ====
# ============================================================================#
# ============================================================================#

# Here, attempt to clean FP from Step 1 by removing Not-climate-nor-green-related
# skills.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#        Assign binary green          # 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# First, need to assign whether the skill is green or not. Exclude cases of Error

df_skills_2 <- df_as_skills_step_2 %>% 
  mutate(
    green = if_else(climate_related == "Not-climate-nor-green-related", 0, 1),
    green = if_else(climate_related == "Error: Processing failed", NA_real_, green)
  ) %>% 
  drop_na(green)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#        Strict Classification        #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# In strict classification, if any of the runs have non-green, the skill is non-green:

df_strict <- df_skills_2 %>% 
  group_by(standard_reference, skill_id) %>% 
  summarise(
    green_strict = if_else(any(green == 0), 0, 1),
    .groups = "drop"
  ) %>% 
  filter(green_strict == 1) %>% select(-green_strict)


df_strict %<>% 
  left_join(df_skills_2, by = c("standard_reference", "skill_id")) %>% 
  select(-green)

# Now take the slice_max()

df_strict %<>% 
  group_by(standard_reference, skill_id) %>% 
  slice_max(runs_agreeing, with_ties = TRUE) %>% 
  ungroup()

# Check the number of ties:

num_ties <- nrow(df_strict) - (df_strict %>% select(standard_reference, skill_id) %>% 
                                 distinct() %>% nrow())

# It's just 1

df_strict %<>% 
  group_by(standard_reference, skill_id) %>% 
  slice_max(runs_agreeing, with_ties = FALSE) %>% 
  ungroup()

# Finally, construct the distribution of skills classifications:

strict_dist <- df_strict %>% 
  group_by(climate_related) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  arrange(desc(n)) %>% 
  # Add low_carbon
  mutate(
    low_carbon = if_else(climate_related %in% low_carbon_classes, 1, 0)
  )


p_strict_dist <- strict_dist %>% 
  ggplot(aes(x = factor(climate_related, levels = climate_related), y = n, fill = factor(low_carbon))) +
  geom_bar(stat = "identity", col = "black") +
  scale_fill_manual(
    values = c("0" = "lightblue", "1" = "lightgreen"),
    labels = c("0" = "Non-Low Carbon", "1" = "Low Carbon")
  ) +
  theme_minimal() +
  labs(
    x = "Climate-Related Category",
    y = "Count of Skills",
    title = "Strict Distribution of Climate-Related Skills",
    fill = "Low Carbon Fill"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


p_strict_dist

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#       Lenient Classification        #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# In this classification, classify the skill as green if the majority of runs
# agree that it's green

df_lenient <- df_skills_2 %>% 
  group_by(standard_reference, skill_id, green) %>% 
  summarise(
    runs_count = sum(runs_agreeing),
    .groups = "drop"
  ) %>% 
  group_by(standard_reference, skill_id) %>% 
  slice_max(runs_count, with_ties = FALSE) %>% 
  filter(green == 1) %>% select(-green)

df_lenient %<>% 
  left_join(df_skills_2, by = c("standard_reference", "skill_id")) %>% 
  select(-green)

# Take slice_max() to get classifications

df_lenient %<>%
  group_by(standard_reference, skill_id) %>% 
  slice_max(runs_agreeing, with_ties = FALSE) %>% 
  ungroup()

# Distribution of skill classifications

lenient_dist <- df_lenient %>% 
  group_by(climate_related) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(low_carbon = if_else(climate_related %in% low_carbon_classes, 1, 0))

p_lenient_dist <- lenient_dist %>% 
  ggplot(aes(x = factor(climate_related, levels = climate_related), y = n, fill = factor(low_carbon))) +
  geom_bar(stat = "identity", col = "black") +
  scale_fill_manual(
    values = c("0" = "lightblue", "1" = "lightgreen"),
    labels = c("0" = "Non-Low Carbon", "1" = "Low Carbon")
  ) +
  theme_minimal() +
  labs(
    x = "Climate-Related Category",
    y = "Count of Skills",
    title = "Lenient Distribution of Climate-Related Skills",
    fill = "Low Carbon Fill"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_lenient_dist

p_strict_dist


# ============================================================================#
# ============================================================================#
# ==== # ======================== Export Files ========================= # ====
# ============================================================================#
# ============================================================================#

df_skills_class <- df_lenient %>%
  mutate(low_carbon = if_else(climate_related %in% low_carbon_classes, 1, 0))

df_lc_skills <- df_skills_class %>% filter(low_carbon == 1) %>% select(-low_carbon) %>%
  mutate(low_carbon = TRUE)

df_non_lc_skills <- df_skills_class %>% filter(low_carbon == 0) %>% select(-low_carbon) %>%
  mutate(low_carbon = FALSE)

library(writexl)

library(writexl)

exporting_xlsx <- function(df, name, dir) {
  base_dir <- dir
  df_exp <- df
  exp_path <- file.path(base_dir, paste0(name, ".xlsx"))
  write_xlsx(df_exp, path = exp_path)
}


exporting_xlsx(df_lc_skills, "lc_skills", "Data 2025/Classification Check")
exporting_xlsx(df_non_lc_skills, "non_lc_skills", "Data 2025/Classification Check")
