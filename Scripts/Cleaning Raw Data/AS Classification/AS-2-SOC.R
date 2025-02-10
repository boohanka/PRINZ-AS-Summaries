

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_as_skills <- read_csv("Data/Cleaned/AS/List AS Skills.csv")
df_as_soc <- read_csv("Data/Raw/AS/AS-UKSOC.csv")

colnames(df_as_soc)

as_soc_colnames <- c(
  "standard_reference",
  "v",
  "standard_name",
  "route",
  "soc10_code",
  "soc10_desc"
)

colnames(df_as_soc) <- as_soc_colnames

# ============================================================================#
# ============================================================================#
# ==== # =================== List of AS Occupations ==================== # ====
# ============================================================================#
# ============================================================================#

df_lc_as <- df_as_skills %>% 
  filter(lc_standard) %>% 
  filter(climate_related != "Not Low Carbon") %>% 
  select(standard_reference, standard_name, climate_related) %>% 
  distinct()

list_lc_standards <- df_lc_as$standard_reference  

df_lc_as_soc <- df_as_soc %>% 
  filter(standard_reference %in% list_lc_standards) %>% 
  mutate(v = as.numeric(v)) %>% 
  group_by(standard_reference) %>% 
  slice_max(v, n = 1) %>%
  ungroup()

df_lc_as_soc %<>% 
  left_join(
    df_lc_as %>% 
      select(standard_reference, climate_related),
    by = "standard_reference"
  )





df_inconsistent_soc_lc <- read_csv("Data/Intermediate/List LC AS-SOC Manual Check.csv")
num_inconsistent <- nrow(df_inconsistent_soc_lc) - sum(df_inconsistent_soc_lc$is_consistent_with_lc)

# ============================================================================#
# ============================================================================#
# ==== # ======================== Export Files ========================= # ====
# ============================================================================#
# ============================================================================#

exporting_csv <- function(df, name, dir) {
  base_dir <- dir
  df_exp <- df
  exp_path <- file.path(base_dir, paste0(name, ".csv"))
  write.csv(df_exp, file = exp_path, row.names = FALSE)
}

exporting_csv(df_lc_as_soc, "List LC AS-SOC", "Data/Intermediate")
