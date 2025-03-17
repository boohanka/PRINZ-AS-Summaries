

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_soc3d_weights <- read_csv("Data/Intermediate/Occupations/SOC Weights.csv")

# ============================================================================#
# ============================================================================#
# ==== # ============== Recreating 2 Digit SOC Distribution ============ # ====
# ============================================================================#
# ============================================================================#

df_soc3d_weights %<>%
  mutate(soc2d_code = substr(soc3d_code, 1, 2))

f_assign_soc2d_desc <- function(df = df_soc3d_weights) {
  
  soc2d_crosswalk <- c(
    "11" = "Corporate managers and directors",
    "12" = "Other managers and proprietors",
    "21" = "Science, research, engineering and technology professionals",
    "22" = "Health professionals",
    "23" = "Teaching and educational professionals",
    "24" = "Business, media and public service professionals",
    "31" = "Science, engineering and technology associate professionals",
    "32" = "Health and social care associate professionals",
    "33" = "Protective service occupations",
    "34" = "Culture, media and sports occupations",
    "35" = "Business and public service associate professionals",
    "41" = "Administrative occupations",
    "42" = "Secretarial and related occupations",
    "51" = "Skilled agricultural and related trades",
    "52" = "Skilled metal, electrical and electronic trades",
    "53" = "Skilled construction and building trades",
    "54" = "Textiles, printing and other skilled trades",
    "61" = "Caring personal service occupations",
    "62" = "Leisure, travel and related personal service occupations",
    "71" = "Sales occupations",
    "72" = "Customer service occupations",
    "81" = "Process, plant and machine operatives",
    "82" = "Transport and mobile machine drivers and operatives",
    "91" = "Elementary trades and related occupations",
    "92" = "Elementary administration and service occupations"
  )
  
  
  df %<>%
    mutate(soc2d_desc = soc2d_crosswalk[as.character(soc2d_code)])
  
  return(df)
  
}

df_soc <- f_assign_soc2d_desc()

df_soc2d_dist <- df_soc %>% 
  group_by(soc2d_code, soc2d_desc) %>% 
  summarise(
    across(
      c("lfs_soc_share", "lightcast_soc_share"), 
      ~ round(sum(., na.rm = TRUE) * 100, 2),
      .names = "{.col}_pct"
    ),
    .groups = "drop"
  ) %>% 
  arrange(soc2d_code)

# ============================================================================#
# ============================================================================#
# ==== # ======================= Clean Table =========================== # ====
# ============================================================================#
# ============================================================================#

df_representativeness_table <- df_soc2d_dist %>% 
  mutate(
    `UK SOC sub-major group` = paste0(soc2d_code, " - ", soc2d_desc)
  ) %>% 
  rename(
    `Unweighted ad share` = lightcast_soc_share_pct,
    `LFS employment share (job starts 2018-2020)` = lfs_soc_share_pct
  ) %>% 
  select(
    `UK SOC sub-major group`, `Unweighted ad share`, `LFS employment share (job starts 2018-2020)`
  )

# ============================================================================#
# ============================================================================#
# ==== # ========================== Export ============================= # ====
# ============================================================================#
# ============================================================================#

library(writexl)
library(flextable)
library(officer)

exporting_xlsx <- function(df, name, dir) {
  exp_path <- file.path(dir, paste0(name, ".xlsx"))
  write_xlsx(df, path = exp_path)
}

exporting_xlsx(df_representativeness_table, "Representativeness Lightcast ads vs UK LFS", "Outputs/Occupations")

# Create a table as PDF:

ft_representativeness <- flextable(df_representativeness_table)
doc <- read_docx() %>% 
  body_add_flextable(ft_representativeness) %>% 
  print(target = "Outputs/Occupations/Representativeness Lightcast ads vs UK LFS.docx")
