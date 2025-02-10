

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_starts_raw <- read_csv("Data-2025/Raw/AS/app-underlying-data-app-starts-202324-q1.csv")
df_lad_changes <- read_xlsx("Data-2025/Raw/Geography/LAD-Changes-2020.xlsx")
df_lad_ttwa <- read_dta("Data-2025/Raw/Geography/LAD21-TTWA-Shares.dta")

# Change the LAD changes

df_lad_changes %<>%
  rename(lad15cd = `Old Code`, lad = `New Code`) %>% 
  filter(`Operative Year` == 2019 | `Operative Year` == 2020) %>% 
  select(lad15cd, lad)
  
  

# ============================================================================#
# ============================================================================#
# ==== # ========== Select Columns and Remove Missing Values =========== # ====
# ============================================================================#
# ============================================================================#

colnames(df_starts_raw)

cols_to_keep <- c("start_quarter", "year", "delivery_lad_code", "delivery_lad",
                  "st_code", "starts")

df_starts <- df_starts_raw %>% select(all_of(cols_to_keep))

# Transform date variable:

df_starts <- df_starts %>% 
  mutate(
    start_quarter = paste0("Q", start_quarter),
    year = substr(year, 1, 4),
    date = paste(year, start_quarter)
  ) %>% 
  select(-c(start_quarter, year)) %>% 
  mutate(date = as.yearqtr(date)) %>% 
  select(date, everything())

# Drop non-AS:

df_starts <- df_starts %>% 
  filter(!is.na(st_code))


# ============================================================================#
# ============================================================================#
# ==== # ==================== Add TTWA Geography ====================== # ====
# ============================================================================#
# ============================================================================#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#          LAD Fixing          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

df_starts <- df_starts %>% 
  rename(
    lad15cd = delivery_lad_code
  ) %>% 
  select(-delivery_lad)

# Remove non-England

df_starts %<>% filter(lad15cd != "z")

# Account for new LADs:

df_starts %<>% 
  left_join(df_lad_changes, by = "lad15cd") %>% 
  mutate(lad = if_else(is.na(lad), lad15cd, lad)) %>% 
  select(-lad15cd) 

missing_lads <- data.frame(
  old_lad = c("E07000152", "E06000066", "E06000065", "E06000063", "E07000156", 
              "E06000064", "E07000155", "E07000151", "E07000153", "E07000150"),
  new_lad = c("E06000061", "E06000052", "E06000063", "E06000058", "E06000061", 
              "E06000059", "E06000062", "E06000062", "E06000061", "E06000061")
)

df_starts$lad <- ifelse(df_starts$lad %in% missing_lads$old_lad, 
                        missing_lads$new_lad[match(df_starts$lad, missing_lads$old_lad)], 
                        df_starts$lad)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#           TTWA Join          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Add geography

df_starts %<>% 
  left_join(df_lad_ttwa, join_by(lad == lad15cd))

# Weight starts by shares

df_starts %<>%
  mutate(starts = starts * share_pc_ttwa)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#           Aggregate          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

df_starts %<>% 
  group_by(date, st_code, ttwa11cd, ttwa11nm) %>% 
  summarise(
    starts = sum(starts),
    .groups = "drop"
  )

# ============================================================================#
# ============================================================================#
# ==== # ========================== Export ============================== # ===
# ============================================================================#
# ============================================================================#

exporting_csv <- function(df, name, dir) {
  base_dir <- dir
  df_exp <- df
  exp_path <- file.path(base_dir, paste0(name, ".csv"))
  write.csv(df_exp, file = exp_path, row.names = FALSE)
}

exporting_csv(df_starts, "AS-Starts-st_q_g", "Data-2025/Cleaned/AS")
