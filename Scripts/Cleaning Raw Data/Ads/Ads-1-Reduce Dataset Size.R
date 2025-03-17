

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

library("nanoparquet")
library("arrow")
library("furrr")

ds_ads <- open_dataset("Data/Raw/Ads/uk_ads_skills_soc_ttwa.parquet")

ds_ads$schema

unique_years <- ds_ads %>%
  select(year) %>%
  distinct() %>%
  collect() %>%
  arrange(year) %>% 
  pull(year) 

# ============================================================================#
# ============================================================================#
# ==== # ===================== Reduce Dataset Size ===================== # ====
# ============================================================================#
# ============================================================================#

f_reduce_dataset <- function(years = c(2018:2024)) {
  
  process_year <- function(year) {
    
    # Open the dataset inside each worker
    ds <- open_dataset("Data/Raw/Ads/uk_ads_skills_soc_ttwa.parquet")
    
    # Load the dataset into memory:
    df_year <- ds %>%
      filter(year == !!year) %>%  
      select(-n_ads_ttwa_occ) %>%  
      collect()  
    
    # Split dataset into low-carbon/green and non-low-carbon
    df_year_lc <- df_year %>% 
      filter(low_carbon == 1 | green == 1)
    
    df_year_non_lc <- df_year %>% 
      filter(low_carbon == 0 & green == 0) %>%  
      group_by(
        year, month, 
        ttwa11cd, ttwa11nm, 
        soc4d_code, soc4d_desc
      ) %>%
      summarise(
        n_ads_skill = sum(n_ads_skill, na.rm = TRUE), 
        .groups = "drop"
      ) %>%
      mutate(skill = "Non-low-carbon")
    
    # Combine the datasets
    df_year <- bind_rows(df_year_lc, df_year_non_lc)
    
    # Convert to quarterly data
    df_year <- df_year %>%
      mutate(
        date = as.yearqtr(as.yearmon(paste0(year, "-", month)))  
      ) %>%
      select(-c(year, month)) %>% 
      group_by(
        date, 
        ttwa11cd, ttwa11nm, 
        soc4d_code, soc4d_desc, 
        skill, low_carbon, green
      ) %>%
      summarise(
        n_ads_skill = sum(n_ads_skill, na.rm = TRUE), 
        .groups = "drop"
      )
    
    # Save to disk instead of keeping in memory
    arrow::write_parquet(df_year, paste0("Data/Intermediate/Ads/Size/ads_", year, ".parquet"))
    
    # Free memory
    gc()  
    
    return(NULL)  # Explicitly return NULL to avoid memory retention
  }
  
  # Process years sequentially to avoid memory crashes
  walk(years, process_year)
}

# Run the function
f_reduce_dataset()

