

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_as_skills <- read_csv("Data 2025/Cleaned/AS/List AS Skills.csv")
df_skills_starts <- read_csv("Data 2025/Cleaned/AS/Skills-Full-Starts-class_q_g.csv")

library(sf)
sf_boundaries <- st_read("Data 2025/Raw/Geography/TTWA Shape Files/TTWA_2011_UK_BGC_V3.shp")
colnames(sf_boundaries) <- str_to_lower(colnames(sf_boundaries))

# ============================================================================#
# ============================================================================#
# ==== # ========== Aggregate Geographies - % Change 2018-2022 ========= # ====
# ============================================================================#
# ============================================================================#

df_g_change <- df_skills_starts %>% 
  mutate(date = year(as.yearqtr(date))) %>% 
  group_by(date, ttwa11cd, ttwa11nm, climate_related) %>% 
  summarise(
    starts = sum(starts),
    .groups = "drop"
  ) %>% 
  group_by(date, ttwa11cd, ttwa11nm) %>% 
  mutate(
    total = sum(starts),
    frac = if_else(total > 0, (starts/total) * 100, 0)
  ) %>% 
  ungroup() %>% 
  group_by(ttwa11cd, ttwa11nm, climate_related) %>% 
  reframe(
    frac_change = frac[date == 2022] - frac[date == 2018],
    starts_change = starts[date == 2022] - starts[date == 2018]
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#       Generate Quintiles        #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Prepare Quintile Data

f_change_quint <- function(df = df_g_change) {
  
  df %<>% filter(climate_related != "Not Low Carbon")
  
  classes <- df %>% pull(climate_related) %>% unique()
  
  ql_changes <- list()
  
  # Loop over each classification
  for (class in classes) {
    # Handle positive changes
    df_inc <- df %>% 
      filter(frac_change >= 0 & climate_related == class) %>% 
      select(-climate_related) %>% 
      mutate(q = ntile(frac_change, n = 5))
    
    df_ql_inc <- df_inc %>% 
      group_by(q) %>% 
      summarise(
        min = min(frac_change), 
        max = max(frac_change), 
        .groups = "drop"
      ) %>% 
      mutate(
        ql = paste0("Q", q, " (\u22650): ",
                    sprintf("%.3f", min), "-", 
                    sprintf("%.3f", max), "%")
      ) %>% 
      select(-c(min, max)) %>% 
      right_join(df_inc, by = "q") %>% 
      mutate(q = factor(ql, levels = unique(ql))) %>% 
      select(-ql)
    
    # Handle negative changes
    df_ql_dec <- df %>% 
      filter(frac_change < 0 & climate_related == class) %>% 
      select(-climate_related) %>% 
      mutate(q = "<0%") %>% 
      select(q, ttwa11cd, ttwa11nm, frac_change, starts_change)
    
    # Combine positive and negative changes
    df_ql <- bind_rows(df_ql_inc, df_ql_dec) %>% 
      arrange(q) %>% 
      mutate(q = factor(q, levels = unique(q)))
    
    # Join with spatial boundaries
    sf_ql <- df_ql %>%
      full_join(sf_boundaries, by = "ttwa11cd") %>% 
      st_as_sf() %>% 
      filter(!str_detect(ttwa11cd, "^(N|S|W)")) 
    
    # Store in list
    ql_changes[[class]] <- sf_ql
    
  }
  
  return(ql_changes)

}

ql_changes <- f_change_quint()

# ============================================================================#
# ============================================================================#
# ==== # ================== Plot Changes for Geographies =============== # ====
# ============================================================================#
# ============================================================================#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#          Plot Settings          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

theme_settings <- theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 22, face = "bold", color = "blue", hjust = 0.5),
    legend.margin = margin(b = 15, unit = "pt")
  )  

library(RColorBrewer)
color_inc <- colorRampPalette(brewer.pal(9, "Greens"))(20)[c(3, 6, 9, 12, 15)]
color_dec <- colorRampPalette(brewer.pal(9, "Reds"))(20)[8]
color_change <- c(color_dec, color_inc)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#              Plot               #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

classes <- df_as_skills$climate_related %>% unique()
class <- classes[4]
#class <- "Decarbonization-buildings"
names(color_change) <- levels(ql_changes[[class]]$q)

p_frac_change <- ql_changes[[class]] %>% 
  ggplot() +
  geom_sf(aes(fill = q), color = NA) +  
  scale_fill_manual(values = color_change, na.value = "grey") +
  labs(
    x = "Longitude", y = "Latitude",
    fill = paste0(class, "\nSkills Starts: Quintiles")) + theme_settings

p_frac_change

