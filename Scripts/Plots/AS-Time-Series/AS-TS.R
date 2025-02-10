

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_as_skills <- read_csv("Data 2025/Cleaned/AS/List AS Skills.csv")
df_skills_starts <- read_csv("Data 2025/Cleaned/AS/Skills-Starts-class_q_g.csv")

# ============================================================================#
# ============================================================================#
# ==== # =================== Aggregate Time Series ===================== # ====
# ============================================================================#
# ============================================================================#

df_ts <- df_skills_starts %>% 
  group_by(date, climate_related) %>% 
  summarise(
    starts = sum(starts),
    .groups = "drop"
  ) %>% 
  group_by(date) %>% 
  mutate(
    total = sum(starts),
    frac = (starts/total) * 100
  ) %>% 
  ungroup()

df_ts %<>% mutate(date = as.yearqtr(date))

df_y_ts <- df_ts %>% 
  mutate(date = year(date)) %>% 
  group_by(date, climate_related) %>% 
  summarise(
    starts = sum(starts),
    .groups = "drop"
  ) %>% 
  group_by(date) %>% 
  mutate(
    total = sum(starts),
    frac = (starts/total) * 100
  ) %>% 
  ungroup()


df_ts %>% filter(is.na(starts)) %>% View()

# ============================================================================#
# ============================================================================#
# ==== # ====================== Plot Time Series ======================= # ====
# ============================================================================#
# ============================================================================#

theme_setting <- theme_linedraw() +
  theme(
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 18, face = "bold"),
    legend.position = "right",
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20, face = "bold"),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank()  
  )


p_frac_ts <- df_y_ts %>% 
  filter(climate_related != "Not Low Carbon") %>% 
  filter(date < 2023) %>% 
  ggplot(aes(x = date, y = frac, color = climate_related)) +
  geom_line(linewidth = 1.2) + 
  geom_point() +
  scale_color_viridis_d(name = "Classification") +  
  theme_setting +
  labs(y = "% Skills Started", x = "Year")

p_frac_ts

