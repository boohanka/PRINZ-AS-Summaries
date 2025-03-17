

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_ads_all_socs <- read_csv("Data/Intermediate/Ads/Time Series/Ads TS All SOCs.csv")
df_ads_as_socs <- read_csv("Data/Intermediate/Ads/Time Series/Ads TS AS SOCs.csv")

df_ads_all_socs %<>%
  mutate(
    date = as.yearqtr(date),
    across(contains("frac_ads"), function(x) x * 100), 
    classification = factor(classification, levels = unique(classification))
  )

df_ads_as_socs %<>%
  mutate(
    date = as.yearqtr(date),
    across(contains("frac_ads"), function(x) x * 100), 
    classification = factor(classification, levels = unique(classification))
  )

# ============================================================================#
# ============================================================================#
# ==== # ====================== Plot Time Series ======================= # ====
# ============================================================================#
# ============================================================================#

# ~~~~~~~~~~~~~~~~~ #
#    Plot Options   #
# ~~~~~~~~~~~~~~~~~ #

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

# ~~~~~~~~~~~~~~~~~ #
#   Generate Plots  #
# ~~~~~~~~~~~~~~~~~ #

# Unweighted, Unaligned

p_ads_all_socs <- df_ads_all_socs %>% 
  filter(classification != "Non-low-carbon") %>% 
  ggplot(aes(x = date, y = frac_ads_class, color = classification)) +
  geom_line(linewidth = 1.2) + 
  geom_point() +
  scale_x_yearqtr(format = "%Y", breaks = seq(min(df_ads_all_socs$date), max(df_ads_all_socs$date), by = 1)) +  
  scale_color_viridis_d(name = "Classification") +  
  theme_setting +
  labs(y = "% Skills Advertised", x = "Year")

p_ads_all_socs

# Weighted, Unaligned

p_weighted_ads_all_socs <- df_ads_all_socs %>% 
  filter(classification != "Non-low-carbon") %>% 
  ggplot(aes(x = date, y = weighted_frac_ads_class, color = classification)) +
  geom_line(linewidth = 1.2) + 
  geom_point() +
  scale_x_yearqtr(format = "%Y", breaks = seq(min(df_ads_all_socs$date), max(df_ads_all_socs$date), by = 1)) +  
  scale_color_viridis_d(name = "Classification") +  
  theme_setting +
  labs(y = "% Skills Advertised: Weighted", x = "Year")

p_weighted_ads_all_socs


# Unweighted, Aligned

p_ads_as_socs <- df_ads_as_socs %>% 
  filter(classification != "Non-low-carbon") %>% 
  ggplot(aes(x = date, y = frac_ads_class, color = classification)) +
  geom_line(linewidth = 1.2) + 
  geom_point() +
  scale_x_yearqtr(format = "%Y", breaks = seq(min(df_ads_all_socs$date), max(df_ads_all_socs$date), by = 1)) +  
  scale_color_viridis_d(name = "Classification") +  
  theme_setting +
  labs(y = "% Skills Advertised", x = "Year")

p_ads_as_socs

# Weighted, Aligned

p_weighted_ads_as_socs <- df_ads_as_socs %>% 
  filter(classification != "Non-low-carbon") %>% 
  ggplot(aes(x = date, y = weighted_frac_ads_class, color = classification)) +
  geom_line(linewidth = 1.2) + 
  geom_point() +
  scale_x_yearqtr(format = "%Y", breaks = seq(min(df_ads_all_socs$date), max(df_ads_all_socs$date), by = 1)) +  
  scale_color_viridis_d(name = "Classification") +  
  theme_setting +
  labs(y = "% Skills Advertised: Weighted", x = "Year")

p_ads_as_socs
p_weighted_ads_as_socs


# ============================================================================#
# ============================================================================#
# ============================= Export =======================================
# ============================================================================#
# ============================================================================#

exporting_plots <- function(plot, name, dir, width, height) {
  exp_path <- file.path(dir, paste0(name, ".pdf"))
  ggsave(exp_path, plot, device = "pdf", width = width, height = height)
}

output_dir <- "Outputs/Ads/Time Series"

exporting_plots(p_ads_as_socs, "Ads TS AS SOCs", output_dir, 12, 7)
exporting_plots(p_weighted_ads_as_socs, "Ads TS AS SOCs - Weighted", output_dir, 12, 7)
exporting_plots(p_ads_all_socs, "Ads TS All SOCs", output_dir, 12, 7)
exporting_plots(p_weighted_ads_all_socs, "Ads TS All SOCs - Weighted", output_dir, 12, 7)

