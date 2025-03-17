

rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_as_soc <- read_csv("Data/Raw/AS/AS-UKSOC.csv")
df_as_skills <- read_csv("Data/Cleaned/AS/List AS Skills.csv")
df_soc_list <- read_xlsx("Data/Raw/Occupations/UKSOC Codes.xlsx")

# Clean SOC List:

colnames(df_soc_list) <- c("soc4d_code", "soc4d_desc")

df_soc_list %<>%
  mutate(
    soc4d_code = as.numeric(soc4d_code),
    soc4d_desc = as.character(soc4d_desc)
  )

# Clean AS SOC Crosswalk:

colnames(df_as_soc)

as_soc_colnames <- c(
  "standard_reference",
  "v",
  "standard_name",
  "route",
  "soc4d_code",
  "soc4d_desc"
)

colnames(df_as_soc) <- as_soc_colnames

 
# ============================================================================#
# ============================================================================#
# ==== # ===================== Add 3 Digit SOCs ======================== # ====
# ============================================================================#
# ============================================================================#

df_soc_list %<>%
  filter(nchar(soc4d_code) == 4) %>% 
  mutate(soc3d_code = substr(soc4d_code, 1, 3))

f_assign_soc3d_desc <- function(df = df_soc_list) {
  
  soc3d_crosswalk <- c(
    "111" = "Chief executives and senior officials",
    "112" = "Production managers and directors",
    "113" = "Functional managers and directors",
    "115" = "Financial institution managers and directors",
    "116" = "Managers and directors in transport and logistics",
    "117" = "Senior officers in protective services",
    "118" = "Health and social services managers and directors",
    "119" = "Managers and directors in retail and wholesale",
    "121" = "Managers and proprietors in agriculture related services",
    "122" = "Managers and proprietors in hospitality and leisure services",
    "124" = "Managers and proprietors in health and care services",
    "125" = "Managers and proprietors in other services",
    "211" = "Natural and social science professionals",
    "212" = "Engineering professionals",
    "213" = "Information technology and telecommunications professionals",
    "214" = "Conservation and environment professionals",
    "215" = "Research and development managers",
    "221" = "Health professionals",
    "222" = "Therapy professionals",
    "223" = "Nursing and midwifery professionals",
    "231" = "Teaching and educational professionals",
    "241" = "Legal professionals",
    "242" = "Business, research and administrative professionals",
    "243" = "Architects, town planners and surveyors",
    "244" = "Welfare professionals",
    "245" = "Librarians and related professionals",
    "246" = "Quality and regulatory professionals",
    "247" = "Media professionals",
    "311" = "Science, engineering and production technicians",
    "312" = "Draughtspersons and related architectural technicians",
    "313" = "Information technology technicians",
    "321" = "Health associate professionals",
    "323" = "Welfare and housing associate professionals",
    "331" = "Protective service occupations",
    "341" = "Artistic, literary and media occupations",
    "342" = "Design occupations",
    "344" = "Sports and fitness occupations",
    "351" = "Transport associate professionals",
    "352" = "Legal associate professionals",
    "353" = "Business, finance and related associate professionals",
    "354" = "Sales, marketing and related associate professionals",
    "355" = "Conservation and environmental associate professionals",
    "356" = "Public services and other associate professionals",
    "411" = "Administrative occupations: government and related organisations",
    "412" = "Administrative occupations: finance",
    "413" = "Administrative occupations: records",
    "415" = "Other administrative occupations",
    "416" = "Administrative occupations: office managers and supervisors",
    "421" = "Secretarial and related occupations",
    "511" = "Agricultural and related trades",
    "521" = "Metal forming, welding and related trades",
    "522" = "Metal machining, fitting and instrument making trades",
    "523" = "Vehicle trades",
    "524" = "Electrical and electronic trades",
    "525" = "Skilled metal, electrical and electronic trades supervisors",
    "531" = "Construction and building trades",
    "532" = "Building finishing trades",
    "533" = "Construction and building trades supervisors",
    "541" = "Textiles and garments trades",
    "542" = "Printing trades",
    "543" = "Food preparation and hospitality trades",
    "544" = "Other skilled trades",
    "612" = "Childcare and related personal services",
    "613" = "Animal care and control services",
    "614" = "Caring personal services",
    "621" = "Leisure and travel services",
    "622" = "Hairdressers and related services",
    "623" = "Housekeeping and related services",
    "624" = "Cleaning and housekeeping managers and supervisors",
    "711" = "Sales assistants and retail cashiers",
    "712" = "Sales related occupations",
    "713" = "Sales supervisors",
    "721" = "Customer service occupations",
    "722" = "Customer service managers and supervisors",
    "811" = "Process operatives",
    "812" = "Plant and machine operatives",
    "813" = "Assemblers and routine operatives",
    "814" = "Construction operatives",
    "821" = "Road transport drivers",
    "822" = "Mobile machine drivers and operatives",
    "823" = "Other drivers and transport operatives",
    "911" = "Elementary agricultural occupations",
    "912" = "Elementary construction occupations",
    "913" = "Elementary process plant occupations",
    "921" = "Elementary administration occupations",
    "923" = "Elementary cleaning occupations",
    "924" = "Elementary security occupations",
    "925" = "Elementary sales occupations",
    "926" = "Elementary storage occupations",
    "927" = "Other elementary services occupations"
  )
  
  df %<>%
    mutate(soc3d_desc = soc3d_crosswalk[as.character(soc3d_code)])
  
  return(df)
  
}
  
df_soc <- f_assign_soc3d_desc()

# ============================================================================#
# ============================================================================#
# ==== # ===================== Extracting AS SOCs ====================== # ====
# ============================================================================#
# ============================================================================#

# Take most recent crosswalk version:

df_as_soc %<>%
  filter(!is.na(soc4d_code)) %>% 
  filter(!str_detect(standard_name, regex("degree", ignore_case = TRUE))) %>% 
  group_by(standard_reference, standard_name) %>% 
  slice_max(v, n = 1) %>% 
  ungroup() %>% 
  select(-v)

as_soc4d <- df_as_soc$soc4d_code %>% unique()

# set_as_soc3d <- substr(set_as_soc4d, 1, 3) %>% unique()
# 
# set_flag_as_soc <- df_soc_list %>% 
#   filter(soc3d_code %in% set_as_soc3d) %>% 
#   pull(soc4d_code) 

# ============================================================================#
# ============================================================================#
# ==== # ================ Restricting SOCs on AS Sample ================ # ====
# ============================================================================#
# ============================================================================#


f_sample_restrictions <- function(df = df_soc_list) {
  
  # ~~~~~~~~~~~~~~~~~~~~~~ # 
  #       Flag AS SOCs     #
  # ~~~~~~~~~~~~~~~~~~~~~~ # 
  
  flag_as_soc <- as_soc4d
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
  #       Flag LC Sectors      #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
  
  transport_soc <- c(
    "1161", "1162", "3511", "3512", "3513", 
    "8211", "8212", "8213", "8214", "8215", 
    "8221", "8222", "8223", "8229", "8231", 
    "8232", "8233", "8234", "8239"
  )
  
  buildings_soc <- c(
    "1122", "1251", "2431", "2432", "2433", 
    "2434", "2435", "2436", "3114", "3121", 
    "3122", "5311", "5312", "5313", "5314", 
    "5315", "5316", "5319", "5321", "5322", 
    "5323", "5330", "8141", "8142", "8143", 
    "8149"
  )
  
  energy_soc <- c(
    "2113", "2141", "2142", "2150", "2123", 
    "2124", "2126", "2127", "8124", "8126", 
    "1255", "3565", "3567"
  )
  
  # Combine all
  flag_sector_soc <- c(
    transport_soc,
    buildings_soc,
    energy_soc
  )
  
  # ~~~~~~~~~~~~~~~~~ # 
  #   Combine Flags   #
  # ~~~~~~~~~~~~~~~~~ # 
  
  df %<>% mutate(
    soc4d_code = as.character(soc4d_code),  
    flag_as_soc = soc4d_code %in% flag_as_soc,
    flag_sector_soc = soc4d_code %in% flag_sector_soc,
    flag_educ = !substr(soc4d_code, 1, 1) %in% c("1", "2")
  )
  
  return(df)
}

df_soc_sample <- f_sample_restrictions()

# ============================================================================#
# ============================================================================#
# ==== # ================= Distribution of Sample SOCs ================= # ====
# ============================================================================#
# ============================================================================#

dist_socs <- df_soc_sample %>% 
  distinct(soc4d_code, .keep_all = TRUE) %>%
  summarise(
    across(starts_with("flag"), ~ sum(.))
  )

# ============================================================================#
# ============================================================================#
# ==== # ========================== Export ============================= # ====
# ============================================================================#
# ============================================================================#


exporting_csv <- function(df, name, dir) {
  exp_path <- file.path(dir, paste0(name, ".csv"))
  write.csv(df, file = exp_path, row.names = FALSE)
}

exporting_csv(df_soc_sample, "SOC Restriction Flags", "Data/Intermediate/Occupations")

  
