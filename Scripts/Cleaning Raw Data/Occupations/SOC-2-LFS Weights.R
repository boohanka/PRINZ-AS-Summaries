rm(list = ls())

# ============================================================================#
# ============================================================================#
# ==== # ========================= Uploading =========================== # ====
# ============================================================================#
# ============================================================================#

df_lfs_1517 <- read_dta("Data/Raw/LFS Weights/2015-2017/aps_3yr_jan15dec17_eul.dta")
df_lfs_1820 <- read_dta("Data/Raw/LFS Weights/2018-2020/aps_3yr_jan18dec20_eul.dta")
colnames(df_lfs_1517) <- str_to_lower(colnames(df_lfs_1517))
colnames(df_lfs_1820) <- str_to_lower(colnames(df_lfs_1820))

# NOTE:

# 0) idref: ID 
# 1) ILODEFR: 1 = In Employment
# 2) CONMPY: Year Current Job
# 3) CONMON: Month Current Job
# 4) EMPLEN: Length of Time Current Job
# 5) REFDTE: Reference Date
# 6) PWTA17: Person Weight
# 7) COUNTRY: 1 = England
# 8) SC10MMJ: Major Occupation SOC
# 9) SC10MMN: Minor Occupation SOC

# Additionally:

# a) -9 = Doesn't Apply
# b) -8 = No Answer

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



# ============================================================================#
# ============================================================================#
# ==== # =========================== Clean ============================= # ====
# ============================================================================#
# ============================================================================#

# 2015-2017 

df_1517 <- df_lfs_1517 %>% 
  select(
    c(idref, pwta17c, ilodefr, sc10mmj, sc10mmn, conmpy, conmon, country)
  )

df_1517 %<>%
  mutate(across(everything(), as.numeric)) %>% 
  filter(
    !if_any(c(sc10mmj, sc10mmn), ~ . == -9 | . == -8)
  ) %>% 
  filter(
    country == 1 & conmpy %in% c(2015:2017) & ilodefr == 1
  )

df_1517_soc3d <- df_1517 %>% 
  group_by(sc10mmn) %>% 
  summarise(
    wgt = sum(pwta17c), 
    .groups = "drop"
  ) %>% 
  arrange(desc(wgt)) %>% 
  mutate(
    weight = (wgt/sum(wgt)) * 100
  ) %>% 
  select(-wgt)


# 2018-2020 

df_1820 <- df_lfs_1820 %>% 
  select(
    c(idref, pwta20c, ilodefr, sc10mmj, sc10mmn, conmpy, conmon, country)
  )

df_1820 %<>%
  mutate(across(everything(), as.numeric)) %>% 
  filter(
    !if_any(c(sc10mmj, sc10mmn), ~ . == -9 | . == -8)
  ) %>% 
  filter(
    country == 1 & conmpy %in% c(2018:2020) & ilodefr == 1
  )

df_1820_soc3d <- df_1820 %>% 
  group_by(sc10mmn) %>% 
  summarise(
    wgt = sum(pwta20c), 
    .groups = "drop"
  ) %>% 
  arrange(desc(wgt)) %>% 
  mutate(
    weight = (wgt/sum(wgt)) * 100
  ) %>% 
  select(-wgt)


# Combine

lfs_soc3d_dist <- df_1517_soc3d %>% 
  left_join(
    df_1820_soc3d, by = "sc10mmn", suffix = c("1517", "1820")
  )

# Also take simple mean to calculate average weights:

lfs_soc3d_dist %<>%
  mutate(
    avg_weight = weight1517 * 0.5 + weight1820 * 0.5
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

exporting_csv(lfs_soc3d_dist, "LFS SOC3D Distribution", "Data/Intermediate")