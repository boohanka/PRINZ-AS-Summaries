

rm(list = ls())


# ============================================================================#
# ============================================================================#
# ==== # ==================== Uploading & Setting ====================== # ====
# ============================================================================#
# ============================================================================#

df_nuts <- read_csv("Data 2025/Raw/Geography/Lightcast Geography/NUTS-Markets.csv")
df_lad_ward_county <- read_csv("Data 2025/Raw/Geography/Lightcast Geography/Ward-LAD-County.csv")
df_lad_to_ttwa <- read_dta("Data 2025/Raw/Geography/LAD21-TTWA-Shares.dta")

colnames(df_lad_ward_county) <- str_to_lower(colnames(df_lad_ward_county))

colnames(df_nuts) <- str_to_lower(colnames(df_nuts))
colnames(df_lad_ward_county) <- str_to_lower(colnames(df_lad_ward_county))

df_nuts %<>% filter(nation_name == "England")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#           Settings           #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

market_replacements <- c(
  " County" = "",
  "\\sAnd\\s" = " and ",
  "\\sUpon\\s" = " upon ",
  "\\sOf\\s" = " of ",
  "\\sWith\\s" = " with ",
  "-Upon-" = "-upon-",
  "-Under-" = "-under-",
  "-On-" = "-on-",
  "-By-" = "-by-",
  "-In-" = "-in-",
  "-Le-" = "-le-",
  "-Next-The-" = "-next-the-",
  "-The-" = "-the-",
  "-Sub-" = "-sub-",
  "'S" = "'s",
  "St. " = "St ",
  "Stke-on-Trent" = "Stoke-on-Trent",
  "Stckton" = "Stockton",
  "Marys" = "Mary's"
)

wiki_replacements <- c(
  "(?i)\\s?(\\(district\\)|\\(borough\\)|district|borough of\\s|borough\\b|borough_of_|city of\\s|council\\b|\\(unitary authority\\)|\\(uk\\)|royal\\b|,\\s?cumbria|,\\s?derbyshire|,\\s?lincolnshire|,\\s?nottinghamshire|,\\s?lancashire|,\\s?surrey|<ref name=\"bfc\"/>|<ref>)" = "",
  "Har" = "Harborough",
  "\\bPeter\\b(?!borough)" = "Peterborough"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#      Exporting Function      #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

exporting_csv <- function(df, name, dir) {
  base_dir <- dir
  df_exp <- df
  exp_path <- file.path(base_dir, paste0(name, ".csv"))
  write.csv(df_exp, file = exp_path, row.names = FALSE)
}

# ============================================================================#
# ============================================================================#
# ==== # ================= Manual Match Function ======================== # ===
# ============================================================================#
# ============================================================================#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#        Manual Match          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

f_market_to_lad <- function(df_market = df_nuts, df_corr = df_lad_ward_county) {
  
  # Step 1: Extract and clean "county" and "city" from market_name
  df_market <- df_market %>%
    mutate(county = sub(".*,\\s*(.*) County", "\\1", market_name),
           city = sub("^(.*),.*$", "\\1", market_name)) %>%
    mutate(across(c(county, city), str_squish)) %>%
    mutate(across(c(county, city), ~ str_replace_all(., "\\sAnd\\s", " and ") %>%
                    str_replace_all("\\sUpon\\s", " upon ") %>%
                    str_replace_all("\\sOf\\s", " of ") %>%
                    str_replace_all("\\sWith\\s", " with ") %>%
                    str_replace_all("-On-", "-on-"))) %>%
    mutate(
      county = if_else(
        grepl("^City of ", county) & county != "City of London",
        sub("^City of (.*)", "\\1, City of", county),
        county
      )
    )
  
  # Step 2: Pull unique LADs, Wards, and Counties from the correspondence table
  lads <- unique(df_corr$lad21nm)
  wds  <- unique(df_corr$wd21nm)
  cty  <- unique(df_corr$cty21nm)
  
  # Step 3: Matching Logic with Proper Method Handling
  df_market_to_lad <- df_market %>%
    rowwise() %>%
    mutate(
      # Initialize variables for lad and method
      lad = NA_character_,
      method = NA_character_,
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~ #
      #    1: Fuzzy Matching     #
      # ~~~~~~~~~~~~~~~~~~~~~~~~ #
      
      # Case 1 (i): CITY ⊆ WARD ≥ 1, WARD -> ≥ 1 LAD, COUNTY[.CITY] ⊆ LAD/CTY[CITY ⊆ WARD], 
      # length(unique(LAD[WARD ⊆ CITY])) = 1
      lad = if_else(
        is.na(lad) & sum(grepl(city, wds)) >= 1,
        {
          matched_lads <- df_corr$lad21nm[grepl(city, df_corr$wd21nm) & grepl(county, paste(df_corr$cty21nm, df_corr$lad21nm))]
          if (length(unique(matched_lads)) == 1) matched_lads[1] else NA_character_
        },
        lad
      ),
      method = if_else(is.na(method) & !is.na(lad), "1", method),
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~ #
      #    2: Sharp Matching     #
      # ~~~~~~~~~~~~~~~~~~~~~~~~ #
      
      # Case 2a: COUNTY = LAD
      lad = if_else(
        sum(county == lads) == 1,
        lads[which(county == lads)][1],
        lad
      ),
      method = if_else(!is.na(lad) & is.na(method), "2a", method),
      
      # Case 2b: CITY = LAD
      lad = if_else(
        is.na(lad) & sum(city == lads) == 1,
        lads[which(city == lads)][1],
        lad
      ),
      method = if_else(!is.na(lad) & is.na(method), "2b", method)
    ) %>% 
    ungroup() %>%
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~ #
    #       3: No Match        #
    # ~~~~~~~~~~~~~~~~~~~~~~~~ #
    mutate(
      lad = if_else(is.na(lad), "Undefined", lad),
      method = if_else(is.na(method), "3", method)
    )
  
  return(df_market_to_lad)
}

# ============================================================================#
# ============================================================================#
# ==== # ============== Manual to API - PREFERRED ======================= # ===
# ============================================================================#
# ============================================================================#

df_market_to_lad_manual <- f_market_to_lad()

df_markets_missing <- df_market_to_lad_manual %>% 
  filter(method == "3") %>% select(-method)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#       Export for API         #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# df_markets_to_api <- df_markets_missing %>% 
#   mutate(
#     market_name = market_name %>%
#       str_squish() %>%
#       str_replace_all(market_replacements)
#   ) %>% 
#   select(market_name)
# 
# exporting_csv(df_markets_to_api, "Remaining Markets for API", "Data 2025/Aux")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#      Import API Result       #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

df_api <- read_csv("Data 2025/Aux/Markets Lads API.csv")
df_api <- df_api %>% 
  mutate(lad = if_else(lad == "", NA_character_, lad) %>% str_replace_all(wiki_replacements)) 
  
df_market_to_lad <- df_market_to_lad_manual %>% 
  mutate(
    market_name = market_name %>%
      str_squish() %>%
      str_replace_all(market_replacements)
  ) %>% 
  left_join(
    df_api,
    by = "market_name",
    suffix = c("_manual", "_api")
  ) %>% 
  mutate(
    lad_manual = if_else(lad_manual == "Undefined", lad_api, lad_manual)
  ) %>% 
  rename(lad = lad_manual) %>% select(-lad_api)



# ============================================================================#
# ============================================================================#
# ==== # ======================= Fill by Hand =========================== # ===
# ============================================================================#
# ============================================================================#

df_market_to_lad %>% filter(is.na(lad)) %>% View()

markets_hand <- c("Oakley, Poole", "Guilden Morden, Cambridgeshire", "Haslingfield, Cambridgeshire",
             "St Mary's, Cambridgeshire", "Teversham, Cambridgeshire", "Calmore, Hampshire",
             "Colyton, Devon", "Bures, Essex", "Walton On The Naze, Essex", "Benfleet, Essex",
             "Eastington, Gloucestershire", "Over, Gloucestershire", "Waltham On The Wolds, Leicestershire",
             "Charnock, Lancashire", "Slade, Northamptonshire", "Kirkby In Ashfield, Nottinghamshire",
             "Woodhouse, Nottinghamshire", "Sherburn In Elmet, North Yorkshire", "King's Lynn, Norfolk",
             "Waterside, Norfolk", "Middle Barton, Oxfordshire", "New Yatt, Oxfordshire",
             "Aston, Derbyshire", "Hollingworth, Derbyshire", "Lynn, Staffordshire",
             "Denham, Suffolk", "Whitchurch, Warwickshire", "Henley, West Sussex",
             "Hillside, Worcestershire")

lads_hand <- c("Bournemouth, Christchurch and Poole", "South Cambridgeshire", "South Cambridgeshire",
          "Huntingdonshire", "South Cambridgeshire", "Test Valley", "East Devon", "Braintree",
          "Tendring", "Castle Point", "Stroud", "Stroud", "Melton", "Chorley", "West Northamptonshire",
          "Ashfield", "Mansfield", "Selby", "King's Lynn and West Norfolk", "Great Yarmouth",
          "West Oxfordshire", "West Oxfordshire", "Derbyshire Dales", "High Peak", "Stafford",
          "West Suffolk", "Stratford-on-Avon", "Chichester", "Malvern Hills")

df_market_to_lad_hand <- data.frame(market_name = markets_hand, lad = lads_hand)

df_m_lad_crosswalk <- df_market_to_lad %>% 
  left_join(df_market_to_lad_hand, by = "market_name") %>% 
  mutate(lad = coalesce(lad.x, lad.y)) %>% 
  select(-c(lad.x, lad.y))

# Check missing

df_m_lad_crosswalk %>% filter(is.na(lad)) %>% View()


# ============================================================================#
# ============================================================================#
# ==== # ==================== Markets to TTWA =========================== # ===
# ============================================================================#
# ============================================================================#

df_lad_to_ttwa %<>% rename(lad21cd = lad15cd) %>% 
  left_join(
    df_lad_ward_county %>% select(lad21cd, lad21nm) %>% 
      distinct(),
    by = "lad21cd"
  )

df_m_lad_ttwa_crosswalk <- df_m_lad_crosswalk %>% 
  rename(lad21nm = lad) %>% 
  left_join(
    df_lad_to_ttwa, 
    by = "lad21nm"
  )

m_ttwa_crosswalk <- df_m_lad_ttwa_crosswalk %>% 
  select(nuts_2021_3_name, market_name, ttwa11cd, ttwa11nm, share_pc_ttwa) 

# ============================================================================#
# ============================================================================#
# ==== # ================== Retrieve Market Names ======================= # ===
# ============================================================================#
# ============================================================================#

# Return to original formatting

market_name_lookup <- df_nuts %>% 
  select(market_name) %>% distinct() %>% 
  rename(market_name_raw = market_name) %>% 
  mutate(
    market_name = market_name_raw %>%
      str_squish() %>%
      str_replace_all(market_replacements)
  )

m_ttwa_crosswalk %<>%
  left_join(
    market_name_lookup, by = "market_name"
  ) %>% 
  select(-market_name) %>% 
  rename(market_name = market_name_raw)


# ============================================================================#
# ============================================================================#
# ==== # ========================== Export ============================== # ===
# ============================================================================#
# ============================================================================#

exporting_csv(m_ttwa_crosswalk, "Lightcast Market to TTWA Crosswalk", "Data 2025/Cleaned/Lightcast Markets")




















# df_manual_api <- df_market_to_lad %>% 
#   select(market_name, lad) %>% distinct()

# ============================================================================#
# ============================================================================#
# ==== # ====================== API to Manual =========================== # ===
# ============================================================================#
# ============================================================================#

# df_all_markets <- df_nuts %>%
#   mutate(
#     market_name = market_name %>%
#       str_squish() %>%
#       str_replace_all(market_replacements)
#   ) %>%
#   select(market_name)
# 
# #exporting_csv(df_all_markets, "all_lightcast_markets", "data/aux_data/new")
# 
# df_api <- f_upload("markets_to_lads_api", type = "aux", cat = "new") %>%
#   mutate(lad = if_else(lad == "", NA_character_, lad))
# 
# df_api_remaining <- df_api %>% filter(is.na(lad)) %>%
#   select(market_name)
# 
# df_market_to_lad_api_incomplete <- f_market_to_lad(df_market = df_api_remaining)
# 
# df_api_manual <- df_api %>%
#   left_join(df_market_to_lad_api_incomplete %>%
#               select(market_name, lad) %>% distinct(),
#             by = "market_name", suffix = c("_api", "_manual")) %>%
#   mutate(
#     lad_api = if_else(is.na(lad_api), lad_manual, lad_api)
#   ) %>%
#   rename(lad = lad_api) %>% select(-lad_manual) %>%
#   mutate(lad = if_else(lad == "Undefined", NA_character_, lad) %>% str_replace_all(wiki_replacements))
# 

# ============================================================================#
# ============================================================================#
# ==== # =================== Compare Approaches ========================= # ===
# ============================================================================#
# ============================================================================#

# df_comparison <- df_api_manual %>% 
#   left_join(df_manual_api, by = "market_name", suffix = c("_wiki", "_manual"))
# 
# df_not_equal <- df_comparison %>% 
#   filter(lad_wiki != lad_manual)
# 
# exporting_csv(df_not_equal, "not_equal", "data")

# ============================================================================#
# ============================================================================#
# ==== # ================= Check for Correct LADs ======================= # ===
# ============================================================================#
# ============================================================================#

# df_lads_missing <- df_market_to_lad %>% 
#   filter(is.na(lad))
# 
# exporting_csv(df_lads_missing, "not_crosswalked", "data")
# 
# lads <- df_lad_ward_county %>% 
#   filter(ctry21nm == "England") %>% 
#   select(lad21nm) %>% pull() %>% unique()
# 
# lads_crosswalked <- df_market_to_lad %>% 
#   select(lad) %>% pull() %>% unique()
# 
# lads_in_crosswalk_not_corr <- setdiff(lads_crosswalked, lads)
# lads_in_corr_not_crosswalk <- setdiff(lads, lads_crosswalked)
# lads_misnomer <- symdiff(lads_crosswalked, lads)
