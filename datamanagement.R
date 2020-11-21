writeLines("Running datamanagement.R")

load("datadump.Rda")
load("datadump_fact.Rda")
load("datadump_scen.Rda")

derivTypes <- c("Bond Futures", "Options on Bond Futures")

dataSet <- holdSet %>%
  mutate(CntryRisk = ifelse(is.na(CntryRisk)|CntryRisk=="", Cntry, CntryRisk),
         MktCap = ifelse(MarketCapPort == 0, MarketCapBench, MarketCapPort),
         MktCap = cut(MktCap, breaks = c(0, 1500, 8500, 40000, +Inf), labels = c("Small", "Mid", "Big", "Mega")),
         Sector = case_when(SecurityType %in% derivTypes ~ "Government", 
                            is.na(Sector) & SecurityType == "Cash" ~ "CashFX",
                            TRUE ~ Sector)) %>%
  left_join(geoMap, by = c("CntryRisk" = "ISO2")) %>%
  left_join(secTypeMap, by = "SecurityType") %>%
  #left_join(rateCtryMap, by = )
  mutate(MSCI_DC = ifelse(MSCI_DC==1, "DEVELOPED", "EMERGING/FRONTIER"),
         IsDerivative = (IsDerivative == 1)) %>%
  select(-starts_with("update"))

dropDownSel <- delFrame %>%
  filter(MainSleeve == 1,
         !is.na(StartDate)) %>%
  mutate(Name = paste(AssetClass, Region, Style)) %>%
  arrange(AssetClass) %>%
  select(Name, DelCode)
  
rm(holdSet)

factMap <- data.frame(Factors = c("Factor", "NonFactor", 
                                  "Equity","FixedIncome","Commodity", "Currency", "Alt", 
                                  "Spread", "YC",
                                  "Style", "Country", "Industry", "Greeks"),
                      Group = c(rep("A-Main", 2), rep("B-AssetClass",5), rep("C-FixedIncome",2), 
                                rep("C-Equity",4)),
                      stringsAsFactors = F) 

### RATINGs ###################################
dataSet <- dataSet %>%
  mutate(RatingPort = as.character(RatingPort),
         RatingBench = as.character(RatingBench)) %>%
  mutate(Rating = case_when(is.na(RatingPort) & is.na(RatingBench) ~ "NR",
                            is.na(RatingPort) ~ RatingBench, 
                            is.na(RatingBench) ~ RatingPort),
         Rating = gsub("\\+|-", "", Rating)) %>%
  select(-c(RatingPort, RatingBench))

dataSet <- dataSet %>%
  mutate(RatingSandP = case_when(is.na(SPPort) ~ gsub("u|\\(P\\)|e|p|\\+|-", "", SPBench),
                                 TRUE ~ gsub("u|\\(P\\)|e|p|\\+|-", "", SPPort)),
         RatingSandP = ifelse(RatingSandP == "NR", NA , RatingSandP),
         RatingSandP = case_when(RatingSandP == "A1" ~ "AAA",
                                 RatingSandP == "A2" ~ "A",
                                 RatingSandP == "A3" ~ "BBB",
                                 RatingSandP == "R" ~ "CCC",
                                 RatingSandP == "SD" ~ "D",
                                 TRUE ~ RatingSandP),
         RatingMoodys = case_when(is.na(MoodysPort) ~ gsub("u|\\(P\\)|e|p|[1,2,3]", "", MoodysBench),
                                  TRUE ~ gsub("u|\\(P\\)|e|p|[1,2,3]", "", MoodysPort)),
         RatingMoodys = ifelse(RatingMoodys == "NR", NA, RatingMoodys),
         RatingMoodys = toupper(RatingMoodys),
         RatingMoodys = case_when(RatingMoodys == "BAA" ~ "BBB",
                                  RatingMoodys == "BA" ~ "BB",
                                  RatingMoodys == "CAA" ~ "CCC",
                                  RatingMoodys == "CA" ~ "CC",
                                  RatingMoodys == "WR" ~ NA_character_,
                                  TRUE ~ RatingMoodys)) %>%
  mutate(RatingSource = case_when(#Rating == "NR" & is.na(RatingSandP) ~ "Moodys",
    Rating == "NR" ~ "SandP",
    TRUE ~ "AverageBBG")) %>%
  mutate(Rating = case_when(#Rating == "NR" & is.na(RatingSandP) ~ RatingMoodys,
    Rating == "NR" ~ RatingSandP,
    TRUE ~ Rating)) %>%
  mutate(Rating = ifelse(is.na(Rating), "NR", Rating)) %>%
  select(-c(RatingSandP, RatingMoodys))

### RATING FROM SP LT COUNTRY RATINGS
rateCtryMap <- rateCtryMap %>%
  rename(CntryRisk = COUNTRY_ISO,
         SPCtryRate = RTG_SP_LT_LC_ISSUER_CREDIT) %>%
  mutate(SPCtryRate = gsub("u|\\(P\\)|e|p|\\+|-", "", SPCtryRate))

dataSet <- dataSet %>%
  left_join(rateCtryMap[, c("CntryRisk", "SPCtryRate")], by = "CntryRisk") %>%
  mutate(RatingSource = ifelse(Rating == "NR" & 
                                 Issuer %in% c("Government"),
                               "CntryRating", RatingSource),
         Rating = ifelse(Rating == "NR" & 
                           Issuer %in% c("Government"),
                         SPCtryRate, Rating)) %>%
  select(-SPCtryRate) %>%
  mutate(RatingGrp = case_when(Rating %in% c("AAA", "AA", "A", "BBB") ~ "IG",
                               Rating %in% c("BB", "B", "CCC", "CC", "C", "D") ~ "HY",
                               TRUE ~ "NR"))

### SCENARIOS ##############################
scenSet <- scenSet %>%
  left_join(scenIDs, by = c("ScenID"="ID")) %>%
  mutate(scenName = gsub(" with propagation", "", scenName))
  
