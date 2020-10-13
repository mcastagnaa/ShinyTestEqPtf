writeLines("Running datamanagement.R")

load("datadump.Rda")

dataSet <- holdSet %>%
  mutate(CntryRisk = ifelse(is.na(CntryRisk)|CntryRisk=="", Cntry, CntryRisk),
         MktCap = ifelse(MarketCapPort == 0, MarketCapBench, MarketCapPort),
         MktCap = cut(MktCap, breaks = c(0, 1500, 8500, 40000, +Inf), labels = c("Small", "Mid", "Big", "Mega"))) %>%
  left_join(geoMap, by = c("CntryRisk" = "ISO2")) %>%
  left_join(secTypeMap, by = "SecurityType") %>%
  select(-starts_with("update"))

rm(holdSet)

factMap <- data.frame(Factors = c("Factor", "NonFactor", 
                                  "Equity","FixedIncome","Commodity", "Currency", "Alt", 
                                  "Spread", "YC",
                                  "Style", "Country", "Industry", "Greeks"),
                      Group = c(rep("A-Main", 2), rep("B-AssetClass",5), rep("C-FixedIncome",2), 
                                rep("C-Equity",4)),
                      stringsAsFactors = T)

