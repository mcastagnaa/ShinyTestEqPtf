writeLines("Running datamanagement.R")

load("datadump.Rda")

dataSet <- holdSet %>%
  mutate(CntryRisk = ifelse(is.na(CntryRisk)|CntryRisk=="", Cntry, CntryRisk)) %>%
  left_join(geoMap, by = c("CntryRisk" = "ISO2")) %>%
  left_join(secTypeMap, by = "SecurityType") %>%
  select(-starts_with("update"))

rm(holdSet)

factMap <- data.frame(Factors = c("NonFactor", "Commodity", "Currency", "Alt", "Spread", "YC",
                                  "Country", "Industry", "Style", "Greeks"),
                      Group = c("NonFactor", "Commodity", "Currency", "Alt", "FixedIncome", "FixedIncome", 
                                "Equity", "Equity", "Equity", "Equity"),
                      stringsAsFactors = F)

