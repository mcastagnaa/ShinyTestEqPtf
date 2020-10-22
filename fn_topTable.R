writeLines("loading fn_topTable.R")

delCode = 697132
date = as.Date("2020-10-20")

fn_topTable <- function(delCode, date) {
  topSet %>%
    filter(Delegate == delCode, 
           ReportDate == date) %>%
    select(DelCodePort = Delegate,
           MktValPort = MtkValPort,
           WgtPort, WgtBench, 
           DeltaAdjWgtPort, DeltaAdjWgtBench,
           TotalRiskPort, TotalRiskBench, TotalRiskDiff,
           VaRMCPort, VaRMCBench, VaRMCDiff) %>%
    pivot_longer(everything()) %>%
    mutate(Object = case_when(grepl("Bench", name) ~ "SAA",
                              grepl("Diff", name) ~ "Diff.",
                              TRUE ~ "Portf."),
           Object = factor(Object, levels = c("Portf.", "SAA", "Diff.")),
           name = gsub("Diff|Port|Bench", "", name),
           value = ifelse(name %in% c("VaRMC", "TotalRisk"), value * sqrt(252), value),
           value = round(value, 2)) %>%
    pivot_wider(names_from = Object, values_from = value) %>%
    rename(Metric = name) %>%
    as.data.frame()
}

