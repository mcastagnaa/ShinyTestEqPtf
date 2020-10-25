writeLines("Loading fn_topStats.R")

delCode = 	693423

fn_topStats <- function(delCode) {
  actExp <- dataSet %>%
    filter(Delegate == delCode) %>%
    mutate(actExp = abs(WgtPort-WgtBench)/2) %>%
    group_by(ReportDate) %>%
    summarise(value = sum(actExp, na.rm = T)/100) %>%
    mutate(name = "ActiveExpDiff", 
           group = "ActiveExp",
           object = "Diff")
  
  chartTopData <- topSet %>%
    filter(Delegate == delCode,
           ReportDate > as.Date("2020-03-27")) %>%
    select(VaRMCPort, VaRMCBench, TotalRiskPort, TotalRiskBench, TotalRiskDiff, ReportDate) %>%
    mutate_at(vars(starts_with("VaR"),starts_with("Total")), ~./100*sqrt(252)) %>%
    pivot_longer(-ReportDate) %>%
    mutate(group = ifelse(grepl("Diff", name),
                          name,
                          gsub("Port|Bench", "", name)),
           object = case_when(grepl("Diff", name) ~ "Diff",
                              grepl("Port", name) ~ "Port",
                              grepl("Bench", name) ~ "Bench")) %>%
    bind_rows(actExp) %>%
    mutate(group = recode(group,
                          ActiveExp = "A-Active weight",
                          TotalRisk = "B-Risk",
                          TotalRiskDiff = "C-Expected TE",
                          VaRMC = "D-VaR"))
  
  
  chartTop <- ggplot(chartTopData, aes(x = ReportDate, y = value, color = object)) +
    geom_line() +
    geom_text(data = chartTopData[chartTopData$ReportDate == max(chartTopData$ReportDate),],
              aes(label = paste0(round(value*100, 1), "%")), vjust = -0.3, size = 2.5) +
    facet_wrap(~group,scales = "free_y") +
    viridis::scale_color_viridis(discrete = T) +
    scale_y_continuous(label = scales::percent) +
    theme_bw() +
    labs(x = "", y = "")
  
  chartFac <- topSet %>%
    filter(Delegate == delCode) %>%
    select(NonFactor = NonFactorContribDiff,
           Factor = FactorContribDiff,
           CommodityDiff, CurrencyDiff, AltDiff = AlternativeDiff, 
           SpreadDiff, YCDiff= YieldCurveDiff,
           FixedIncomeDiff, EquityDiff,
           CountryDiff, IndustryDiff, StyleDiff, GreeksDiff, ReportDate) %>%
    pivot_longer(-ReportDate, names_to = "Factors") %>%
    mutate(Factors = gsub("Diff", "", Factors),
           value = value/100) %>%
    filter(value != 0) %>%
    left_join(factMap, by = "Factors") %>%
    mutate(Factors = factor(Factors, levels= rev(c("Factor", "NonFactor", 
                                               "Equity","FixedIncome","Commodity", "Currency", "Alt", 
                                               "Spread", "YC",
                                               "Style", "Country", "Industry", "Greeks")))) %>%
    ggplot(aes(x = ReportDate, y = value, fill = Factors)) +
    geom_area(stat = "identity") +
    facet_wrap(~Group, nrow = 1) +
    viridis::scale_fill_viridis(discrete = T) +
    theme_bw() +
    scale_y_continuous(label = scales::percent) +
    labs(x = "", y = "")
    
    return(list(chartTop, chartFac))
}

topSet$Factors