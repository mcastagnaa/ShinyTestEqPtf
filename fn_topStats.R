writeLines("Loading fn_topStats.R")

#delCode = 701878

fn_topStats <- function(delCode) {
  actExp <- dataSet %>%
    filter(Delegate == delCode) %>%
    mutate(actExp = abs(WgtPort-WgtBench)/2) %>%
    group_by(ReportDate) %>%
    summarise(value = sum(actExp, na.rm = T)/100) %>%
    mutate(name = "ActiveExp")
  
  chartTopData <- topSet %>%
    filter(Delegate == delCode,
           ReportDate > as.Date("2020-03-27")) %>%
    select(VaRMCPort, VaRMCBench, TotalRiskPort, TotalRiskBench, TotalRiskDiff, ReportDate) %>%
    mutate_at(vars(starts_with("VaR"),starts_with("Total")), ~./100*sqrt(252)) %>%
    pivot_longer(-ReportDate) %>%
    bind_rows(actExp) 
  
  chartTop <- ggplot(chartTopData, aes(x = ReportDate, y = value)) +
    geom_line() +
    geom_text(data = chartTopData[chartTopData$ReportDate == max(chartTopData$ReportDate),],
              aes(label = paste0(round(value*100, 1), "%")), vjust = -0.3, size = 2) +
    facet_wrap(~name) +
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
    ggplot(aes(x = ReportDate, y = value, fill = Factors)) +
    geom_area(stat = "identity") +
    facet_wrap(~Group, nrow = 1) +
    theme_bw() +
    scale_y_continuous(label = scales::percent) +
    labs(x = "", y = "")
    
    return(list(chartTop, chartFac))
}

