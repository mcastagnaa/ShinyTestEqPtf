writeLines("loading fn_HstRiskSplit.R")

delCode = 701880
split = "GICSSectorName"

fn_HstRiskSplit <- function(delCode, split) {
  
  chartSetACS <- dataSet %>%
    filter(Delegate == delCode) %>%
    mutate(WgtPort = ifelse(IsDerivative & split == "Crncy", 0, WgtPort)) %>%
    group_by_at(c(split, "ReportDate")) %>%
    summarise(`A-Wgt` = sum(WgtPort, na.rm = T),
              `B-ActiveWeight` = sum(WgtPort-WgtBench, na.rm = T),
              `C-RelativeRiskCtb` = sum(ContributionDiff, na.rm = T)) %>%
    pivot_longer(-c(all_of(split), ReportDate), names_to = "Dimension") %>%
    mutate(value = value/100)  %>%
    filter(value != 0.00)
  
  chartACS_H <- chartSetACS %>%
    ggplot(aes_string(x = "ReportDate", y = "value", fill = as.character(split))) +
    geom_bar(stat = "identity") +
    facet_wrap(~Dimension, nrow = 1) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    viridis::scale_fill_viridis(discrete = T) +
    labs(title = paste("Split by", split),
         x = "", y = "")
  
  return(chartACS_H)
}

## possible groupings: Crncy, GICSSectorName, GICSIndustryName, Sector, Industry, 
##                     SecurityType, MktCap, Developed/EM, CtryName, Region 