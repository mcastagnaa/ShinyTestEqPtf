writeLines("loading fn_actRiskSplit.R")

delCode = 701878
date = as.Date("2023-03-29")
split = "GICSSectorName"

fn_actRiskSplit <- function(delCode, date, split) {
  
  thisDataSet <- dataSet %>%
    filter(Delegate == delCode,
           ReportDate == date) %>%
    mutate(WgtPort = ifelse(IsDerivative & split == "Crncy", 0, WgtPort)) %>%
    group_by_at(c("ID059", "Name", split)) %>%
    summarise(WgtPort = sum(WgtPort, na.rm = T),
              WgtBench = sum(WgtBench, na.rm = T),
              ContributionDiff = sum(ContributionDiff, na.rm = T)) %>%
    ungroup()
  
  chartSetACS <- thisDataSet %>%
    group_by_at(split) %>%
    summarise(`A-Wgt` = sum(WgtPort, na.rm = T),
              `B-ActiveWeight` = sum(WgtPort-WgtBench, na.rm = T),
              `C-RelativeRiskCtb` = sum(ContributionDiff, na.rm = T)) %>%
    pivot_longer(-all_of(split), names_to = "Dimension") %>%
    mutate(value = value/100)  %>%
    filter(value != 0.00)
  
  naSet <- thisDataSet %>%
    filter(is.na(get(split))) %>%
    select(Name, WgtPort, WgtBench, ActRiskCtb = ContributionDiff) %>%
    mutate(ActRiskCtb = round(ActRiskCtb,2))
  
  chartACS <- chartSetACS %>%
    ggplot(aes_string(x = as.character(split), y = "value")) +
    geom_bar(stat = "identity", position = "dodge", fill = "light green") +
    geom_text(aes(label= paste0(round(value*100, 1), "%")), size = 2.5) + #, position=position_dodge(width=1), vjust=-0.25)+
    facet_wrap(~Dimension, nrow = 1) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(title = paste("Split by", split, "for", date),
         x = "", y = "")
  
  return(list(chartACS, naSet))
}

## possible groupings: Crncy, GICSSectorName, GICSIndustryName, Sector, Industry, 
##                     SecurityType, MktCap, Developed/EM, CtryName, Region 