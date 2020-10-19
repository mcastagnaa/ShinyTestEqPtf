writeLines("loading fn_actRiskByRegion.R")

# delCode = 697132
# date = as.Date("2020-10-15")
# split = "Crncy"

fn_actRiskSplit <- function(delCode, date, split) {
  
  dataSet %>%
    filter(Delegate == delCode,
           ReportDate == date) %>%
    mutate(WgtPort = ifelse(IsDerivative & split == "Crncy", 0, WgtPort)) %>%
    group_by_at(split) %>%
    summarise(Wgt = sum(WgtPort, na.rm = T),
              actWgt = sum(WgtPort-WgtBench, na.rm = T),
              actRiskCtb = sum(ContributionDiff, na.rm = T)) %>%
    pivot_longer(-all_of(split), names_to = "Dimension") %>%
    mutate(value = value/100)  %>%
    filter(value != 0.00) %>%
    ggplot(aes_string(x = as.character(split), y = "value")) +
    geom_bar(stat = "identity", position = "dodge", fill = "light green") +
    geom_text(aes(label= paste0(round(value*100, 1), "%")), size = 2.5) + #, position=position_dodge(width=1), vjust=-0.25)+
    facet_wrap(~Dimension, nrow = 1) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(title = paste("Split by", split, "for", date),
         x = "", y = "")
}

## possible groupings: Crncy, GICSSectorName, GICSIndustryName, Sector, Industry, 
##                     SecurityType, MktCap, Developed/EM, CtryName, Region 