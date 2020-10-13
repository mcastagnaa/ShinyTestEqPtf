writeLines("loading fn_actRiskByRegion.R")

# delCode = 701878
# date = as.Date("2020-10-08")
# split = "Region"

fn_actRiskSplit <- function(delCode, date, split) {
  dataSet %>%
    filter(Delegate == delCode,
           ReportDate == date) %>%
    group_by_at(split) %>%
    summarise(Wgt = sum(WgtPort, na.rm = T),
              actWgt = sum(WgtPort-WgtBench, na.rm = T),
              actRiskCtb = sum(ContributionDiff, na.rm = T)) %>%
    pivot_longer(-all_of(split), names_to = "Dimension") %>%
    mutate(value = value/100) %>%
    ggplot(aes_string(x = split, y = "value")) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Dimension, nrow = 1) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(title = paste("Split by", split),
         x = "", y = "")
}

## possible groupings: Crncy, GICSSectorName, GICSIndustryName, Sector, Industry, 
##                     SecurityType, MktCap, Developed/EM, CtryName, Region 