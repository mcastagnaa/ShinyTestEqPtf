writeLines("loading fn_actRiskByRegion.R")

# delCode = 701878
# date = as.Date("2020-10-08")

fn_actRiskByRegion <- function(delCode, date) {
  dataSet %>%
    filter(Delegate == delCode,
           ReportDate == date) %>%
    group_by(Region) %>%
    summarise(Wgt = sum(WgtPort, na.rm = T),
              actWgt = sum(WgtPort-WgtBench, na.rm = T),
              actRiskCtb = sum(ContributionDiff, na.rm = T)) %>%
    pivot_longer(-Region, names_to = "Dimension") %>%
    mutate(value = value/100) %>%
    ggplot(aes(x = Region, y = value)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Dimension, nrow = 1) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(main = "Regional split",
         x = "", y = "")
}