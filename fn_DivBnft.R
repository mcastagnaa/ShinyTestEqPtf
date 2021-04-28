writeLines("loading fn_DivBnft.R")

#delCode = 701880

fn_DivBnft <- function(delCode) {
  
  PortfData <- dataSet %>%
    filter(Delegate == delCode) %>%
    group_by(ReportDate) %>%
    summarise(Port = sum(TotalRiskPort, na.rm = T),
              SAA = sum(TotalRiskBench, na.rm = T)) %>%
    pivot_longer(-ReportDate, names_to = "Object", values_to = "SumUndiversified")
  
  TopData <- topSet %>%
    filter(Delegate == delCode) %>%
    select(ReportDate, Port = TotalRiskPort, SAA = TotalRiskBench) %>%
    pivot_longer(-ReportDate, names_to = "Object", values_to = "Diversified")
  
  DivBnft <- PortfData %>%
    left_join(TopData, by = c("ReportDate", "Object")) %>%
    mutate(DivB = 1 - Diversified/SumUndiversified) %>%
    ggplot(aes(x = ReportDate, y = DivB, color = Object)) +
    geom_line() +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_color_viridis_d() +
    labs(title = "Diversification benefits",
         x = "", y = "")
  rm(TopData, PortfData)
  
  return(DivBnft)
}
