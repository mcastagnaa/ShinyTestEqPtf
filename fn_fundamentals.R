writeLines("Loading fn_fundamentals.R")

#delCode = 	693423

fn_fundamentals <- function(delCode) {
  chartTopData <- topSet %>%
    filter(Delegate == delCode,
           ReportDate > as.Date("2020-03-27")) %>%
    select(OADPort, OADBench, LOASPort, LOASBench, DTSPort, DTSBench,LOASDPort, LOASDPort,
           YearstoMatPort, YearstoMatBench, YTWPort,YTWBench, 
           PEPort, PEBench, DivYldPort, DivYldBench, BEstPEPort, BEstPEBench,
           BEstLTGEPSPort, BEstLTGEPSBench, PBPort, PBBench, 
           ReportDate) %>%
    pivot_longer(-ReportDate) %>%
    filter(value != 0) %>%
    mutate(Stat = gsub("Port|Bench", "", name),
           object = case_when(grepl("Port", name) ~ "Port",
                              grepl("Bench", name) ~ "Bench")) 
  
  chartTop <- ggplot(chartTopData, aes(x = ReportDate, y = value, color = object)) +
    geom_line() +
    geom_text(data = chartTopData[chartTopData$ReportDate == max(chartTopData$ReportDate),],
              aes(label = value), vjust = -0.3, size = 2) +
    facet_wrap(~Stat, scales = "free_y") +
    ggsci::scale_color_npg() +
    #scale_y_continuous(label = scales::percent) +
    theme_bw() +
    labs(x = "", y = "")
  
    return(chartTop)
}
