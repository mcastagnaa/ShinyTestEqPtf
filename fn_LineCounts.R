writeLines("loading fn_LineCounts.R")

delCode = 701880

fn_LineCounts <- function(delCode) {
  
    LinesCount <- dataSet %>%
      filter(Delegate == delCode) %>%
      group_by(ReportDate) %>%
      summarise(Port = sum(WgtPort!=0, na.rm = T),
                SAA = sum(WgtBench!=0, na.rm = T)) %>%
      pivot_longer(-ReportDate, names_to = "Object", values_to = "Count") %>%
      ggplot(aes(x = ReportDate, y = Count, color = Object)) +
      geom_line() +
      facet_wrap(~Object, scale = "free_y", nrow = 1) +
      theme_bw() +
      theme(legend.position = "none") +
      scale_color_viridis_d() +
      labs(title = "Number of holdings",
           x = "", y = "")
    
  return(LinesCount)
}
