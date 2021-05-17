writeLines("loading fn_Rets.R")

#delCode = 701880
#startDate = as.Date("2021-2-28")
#endDate = as.Date("2021-04-10")

fn_Rets <- function(delCode, startDate, endDate) {
  
  chartData <- retsSet %>%
    mutate(Date = as.Date(Date)) %>%
    filter(DelCode == delCode,
           Date >= startDate,
           Date <= endDate) %>%
    select(-DelCode) %>%
    rename(Bench = SAA) %>%
    arrange(Date) %>%
    mutate(GAV = GAV/first(GAV) * 100,
           Bench = Bench/first(Bench) * 100,
           Relative = GAV-Bench) %>%
    pivot_longer(-Date, names_to = "Object") %>%
    mutate(panel = ifelse(Object == "Relative", Object, "Absolute"))
    
    
    RetChart <- chartData %>%
      ggplot(aes(x = Date, y = value, color = Object)) +
      geom_line() +
      geom_text(data = chartData[chartData$Date == max(chartData$Date),],
                aes(label = round(value, 2)), vjust = -0.3, size = 2.5, color = "dark grey") +
      facet_wrap(~panel, ncol = 1, scales = "free_y") +
      #geom_hline(yintercept = 0, color = "black")+
      viridis::scale_color_viridis(discrete = T) +
      #scale_y_continuous(label = scales::percent) +
      theme_bw() +
      labs(title = paste("From", format(min(chartData$Date), "%d-%h-%y to"), format(max(chartData$Date),  "%d-%h-%y")),
           x = "", 
           y = "")
  
  return(RetChart)
}
