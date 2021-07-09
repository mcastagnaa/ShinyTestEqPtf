writeLines("loading fn_MktVal.R")

#delCode = 701880

fn_mktVal <- function(delCode) {
  
    mktVal_H <- topSet %>%
      filter(Delegate == delCode) %>%
      mutate(MktVal = round(MtkValPort/1000000,2)) %>%
      ggplot(aes(x = ReportDate, y = MktVal)) +
      geom_line(color = "Blue") +
      theme_bw() +
      labs(title = "Sleeve AUM (EUR mn)",
           x = "", y = "")
    
  return(mktVal_H)
}
