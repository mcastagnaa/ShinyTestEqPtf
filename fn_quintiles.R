writeLines("Loading fn_quintiles.R")

delCode = 	693423
date = '2020-11-06'
var = "DivYld"

fn_quintiles <- function(delCode, date) {
  if(exists("chartSet")) rm(chartSet)
  for(var in c("OAD", "LOAS", "DTS", "LOASD", "YearstoMat", "YTW",
               "PE", "DivYld", "BEstPE", "BEstLTGEPS", "PB")) {
    
    split <- tryCatch(
      {
        dataSet %>%
          filter(Delegate == delCode,
                 ReportDate == date) %>%
          select(Name, WgtPort, WgtBench, ContributionDiff, paste0(var, "Bench"), paste0(var, "Port")) %>%
          mutate(variable = ifelse(.[[5]] == 0, .[[6]], .[[5]]),
                 Quintile = cut(variable, 
                                breaks = unique(quantile(variable, probs=seq(0, 1, by= 0.2), na.rm = T)), 
                                include.lowest = TRUE))
    },
    error = function(Err) {return(NULL)})
    
    if(!is.null(split)) {
      split <- split %>%
        group_by(Quintile) %>%
        summarise(WgtPort = sum(WgtPort, na.rm = T)/100,
                  WgtBench = sum(WgtBench, na.rm =T)/100,
                  TEcontrib = sum(ContributionDiff, na.rm = T)/100,
                  Variable = var)
      
      if(exists("chartSet")) chartSet <- bind_rows(chartSet, split) else chartSet <- split
    }
  }
  
  chartQ <- chartSet %>%
    pivot_longer(-c(Variable, Quintile), names_to = "Stat") %>%
    ggplot(aes(x = Quintile, y = value, fill = Stat)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Variable, nrow = 2, scales = "free") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    viridis::scale_fill_viridis(discrete = T) +
    labs(x = "", y = "") +
    coord_flip()
  
  return(chartQ)
}
