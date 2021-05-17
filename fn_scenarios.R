writeLines("Loading fn_scenarios.R")

delCode = 	693423
repDate = '2020-11-19'

fn_scen <- function(delCode, repDate) {
  chartData <- scenSet %>%
    filter(delegate == delCode,
           reportDate == repDate,
           ScenID != "S_W") %>%
    select(object, returns, scenName) %>%
    pivot_wider(id_cols = "scenName", names_from = "object", values_from = "returns") %>%
    mutate(RR = Ptfl-SAA) %>%
    pivot_longer(-scenName, names_to = "Object") %>%
    mutate(facet = ifelse(Object %in% c("Ptfl", "SAA"), "Absolute", "Relative"))
  
  scenChart <- chartData %>%
    ggplot(aes(x = scenName, y = value, fill = Object)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(data = chartData,
              aes(label = round(value*100, 2)), 
              position = position_dodge(width = 1),
              vjust = 0, hjust = 0, size = 2.5,
              color = "dark grey") +
    facet_wrap(~facet, scales = "free_x") +
    viridis::scale_fill_viridis(discrete = T) +
    scale_y_continuous(label = scales::percent) +
    theme_bw() +
    coord_flip() +
    labs(x = "", y = "")
  
  scenDes <- scenIDs %>%
    filter(ID != "S_W",
           !is.na(Definition)) %>%
    select(Name = scenName, Definition) %>%
    distinct()
  
    return(list(scenChart, scenDes))
}
