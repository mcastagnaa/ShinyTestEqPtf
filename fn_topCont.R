writeLines("Loading fn_topCont.R")

fn_topCont <- function(delCode, date) {
  
  thisDataSet <- dataSet %>%
    filter(Delegate == delCode,
           ReportDate == date) %>%
    group_by_at(c("ID059", "Name")) %>%
    summarise(WgtPort = sum(WgtPort, na.rm = T),
              WgtBench = sum(WgtBench, na.rm = T),
              ContributionDiff = sum(ContributionDiff, na.rm = T)) %>%
    ungroup() %>%
    select(Name, WgtPort, WgtBench, TEContrib = ContributionDiff)
  
  rbind(thisDataSet %>%
          arrange(desc(TEContrib)) %>%
          top_n(5, TEContrib) %>%
          mutate(Tag = "Top contributors"),
        thisDataSet %>%
          arrange(TEContrib) %>%
          top_n(5, -TEContrib) %>%
          mutate(Tag = "Top detractors")
        )
}


