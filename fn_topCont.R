writeLines("Loading fn_topCont.R")

fn_topCont <- function(delCode, date) {
  rbind(dataSet %>%
          filter(Delegate == delCode,
                 ReportDate == date) %>%
          select(Name, WgtPort, WgtBench, TEContrib = ContributionDiff) %>%
          arrange(desc(TEContrib)) %>%
          top_n(5, TEContrib) %>%
          mutate(Tag = "Top contributors"),
        dataSet %>%
          filter(Delegate == delCode,
                 ReportDate == date) %>%
          select(Name, WgtPort, WgtBench, TEContrib = ContributionDiff) %>%
          arrange(TEContrib) %>%
          top_n(5, -TEContrib) %>%
          mutate(Tag = "Top detractors")
        )
}


