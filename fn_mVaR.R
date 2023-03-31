writeLines("loading fn_mVaR.R")

# delCode = 701878
# date = as.Date("2020-10-08")

fn_mVaR <- function(delCode, date) {
  dataSet %>%
    filter(Delegate == delCode,
           ReportDate == date) %>%
    group_by_at(c("Name")) %>%
    summarise(WgtPort = sum(WgtPort, na.rm = T),
              WgtBench = sum(WgtBench, na.rm = T),
              MarginalVaRMCPort = sum(MarginalVaRMCPort, na.rm = T),
              PartialVaRMCPort = sum(PartialVaRMCPort, na.rm = T)) %>%
    ungroup() %>%
    filter(WgtPort != 0) %>%
    select(Name, WgtPort, WgtBench, MarginalVaRMCPort, PartialVaRMCPort) %>%
    arrange(desc(MarginalVaRMCPort))
  
}
  