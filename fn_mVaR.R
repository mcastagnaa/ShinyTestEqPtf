writeLines("loading fn_mVaR.R")

# delCode = 701878
# date = as.Date("2020-10-08")

fn_mVaR <- function(delCode, date) {
  dataSet %>%
    filter(Delegate == delCode,
           ReportDate == date, 
           WgtPort != 0) %>%
    select(Name, WgtPort, WgtBench, MarginalVaRMCPort, PartialVaRMCPort) %>%
    arrange(desc(MarginalVaRMCPort))
}
  