writeLines("Loading fn_factors.R")

# delCode = 	693423
# repDate = '2020-11-19'

fn_factors <- function(delCode, repDate) {
  factSet %>%
    filter(delegate == delCode,
           reportDate == repDate) %>%
    select(FactorName, PtflBeta, SAABeta, ActiveBeta, FactorVol, TEContrib = Contribution) %>%
    arrange(desc(abs(TEContrib)))
}
