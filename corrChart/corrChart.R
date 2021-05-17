library(tidyquant)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(partition)

rm(list = ls())

# assets <- read.xlsx("./corrChart/findYahooTickers.xlsx") %>%
#   mutate(TICKER = ifelse(TICKER == "ABX", "GOLD", TICKER))
# 
# stockPrices <- assets$TICKER %>%
#   tq_get(get  = "stock.prices",
#          from = "2019-03-12",
#          to   = "2021-03-12")
# 
# Bmk <- "VYM" %>%
#   tq_get(get  = "stock.prices",
#          from = "2019-03-12",
#          to   = "2021-03-12")
# 
# save(stockPrices, Bmk, file = "./corrChart/Yahoo.Rda")
load("./corrChart/Yahoo.Rda")

PortRets <- stockPrices %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               col_rename = "Ra")

BmkRets <- Bmk %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               col_rename = "Rb")

dataSet <- PortRets %>%
  left_join(BmkRets, by = "date") %>%
  mutate(Rret = Ra-Rb)

wide <- dataSet %>%
  select(date, symbol, Rret) %>%
  pivot_wider(id_cols = "date", names_from = "symbol", values_from = "Rret") %>%
  select(-date)



ggcorrplot(round(cor(wide),2), 
           hc.order = TRUE, 
           outline.color = "white",
           lab = T,
           tl.cex = 4, lab_size = 1,
           p.mat = cor_pmat(wide))

ggsave("./www/corrPlotEx.png", device = "png", units = "cm", scale = 1)
