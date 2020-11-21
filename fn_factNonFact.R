writeLines("loading fn_factNonFact.R")

# split = "Sector"
# delCode = "693423"
# repDate = "2020-11-19"

fn_factNonFact <- function(delCode, date, split) {

    dataSet %>%
    filter(Delegate == delCode,
           ReportDate == date) %>%
    group_by_at(split) %>%
    summarise(Fact = sum(FactorContribDiff),
              NonFact = sum(NonFactorContribDiff)) %>%
    pivot_longer(-all_of(split), names_to = "TEContrib") %>%
    ggplot(aes_string(x = split, y = "value/100", fill = "TEContrib")) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(value, 2)), 
              position = position_dodge(width = 1),
              vjust = 0, hjust = 0, size = 2.5,
              color = "dark grey") +
    viridis::scale_fill_viridis(discrete = T) +
    scale_y_continuous(label = scales::percent) +
    theme_bw() +
    coord_flip() +
    labs(x = "", y = "")

  }



