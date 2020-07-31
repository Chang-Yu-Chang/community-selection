#' Table S4: Protocol-specific parameters
library(tidyverse)
library(data.table)
library(flextable)
library(officer)

t4 <- c(
    "d", "Dilution factor in the batch culture", 0.001,
    "t", "Incubation time", 1, 
    "n_wells", "Number of wells; number of metacommunities", 96,
    "T_tot", "Number of total transfers (generations)", 40,
    "T_selc", "Number of selection transfers (generations)", 20
) %>%
    matrix(ncol = 3, byrow = T) %>%
    as_tibble() %>%
    setNames(c("Parameter",	"Description and units", "Value"))

fwrite(t4, "../data/TableS4.csv")

ft4 <- flextable(t4) %>% 
    width(j = 2, width = 5) %>%
    align(j = 1:3, align = "left", part = "all") %>% 
    compose(i = 3, j = "Parameter", value = as_paragraph("n", as_sub("wells"))) %>% 
    compose(i = 4, j = "Parameter", value = as_paragraph("T", as_sub("tot"))) %>% 
    compose(i = 5, j = "Parameter", value = as_paragraph("T", as_sub("selc"))) %>% 
    colformat_num(j = 3, i = 1, big.mark=",", digits = 3, na_str = "N/A") %>%
    valign(j = 1:3, valign = "top", part = "all") %>%
    {.}

save_as_image(ft4, "../Plots/TableS4.png")
