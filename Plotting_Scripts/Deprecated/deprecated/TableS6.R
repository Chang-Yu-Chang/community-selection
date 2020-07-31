#' Table S6: Ecoprospector parameters
library(tidyverse)
library(data.table)
library(flextable)
library(officer)


t6 <- c(
    
    "theta", "The percentile determining the high-performing species in the species pool used to knock in", 0.95,
    "d_bottleneck", "Bottleneck size", 10^5, 
    "n_mig", "Number of cells in the migrant community", 10^6, 
    "f_coalescence", "Mixing ratio of coalescence; biomass of immigrant community relative to that of a perturbed community copy", 0.5,
    "r_percent", "Tunes the magnitude of resource perturbation. The fraction from depleting a resource and move the same amount to another", 1
) %>%
    matrix(ncol = 3, byrow = T) %>%
    as_tibble() %>%
    setNames(c("Parameter",	"Description", "Value"))

fwrite(t6, "../data/TableS6.csv")

ft6 <- flextable(t6) %>% 
    width(j = 2, width = 5) %>%
    align(j = 1:3, align = "left", part = "all") %>% 
    compose(i = 1, j = "Parameter", value = as_paragraph("θ")) %>% 
    compose(i = 2, j = "Parameter", value = as_paragraph("d", as_sub("bot"))) %>% 
    compose(i = 3, j = "Parameter", value = as_paragraph("n", as_sub("mig"))) %>% 
    compose(i = 4, j = "Parameter", value = as_paragraph("f", as_sub("coa"))) %>% 
    compose(i = 5, j = "Parameter", value = as_paragraph("r", as_sub("percent"))) %>% 
    valign(j = 1:3, valign = "top", part = "all") %>%
    {.}

save_as_image(ft6, "../Plots/TableS6.png")


#"s_mig", "Number of species in the migrant community; if specified, make synthetic community with that number of species", NA,
#"r_type", "Mode of resource shift: rescale_add, rescale_remove, add, remove, old", "add",
