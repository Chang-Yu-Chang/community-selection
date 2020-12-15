#' Table S2 : Parameters and units for the Microbial Consumer Resource Model
library(tidyverse)
library(data.table)
library(flextable)
library(officer)

t2 <- c(
    "Ni", "population density of species i (individuals/volume)", NA,
    "Rα", "Concentration of resource α (mass/volume)", NA,
    "Ciα", "Uptake rate per unit concentration of resource α by species i (volume/time)", NA,
    "Dαβ", "Fraction of byproducts from resource β converted to α (unitless)", NA,
    "gi", "Conversion factor from energy uptake to growth rate (1/energy)", 1,
    "wα", "Energy content of resource α (energy/mass)", 1,
    "lα", "Leakage fraction for resource α (unitless)", 0,
    "mi", "Minimal energy uptake for maintenance of species i (energy/time)", 0,
    "σα", "Functional response of utilization on resource α", NA,
    "n", "Hill coefficient for functional response (unitless)", 2,
    "σmax", "Maximum input flux (mass/time)", 1
) %>%
    matrix(ncol = 3, byrow = T) %>%
    as_tibble() %>%
    setNames(c("Parameter",	"Description and units", "Value"))

fwrite(t2, "../Data/Tables/TableS2.csv")

ft2 <- flextable(t2) %>% 
    width(j = 2, width = 5) %>% 
    valign(j = 1:3, valign = "top", part = "all") %>%
    align(j = 1:3, align = "left", part = "all") %>% 
    compose(i = 1:4, j = "Value", value = as_paragraph("")) %>% 
    compose(i = 1, j = "Parameter", value = as_paragraph("N", as_sub("i"))) %>% 
    compose(i = 2, j = "Parameter", value = as_paragraph("R", as_sub("α"))) %>% 
    compose(i = 3, j = "Parameter", value = as_paragraph("C", as_sub("iα"))) %>% 
    compose(i = 4, j = "Parameter", value = as_paragraph("D", as_sub("αβ"))) %>% 
    compose(i = 5, j = "Parameter", value = as_paragraph("g", as_sub("i"))) %>% 
    compose(i = 6, j = "Parameter", value = as_paragraph("w", as_sub("α"))) %>% 
    compose(i = 7, j = "Parameter", value = as_paragraph("l", as_sub("α"))) %>% 
    compose(i = 8, j = "Parameter", value = as_paragraph("m", as_sub("i"))) %>% 
    compose(i = 9, j = "Parameter", value = as_paragraph("σ", as_sub("α"))) %>%
    compose(i = 10, j = "Parameter", value = as_paragraph("n", as_sub(""))) %>% 
    compose(i = 11, j = "Parameter", value = as_paragraph("σ", as_sub("max"))) %>% 
    # Footnote
    footnote(i = 1:2, j = "Value", part = "body",
        value = as_paragraph(c("Values change with consumer-resouce dynamics.", "Values change with consumer-resouce dynamics.")),
        ref_symbols = c("a", "a"), inline = F) %>% 
    footnote(i = 3:4, j = "Value", part = "body",
        value = as_paragraph(c("Values are assigned randomly to each species during simulation setup.", "Values are assigned randomly to each species during simulation setup.")),
        ref_symbols = c("b", "b"), inline = F) %>% 
    footnote(i = 4, j = "Value", part = "body",
        value = as_paragraph("The values in D", as_sub("αβ"), " do not matter if l", as_sub("α"), " is 0."),
        ref_symbols = c("c"), inline = F) %>% 
    footnote(i = 9, j = "Value", part = "body",
        value = as_paragraph("Depending on type of functional response chosen."),
        ref_symbols = c("d"), inline = F) %>%
    merge_v(part = "footer")

save_as_image(ft2, "../Plots/TableS2.png")

