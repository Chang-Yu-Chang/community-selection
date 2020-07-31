#' Table S3: global parameters used for constructing random ecosystems
library(tidyverse)
library(data.table)
library(flextable)
library(officer)

t3 <- c(
    "M", "Number of resources", 90,
    "T", "Number of resource classes", 1,
    "H", "Number of microbial species in global pool", 2100,
    "Rtot",	'Total resource abundance', 1000,
    "S_f", "Number of specialist families", 1,
    "uc", "Mean sum over a row of the preference matrix ciα", 10,
    "σc", "Standard deviation of sum over a row of the preference matrix ciα", 3,
    "c0", "Low consumption level for binary ciα", 0,
    "c1", "High consumption level for binary ciα", 1,
    "q", "Fraction of consumption capacity allocated to preferred resource class",	0,
    "s", "Sparsity of metabolic matrix", 0.2,
    "fw", "Fraction of secreted byproducts allocated to waste resource class",	0.45,
    "fs", "Fraction of secreted byproducts allocated to the same resource class", 0.45,
    "a", "Exponent parameter in power-law distribution that determines the species abundance in regional pool", 0.01,
    "scale", "Number of cells equivalent to N_i = 1", 1000000,
    "n_inoc", "Number of cells in the initial inoculum", 1000000,
    "α", "Relative functional contribution of species interaction to the additive case", 1
    #"seed", "A number used to initialize a pseudorandom number generator", 2
) %>%
    matrix(ncol = 3, byrow = T) %>%
    as_tibble() %>%
    setNames(c("Parameter",	"Description and units", "Value"))

fwrite(t3, "../data/TableS3.csv")

ft3 <- flextable(t3) %>% 
    width(j = 2, width = 5) %>%
    align(j = 1:3, align = "left", part = "all") %>% 
    compose(i = 4, j = "Parameter", value = as_paragraph("R", as_sub("tot"))) %>% 
    compose(i = 5, j = "Parameter", value = as_paragraph("S", as_sub("f"))) %>% 
    compose(i = 6, j = "Parameter", value = as_paragraph("u", as_sub("c"))) %>% 
    compose(i = 6, j = 2, value = as_paragraph("Mean sum over a row of the preference matrix c", as_sub("iα"))) %>% 
    compose(i = 7, j = "Parameter", value = as_paragraph("σ", as_sub("c"))) %>% 
    compose(i = 7, j = 2, value = as_paragraph("Standard deviation of sum over a row of the preference matrix c", as_sub("iα"))) %>% 
    compose(i = 8, j = "Parameter", value = as_paragraph("c", as_sub("0"))) %>% 
    compose(i = 8, j = 2, value = as_paragraph("Low consumption level for Binary c", as_sub("iα"))) %>% 
    compose(i = 9, j = 2, value = as_paragraph("High consumption level for Binary c", as_sub("iα"))) %>% 
    compose(i = 9, j = "Parameter", value = as_paragraph("c", as_sub("1"))) %>% 
    compose(i = 12, j = "Parameter", value = as_paragraph("f", as_sub("w"))) %>% 
    compose(i = 13, j = "Parameter", value = as_paragraph("f", as_sub("s"))) %>% 
    compose(i = 15, j = "Parameter", value = as_paragraph("ψ")) %>% 
    compose(i = 15, j = 2, value = as_paragraph("Number of cells when N", as_sub("i"), " = 1")) %>% 
    compose(i = 16, j = "Parameter", value = as_paragraph("n", as_sub("inoc"))) %>% 
    colformat_num(j = 3, i = 1:10, big.mark=",", digits = 0, na_str = "N/A") %>%
    colformat_num(j = 3, i = 11:13, big.mark=",", digits = 2, na_str = "N/A") %>%
    # Footnote
    footnote(i = c(10, 12, 13), j = "Value", part = "body",
        value = as_paragraph("These values do not matter if S", as_sub("f"), " is 1"),
        ref_symbols = c("a"), inline = F) %>% 
    footnote(i = c(11), j = "Value", part = "body",
        value = as_paragraph("This value does not matter if l", as_sub("α"), " is 0"),
        ref_symbols = c("b"), inline = F) %>% 
    valign(j = 1:3, valign = "top", part = "all") %>%
    {.}

save_as_image(ft3, "../Plots/TableS3.png")
