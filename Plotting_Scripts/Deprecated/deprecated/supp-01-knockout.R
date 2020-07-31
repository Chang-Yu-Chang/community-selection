#' To test whether changes in community function results from ecological interaction,
#' by testing the effect of species removal on the changes in community function
#'
#' This script
#' 1. takes the compostion data from knock-out perturbation
#' 2. plots the per-capita function of the knockout species as the predictor of community function after knockout
#' 3. plots the species contrbution as the predictor of community function after knockout


library(tidyverse)
library(data.table)
library(broom)
library(cowplot)

# Read data
df_per_capita <- fread("../data/raw/per_capita_function/f1_additive.txt")
df_func <- fread("../data/temp/df_function_filtered_f1.txt") %>% filter(Algorithm == "knock_out") %>% as_tibble
df_comp <- fread("../data/temp/df_composition_perturbation.txt") %>% filter(Algorithm == "knock_out") %>% as_tibble


# Match the knockout isolate to the T40 community function
n_pools = 100
temp_list <- rep(list(NA), n_pools)
temp_list2 <- rep(list(NA), n_pools)

for (i in 1:n_pools) {
  # Subset for one pool
  df_func_s1 <- df_func %>% filter(SpeciesPool == i) %>% arrange(Well)
  df_comp_s1 <- df_comp %>% filter(SpeciesPool == i) %>% arrange(Well)

  # Find the best community at T20
  best_comm_func <- df_func_s1 %>%
    #  select(SpeciesPool, Algorithm, Transfer, Well, CommunityPhenotypeName, CommunityPhenotype) %>%
    #  pivot_wider(names_from = Transfer, values_from = CommunityPhenotype)
    filter(Transfer == 20) %>%
    group_by(SpeciesPool) %>%
    arrange(desc(CommunityPhenotype)) %>%
    slice(1) %>%
    select(SpeciesPool, Well, CommunityPhenotype)

  temp_list2[[i]] <- best_comm_func

  best_comm_comp <-
    best_comm_func %>%
    select(SpeciesPool, Well) %>%
    left_join(df_comp_s1, by = c("SpeciesPool", "Well")) %>%
    filter(Type == "consumer")

  # Replicate the best comm comp at T20 for all wells
  temp <- best_comm_comp %>% filter(Transfer == 20)
  temp_vec <- unique(df_func_s1$Well)
  temp_list_loop <- rep(list(NA), length(temp_vec))
  for (j in 1:length(temp_vec)) temp_list_loop[[j]] <- temp %>% mutate(Well = temp_vec[j])
  best_comm_comp_T20 <- bind_rows(temp_list_loop)


  # For each well, match the knocked out community to find out which species is knocked out
  ## Find all species that go extinct in the stabilization phase
  extinct_species <- df_comp_s1 %>%
    # Removed the compostion at T20
    filter(Type == "consumer", Transfer %in% c(40)) %>%
    # Add the best community compostition at T20
    bind_rows(best_comm_comp_T20, .) %>%
    select(SpeciesPool, Transfer, Well, Type, ID, Abundance) %>%
    group_by(SpeciesPool, Well) %>%
    pivot_wider(names_from = Transfer, values_from = Abundance) %>%
    arrange(SpeciesPool, Well, ID) %>%
    # A species goes extinct if its biomass at T40 is NA
    filter(is.na(`40`)) %>%
    select(-`40`)

  ## Species that go extinct in the unperturbed plate
  extinct_species_ctrl <- extinct_species %>%
    right_join(best_comm_func, by = c("SpeciesPool", "Well")) %>%
    ungroup() %>%
    select(SpeciesPool, ID)

  ## Remove the species that go extinct naturally to find the knockout species.
  ## The knockout species can be a species that would go naturally entinct by itself
  ## In this case those knockout species cannot be detected and the result dataframe has fewer rows than richness
  knock_out_species <- extinct_species %>%
    anti_join(extinct_species_ctrl, by = "ID") %>%
    # In a well there can be two species that go extinct. Pick the one with highest abundance (the once mostly likely being knocked out)
    group_by(SpeciesPool, Well) %>%
    arrange(SpeciesPool, Well, desc(`20`)) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(`20`) %>%
    # There are pseudo replicates because its the old compostition data
    # distinct(SpeciesPool, ID, .keep_all = T) %>%
    # select(SpeciesPool, Well, ID, Abundance = `20`) %>%
    arrange(Well) %>%
    select(SpeciesPool, Well, ID, KnockoutSpeciesAbundanceT20 = `20`)


  # Match the knockout species, abudnace, percapita function, and community function
  df_func_match_s1 <- df_func_s1 %>%
    filter(Transfer %in% c(20,40)) %>%
    select(SpeciesPool, Well, CommunityPhenotype, Transfer, Biomass) %>%
    # Community phenotpe Biomass at T20 and T40
    group_by(SpeciesPool, Well) %>%
    pivot_wider(names_from = Transfer, values_from = c(CommunityPhenotype, Biomass), names_prefix = "T", names_sep = "") %>%
    # We do not need each community function at T20, we only need the best
    select(-CommunityPhenotypeT20) %>%
    # Instead we add the community phenitype of the best at T20
    left_join(select(best_comm_func, SpeciesPool, CommunityPhenotypeT20 = CommunityPhenotype), by = c("SpeciesPool")) %>%
    select(SpeciesPool, Well, CommunityPhenotypeT20, CommunityPhenotypeT40, BiomassT20, BiomassT40) %>%
    # Knockout species
    right_join(knock_out_species, by = c("SpeciesPool", "Well")) %>%
    # Per capita function
    left_join(df_per_capita, by = c("SpeciesPool", "ID")) %>%
    # Compute the KnockoutSpeciesFunction = lambda*n
    mutate(KnockoutSpeciesID = ID,
           KnockoutSpeciesPerCapitaFunction = PerCapitaFunction,
           KnockoutSpeciesContribution = KnockoutSpeciesAbundanceT20 * KnockoutSpeciesPerCapitaFunction)


  temp_list[[i]] <- df_func_match_s1
  cat(i, "")
}

df_func_match <- bind_rows(temp_list)
df_func_best_comm <- bind_rows(temp_list2)

# Compute 1) delta function  and 2) delta function corrected
df_func_match <- df_func_match %>%
  mutate(deltaFunction = CommunityPhenotypeT40 - CommunityPhenotypeT20,
         deltaFunctionCorrected = (BiomassT20-KnockoutSpeciesAbundanceT20)/BiomassT40 * CommunityPhenotypeT40 - CommunityPhenotypeT20)

# fwrite(df_func_match, "../data/temp/df_func_match.txt")
# fwrite(df_func_best_comm, "../data/temp/df_func_match.txt")


# Plot 1: Lambda as the predictor
seed = 1
p1 <- df_func_match %>%
  filter(SpeciesPool == seed) %>%
  ggplot(aes(x = KnockoutSpeciesPerCapitaFunction, y = deltaFunction)) +
  geom_smooth(method = "lm") +
  #  geom_abline(intercept = -1, slope = -1, color = "red", linetype = 2) +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  theme_cowplot() +
  labs(x = expression(lambda[i]), y = expression(paste(Delta,"F")==F[new]-F[old])) +
  ggtitle("no niche overlap")

p2 <- df_func_match %>%
  filter(SpeciesPool == seed) %>%
  ggplot(aes(x = KnockoutSpeciesPerCapitaFunction, y = deltaFunctionCorrected)) +
  geom_smooth(method = "lm") +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  theme_cowplot() +
  labs(x = expression(lambda[i]), y = expression(paste(Delta,"F")[cor]==frac(n[old]-n[i],n[new])%.%F[new]-F[old])) +
  ggtitle("niche overlap")

# Statistics
## deltaFuction ~lambda*n
df_func_match_stat1 <- df_func_match %>%
  group_by(SpeciesPool) %>%
  nest() %>%
  mutate(model = map(data, ~lm(.x$deltaFunction ~ .x$KnockoutSpeciesPerCapitaFunction)),
         tidied = map(model, tidy),
         glanced = map(model, glance)) %>%
  unnest(tidied) %>%
  filter(grepl("KnockoutSpeciesPerCapitaFunction", term)) %>%
  select(SpeciesPool, slope = estimate, glanced) %>%
  mutate(DependentVariable = "deltaFunction") %>%
  unnest(glanced) %>%
  select(SpeciesPool, DependentVariable, slope, r.squared, p.value)

## deltaFuctionCorrected ~lambda*n
df_func_match_stat2 <- df_func_match %>%
  group_by(SpeciesPool) %>%
  nest() %>%
  mutate(model = map(data, ~lm(.x$deltaFunctionCorrected ~ .x$KnockoutSpeciesPerCapitaFunction)),
         tidied = map(model, tidy),
         glanced = map(model, glance)) %>%
  unnest(tidied) %>%
  filter(grepl("KnockoutSpeciesPerCapitaFunction", term)) %>%
  select(SpeciesPool, slope = estimate, glanced) %>%
  mutate(DependentVariable = "deltaFunctionCorrected") %>%
  unnest(glanced) %>%
  select(SpeciesPool, DependentVariable, slope, r.squared, p.value)

#
p3 <- df_func_match_stat1 %>%
  bind_rows(df_func_match_stat2) %>%
  filter(DependentVariable == "deltaFunction") %>%
  filter(p.value < 0.05) %>%
  ggplot(aes(x = r.squared, y = slope)) +
  geom_hline(yintercept = -1, color = "black", linetype = 2) +
  geom_vline(xintercept = 0.5, color = "black", linetype = 2) +
  geom_point(shape = 21) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_cowplot() +
  labs(x = expression(R^2)) +
  ggtitle("no niche overlap")

p4 <- df_func_match_stat1 %>%
  bind_rows(df_func_match_stat2) %>%
  filter(DependentVariable == "deltaFunctionCorrected") %>%
  filter(p.value < 0.05) %>%
  ggplot(aes(x = r.squared, y = slope)) +
  geom_hline(yintercept = -1, color = "black", linetype = 2) +
  geom_vline(xintercept = 0.5, color = "black", linetype = 2) +
  geom_point(shape = 21) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_cowplot() +
  labs(x = expression(R^2)) +
  ggtitle("niche overlap")

p <- plot_grid(plotlist = list(p1, p2, p3, p4), nrow = 2, align = "vh", labels = c("A", "", "B", ""))

ggsave("../figure/supp-01-knock_out-per_capita.png", plot = p, width = 10, height = 8)




# Plot2: lambda*n as predictor
seed = 1
p1 <- df_func_match %>%
  filter(SpeciesPool == seed) %>%
  ggplot(aes(x = KnockoutSpeciesContribution, y = deltaFunction)) +
  geom_abline(intercept = -1, slope = -1, color = "red", linetype = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  geom_point(shape = 21) +
  theme_cowplot() +
  labs(x = expression(lambda[i] %.% n[i]), y = expression(paste(Delta,"F")==F[new]-F[old])) +
  ggtitle("no niche overlap")

p2 <- df_func_match %>%
  filter(SpeciesPool == seed) %>%
  ggplot(aes(x = KnockoutSpeciesContribution, y = deltaFunctionCorrected)) +
  geom_abline(intercept = -1, slope = -1, color = "red", linetype = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  geom_point(shape = 21) +
  theme_cowplot() +
  labs(x = expression(lambda[i] %.% n[i]), y = expression(paste(Delta,"F")[cor]==frac(n[old]-n[i],n[new])%.%F[new]-F[old])) +
  ggtitle("niche overlap")

# Statistics
## deltaFuction ~lambda*n
df_func_match_stat1 <- df_func_match %>%
  group_by(SpeciesPool) %>%
  nest() %>%
  mutate(model = map(data, ~lm(.x$deltaFunction ~ .x$KnockoutSpeciesContribution)),
         tidied = map(model, tidy),
         glanced = map(model, glance)) %>%
  unnest(tidied) %>%
  filter(grepl("KnockoutSpeciesContribution", term)) %>%
  select(SpeciesPool, slope = estimate, glanced) %>%
  mutate(DependentVariable = "deltaFunction") %>%
  unnest(glanced) %>%
  select(SpeciesPool, DependentVariable, slope, r.squared, p.value)

## deltaFuctionCorrected ~lambda*n
df_func_match_stat2 <- df_func_match %>%
  group_by(SpeciesPool) %>%
  nest() %>%
  mutate(model = map(data, ~lm(.x$deltaFunctionCorrected ~ .x$KnockoutSpeciesContribution)),
         tidied = map(model, tidy),
         glanced = map(model, glance)) %>%
  unnest(tidied) %>%
  filter(grepl("KnockoutSpeciesContribution", term)) %>%
  select(SpeciesPool, slope = estimate, glanced) %>%
  mutate(DependentVariable = "deltaFunctionCorrected") %>%
  unnest(glanced) %>%
  select(SpeciesPool, DependentVariable, slope, r.squared, p.value)

#
p3 <- df_func_match_stat1 %>%
  bind_rows(df_func_match_stat2) %>%
  filter(DependentVariable == "deltaFunction") %>%
  filter(p.value < 0.05) %>%
  ggplot(aes(x = r.squared, y = slope)) +
  geom_hline(yintercept = -1, color = "black", linetype = 2) +
  geom_vline(xintercept = 0.5, color = "black", linetype = 2) +
  geom_point(shape = 21) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-1.8, -0.4)) +
  theme_cowplot() +
  labs(x = expression(R^2)) +
  ggtitle("no niche overlap")


p4 <- df_func_match_stat1 %>%
  bind_rows(df_func_match_stat2) %>%
  filter(DependentVariable == "deltaFunctionCorrected") %>%
  filter(p.value < 0.05) %>%
  ggplot(aes(x = r.squared, y = slope)) +
  geom_hline(yintercept = -1, color = "black", linetype = 2) +
  geom_vline(xintercept = 0.5, color = "black", linetype = 2) +
  geom_point(shape = 21) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-1.8, -0.4)) +
  theme_cowplot() +
  labs(x = expression(R^2)) +
  ggtitle("niche overlap")

p <- plot_grid(plotlist = list(p1, p2, p3, p4), nrow = 2, align = "vh", labels = c("A", "", "B", ""))

ggsave("../figure/supp-01-knock_out-species_contribution.png", plot = p, width = 10, height = 8)






