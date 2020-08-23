library(data.table)
library(tidyverse)

# Read function data
df_function <- fread("../data/temp/df_function.txt", stringsAsFactors = T) %>%
  filter(!(CommunityPhenotypeName %in% c("f3_additive_binary", "f4_interaction_binary"))) %>%
  as.data.table()

df_function[grepl("f5|f6", CommunityPhenotypeName), CommunityPhenotype := -1/CommunityPhenotype]

# Algorithm
algorithms <- fread("../data/temp/algorithms.txt", stringsAsFactors = T)

# Summary statistics of functions
df_function_summary <- df_function %>%
  group_by(CommunityPhenotypeName, AlgorithmCategory, Algorithm, SpeciesPool, Transfer) %>%
  summarize(MaxCP = max(CommunityPhenotype), MinCP = min(CommunityPhenotype), MeanCP = mean(CommunityPhenotype), MedianCP = median(CommunityPhenotype), VarCP = var(CommunityPhenotype))

# Screen
## Filter for screening
df_function_summary_screen <- df_function_summary %>%
  filter(Algorithm == "simple_screening") %>%
  ungroup() %>%
  select(-AlgorithmCategory, -Algorithm) %>%
  setNames(c("CommunityPhenotypeName", "SpeciesPool", "Transfer", "MaxCP_screen", "MinCP_screen", "MeanCP_screen", "MedianCP_screen", "VarCP_screen"))

## Filter for monoculture
df_function_summary_monoculture <- df_function_summary %>%
  filter(Algorithm == "monoculture") %>%
  filter(Transfer == 20) %>% # Some experiments did not end at transfer 40, but single isolate reach equilibrium really quick, so transfer 20 is enought to represent treansfer 40
  ungroup() %>%
  select(-AlgorithmCategory, -Algorithm, -Transfer) %>%
  setNames(c("CommunityPhenotypeName", "SpeciesPool", "MaxCP_monoculture", "MinCP_monoculture", "MeanCP_monoculture", "MedianCP_monoculture", "VarCP_monoculture"))


# Bind two dfs
df_function_summary2 <- df_function_summary %>%
  left_join(df_function_summary_screen) %>%
  left_join(df_function_summary_monoculture)

# Compute performance
df_performance <- df_function_summary2 %>%
  mutate(Max_Performance = MaxCP/MaxCP_screen,
         Min_Performance = MinCP/MinCP_screen,
         Median_Performance = MedianCP/MedianCP_screen,
         Var_Performance = VarCP/VarCP_screen,
         Max_Performance_monoculture = MaxCP/MaxCP_monoculture,
         Min_Performance_monoculture = MinCP/MinCP_monoculture,
         Median_Performance_monoculture = MedianCP/MedianCP_monoculture,
         Var_Performance_monoculture = VarCP/VarCP_monoculture) %>%
  select(CommunityPhenotypeName, AlgorithmCategory, Algorithm, SpeciesPool, Transfer,
         Max_Performance, Min_Performance, Median_Performance, Var_Performance,
         Max_Performance_monoculture, Min_Performance_monoculture, Median_Performance_monoculture, Var_Performance_monoculture)

# Write files
fwrite(df_performance, file = "../data/temp/df_performance.txt")






