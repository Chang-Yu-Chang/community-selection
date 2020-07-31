library(data.table)
library(tidyverse)

# Algorithm list
tested_algorithms <-
  c("simple_screening", "monoculture",
    "Arora2019", "Swenson2000b", "Blouin2015", "Blouin2015_control", "Jochum2019", "Mueller2019", "Panke_Buisse2015",
    "Penn2004", "Raynaud2019a", "Raynaud2019b",
    "Swenson2000a", "Swenson2000a_control", "Swenson2000b_control", "Swenson2000c",
    "Williams2007b", "Wright2019", "Xie2019a", "Xie2019b",
    "Williams2007a",
    "directed_selection_migration", "pair_top_communities", "multiple_pair_top",
    "coalescence", "migration", "resource", "bottleneck", "knock_out", "knock_in",
    "iterative_ctrl", "iterative_resource", "iterative_migration", "iterative_resource_migration", "iterative_coalescence")
tested_algorithms <- ordered(tested_algorithms, tested_algorithms)
tested_algorithms <- sort(tested_algorithms)
algorithms <- tibble(Algorithm = tested_algorithms, AlgorithmCategory = c(rep("screen", 2), rep("literature", 19), rep("directed_selection", 3), rep("perturbation", 6), rep("iteration", 5)))
fwrite(algorithms, file = "../data/temp/algorithms.txt")

# Read file
experiments <- c("example_run", "directed_selection", "literature", "perturbation")
function_list <- rep(list(NA), length(experiments))

for (j in 1:length(experiments)) {
  print(experiments[j])
  temp_file <- list.files(paste0("../data/", experiments[j] ,"/functions/"), full.names = T)
  n_pools <- length(temp_file)
  temp_list <- rep(list(NA), n_pools)

  for (i in 1:n_pools) {
    temp_list[[i]] <-
      temp <- fread(temp_file[i]) %>%
      mutate(Algorithm = ordered(Assembly, tested_algorithms)) %>%
      filter(Algorithm %in% tested_algorithms) %>%
      select(-Assembly) %>%
      left_join(algorithms, by = "Algorithm")
    cat(i, "\t")
  }

  function_list[[j]] <- rbindlist(temp_list)
}

# Merge files
df_function <- rbindlist(function_list)

# Write files
fwrite(df_function, file = "../data/temp/df_function.txt")
df_function %>% filter(Transfer %in% c(0, 19, 20, 40)) %>% fwrite(file = "../data/temp/df_function_filtered.txt")





