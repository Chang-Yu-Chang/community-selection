library(tidyverse)
library(data.table)

well <- 1:96

#
perturbation <- fread("../data/temp/perturbation.txt")


# Composition; BC distance
BC_dissimilarity <- function (df) {
  common = 0; for (i in 1:nrow(df)) common = common + min(df[i,c("Ctrl", "Algo")])
  S1 = sum(df$Ctrl)
  S2 = sum(df$Algo)
  BC = 1 - 2 * common / (S1+S2)
  return (BC)
}


temp_list2 <- rep(list(NA), length(well))
names(temp_list2) <- paste0("TW", well)

for (k in 1:length(well)) {
  # Read data
  df_community_stable <- list.files("../data/perturbation_effect/raw/", pattern = "composition", full.names = T) %>%
    grep(paste0("-TW", well[k], "-"), ., value = T) %>%
    lapply(fread) %>%
    rbindlist(fill = T) %>%
    filter(Type == "consumer") %>%
    filter(Transfer == 40) %>%
    select(Perturbation, Transfer, Well, ID, Abundance) %>%
    group_by(Perturbation, Transfer, Well) %>%
    mutate(RelativeAbundance = Abundance / sum(Abundance)) %>%
    ungroup


  temp_vc <- unique(df_community_stable$Perturbation)
  temp_vc = temp_vc[temp_vc!=0]
  temp_list <- rep(list(rep(NA, 96)), length(temp_vc))
  names(temp_list) <- temp_vc

  for (i in 1:(length(temp_list))) {
    for (j in 1:96){
      temp_list[[i]][j] <-
        df_community_stable %>%
        filter(Perturbation %in% c(0, temp_vc[i])) %>%
        filter(Well == paste0("W", j-1)) %>%
        select(Perturbation, ID, RelativeAbundance) %>%
        pivot_wider(names_from = Perturbation, values_from = RelativeAbundance) %>%
        setNames(c("Sp", "Ctrl", "Algo")) %>%
        replace_na(list(Ctrl = 0, Algo = 0)) %>%
        BC_dissimilarity()
    }
    #    cat(i, " ")
  }

  temp_list2[[k]] <-  as_tibble(temp_list)
  cat("\nComputing BC dissimilarity for TW", well[k])
}

"
melte the datafraec in each loop so that the column numbe matches
"

df_perturbation_BC <- rbindlist(temp_list2, idcol = "TargetWell") %>%
  group_by(TargetWell) %>%
  mutate(Well = 1:96) %>%
  pivot_longer(cols = c(-Well, -TargetWell), names_to = "Perturbation", values_to = "Dissimilarity") %>%
  select(TargetWell, Perturbation, Well, Dissimilarity) %>%
  arrange(TargetWell, Perturbation, Well)

fwrite(df_perturbation_BC, "../data/temp/df_perturbation_BC.txt")


