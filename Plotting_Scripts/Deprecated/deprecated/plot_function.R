library(tidyverse)
library(data.table)
library(cowplot)

# Small threshold 1
list.files("data/test/", "-function.txt") %>%
  grep("^simple", ., value = T) %>%
  grep("f3", ., value = T) %>%
  lapply(function(x) fread(paste0("data/test/", x))) %>%
  rbindlist() %>%
  ggplot(aes(x = Transfer, y = CommunityPhenotype, color = Well)) +
  geom_line() +
  theme_half_open()


# Large threshold 10
df_function <- list.files("data/test/", "-function.txt") %>%
  grep("large_threshold", ., value = T) %>%
  grep("f3", ., value = T) %>%
  lapply(function(x) fread(paste0("data/test/", x))) %>%
  rbindlist()

df_function %>%
#  filter(Well == "W0") %>%
  ggplot(aes(x = Transfer, y = CommunityPhenotype, color = Well)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 40)) +
  theme_half_open()


 # Compostition
df_community <- list.files("data/test/", "-composition.txt") %>%
  grep("large_threshold", ., value = T) %>%
  grep("f3", ., value = T) %>%
  lapply(function(x) fread(paste0("data/test/", x))) %>%
  rbindlist() %>%
  mutate(ID = factor(ID))

df_community %>%
  filter(Well == "W0") %>%
  filter(Type == "consumer") %>%
  group_by(Assembly, CommunityPhenotypeName, Well, Transfer, Type, ID) %>%
  ggplot(aes(x = Transfer, y = Abundance, color = ID)) +
  geom_line() +
  geom_hline(yintercept = 10, color = "red") +
  scale_y_continuous(limits = c(0, 20)) +
  facet_grid(Type ~., scale = "free_y") +
  theme_half_open() +
  theme(legend.position = "none")



df_community %>%
  filter(Transfer %in% 76:80, Well == "W0", Type == "consumer")  %>%
  select(Transfer, Well, ID, Abundance) %>%
  pivot_wider(names_from = Transfer, values_from = Abundance)




