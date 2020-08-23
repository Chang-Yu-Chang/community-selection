library(tidyverse)
library(data.table)
library(cowplot)

# Read data
df_richness <- fread("../data/temp/df_richness.txt", stringsAsFactors = T) %>%
  filter(grepl("f1|f2|f2a|f5|f6", CommunityPhenotypeName)) %>%
  as_tibble()
df_rich_func_screen <- fread("../data/temp/df_rich_func_screen.txt", stringsAsFactors = T) %>%
  filter(grepl("f1|f2|f2a|f5|f6", CommunityPhenotypeName)) %>%
  as_tibble()
df_rich_func <- fread("../data/temp/df_rich_func.txt", stringsAsFactors = T) %>%
  filter(grepl("f1|f2|f2a|f5|f6", CommunityPhenotypeName)) %>%
  as_tibble()

# Richness at the transfer 40
p <- df_richness %>%
  group_by(CommunityPhenotypeName, AlgorithmCategory, Algorithm, SpeciesPool, Transfer) %>%
  filter(CommunityPhenotype == max(CommunityPhenotype)) %>%
  filter(Algorithm != "monoculture", Richness != 0 ) %>%
  ggplot(aes(x = Algorithm, y = Richness, color = AlgorithmCategory, group = Algorithm)) +
  geom_boxplot() +
  geom_jitter() +
  # facet_grid(, scales = "free") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.title = element_blank(), legend.position = "top")

ggsave("../figure/algorithm_richness.pdf", width = 8, height = 8); p; invisible(dev.off())
ggsave("../figure/algorithm_richness.png", width = 8, height = 8); p; invisible(dev.off())


# Diversity-function relationships
## Scatter plot
temp <- df_richness %>%
  filter(SpeciesPool %in% c(1:10)) %>%
  filter(Algorithm != "monoculture") %>%
  #filter(Algorithm %in% c("simple_screening", "Blouin2015", "Swenson2000b", "pair_top_communities")) %>%
  filter(Algorithm %in% c("simple_screening")) %>%
  filter(grepl("f5|f6", CommunityPhenotypeName)) %>%
  {.}

p1 <-  temp %>%
  ggplot(aes(x = Richness, y = CommunityPhenotype)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(CommunityPhenotypeName ~ SpeciesPool, scales = "free") +
  theme_cowplot() +
  panel_border() +
  ggtitle("x0/x1") +
  labs(x = "richness", y = "function")

p2 <- temp %>%
  mutate(CommunityPhenotype = -1/CommunityPhenotype) %>%
  ggplot(aes(x = Richness, y = CommunityPhenotype)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(CommunityPhenotypeName ~ SpeciesPool, scales = "free") +
  theme_cowplot()  +
  panel_border() +
  ggtitle("-x1/x0") +
  labs(x = "richness", y = "function")

p <- plot_grid(plotlist = list(p1,p2), ncol = 1, labels = LETTERS[1:2])
ggsave("../figure/algorithm_diversity_function_scatter.pdf", width = 20, height = 10); p; invisible(dev.off())
ggsave("../figure/algorithm_diversity_function_scatter.png", width = 20, height = 10); p; invisible(dev.off())


## p.value distribution
p1 <- df_rich_func %>%
  filter(Transfer == 40) %>%
  #  as_tibble()
  ggplot(aes(x = p.value)) +
  geom_histogram (binwidth = 0.02, fill = "grey", color = 1) +
  geom_vline(xintercept = 0.05, color = "red") +
  facet_wrap(CommunityPhenotypeName~., scales = "free", ncol = 1, strip.position = "right") +
  theme_cowplot()

## Slope distribution
p2 <- df_rich_func %>%
  filter(Transfer == 40) %>%
#  filter(p.value < 0.05) %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(fill = "grey", color = 1) +
  geom_vline(xintercept = 0, color = "red") +
  facet_wrap(CommunityPhenotypeName~., scales = "free", ncol = 1, strip.position = "right") +
  theme_cowplot() +
  labs(x = "slope")

p <- plot_grid(p1, p2, labels = c("A", "B"))
ggsave("../figure/algorithm_diversity_function.pdf", width = 10, height = 10); p; invisible(dev.off())
ggsave("../figure/algorithm_diversity_function.png", width = 10, height = 10); p; invisible(dev.off())



