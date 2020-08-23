#' Iteration protocol and robustness
#' 1. Aggregate the robutness data: all function of one seed, final function of all seeds, robustness function
#' 2. Calculate robustness

library(tidyverse)
library(data.table)
library(cowplot)
library(ggpubr)

# Parameters
n_directed_selected = 20 # Number of rounds of directed selection
n_transfer_round = 20 # Number of transfer between rounds 
show_iteration_protocol <- c("simple_screening", 1:3)
name_iteration_protocol <- c("simple_screening","bottleneck", "migration", "bottleneck+migration") %>% setNames(show_iteration_protocol)
data_directory <- "../data/raw/"
example_pool_for_plot <- 3

# Input csv
input_csv_iteration <- fread("../data/input_additive_iteration.csv")

# Aggregate funciton of iterative directed selection, for one seed
aggregate_iteration_function <- function(x){ 
    # Extract the iteration algorithm from file name
    iteration_protocol <- x %>% 
        strsplit("/") %>% unlist %>% `[`(length(.)) %>% 
        strsplit("-") %>% unlist %>% `[`(2) %>%
        strsplit("_") %>% unlist %>% `[`(2) %>% as.numeric()
    
    # Extract the iteraction round from file name
    iteration_round <- x %>% 
        strsplit("/") %>% unlist %>% `[`(length(.)) %>% 
        strsplit("-") %>% unlist %>% `[`(2) %>%
        strsplit("_") %>% unlist %>% `[`(3) %>%
        sub("round", "", .) %>% as.numeric()
    
    # Seed
    seed <- x %>% 
        strsplit("/") %>% unlist %>% `[`(length(.)) %>% 
        strsplit("-") %>% unlist %>% `[`(3) %>%
        strsplit("_") %>% unlist %>% `[`(1)
    # Stack on the number of transfer
    df <- fread(x)
    n_transfer_round <- max(df$Transfer) - min(df$Transfer)
    df <- df %>%
        mutate(Transfer = Transfer + (iteration_round-1) * n_transfer_round,
            IterationProtocol = iteration_protocol)
    
    # Remove the first transfer in each iteraction, because it is the same as the last transfer in the previous directed selection round
    if (iteration_round > 1) df <- df %>% filter(Transfer != min(Transfer))
    df <- df %>% mutate(SpeciesPool = seed)
    return(df)
}

## Function of one seed
df_func_pool1 <- input_csv_iteration$exp_id %>% 
    grep(paste0("-", example_pool_for_plot, "$"), ., value = T) %>% 
    grep("round", ., value = T) %>% 
    paste0(data_directory, ., "_function.txt") %>% 
    lapply(aggregate_iteration_function) %>% 
    rbindlist() %>% 
    ## Clean up column names
    separate(col = exp_id, sep = "-", into = c("SelectedFunction", "Protocol", "SpeciesPool")) %>%
    separate(col = Protocol, sep = "_", into = c("temp", "IterationProtocol", "Round")) %>% 
    mutate(Round = as.numeric(sub("round", "", Round))) %>% 
    select(-temp) %>% 
    as_tibble

## Final function of all seeds
df_func_iteration <- grep("round23", input_csv_iteration$exp_id, value = T) %>% 
    paste0(data_directory, ., "_function.txt")  %>%
    lapply(aggregate_iteration_function) %>% 
    rbindlist() %>% as_tibble %>% 
    mutate(IterationProtocol = as.character(IterationProtocol))
df_func_screen <- grep("simple_screening", input_csv_iteration$exp_id, value = T) %>% 
    paste0(data_directory, ., "_function.txt")  %>%
    lapply(fread) %>% 
    rbindlist() %>% as_tibble %>% 
    mutate(IterationProtocol = "simple_screening")
df_func_final <- bind_rows(df_func_screen, df_func_iteration)

#fwrite(df_func_pool1, file = "../data/temp/df_func_pool1.txt")
#fwrite(df_func_final, file = "../data/temp/df_func_final.txt)
#fwrite(df_robustness, file = "../data/temp/df_robustness.txt")


# Plot
# Cartoon
p1_cartoon <- ggplot()

# Plot function over time
## Iteration for plotting vertical lines
df_iteration <- list(c("bottleneck", "bottleneck"), c("migration", "migration"), c("bottleneck+migration", "bottleneck+migration")) %>%
    lapply(function(x) c("simple_screening",rep(x, n_directed_selected/length(x)), "simple_screening")) %>%
    lapply(function(x) tibble(Perturbation = x, SelectionRound = 1:length(x))) %>%
    setNames(c(1,2,3)) %>% 
    rbindlist(idcol = "IterationProtocol") %>%
    #mutate(IterationProtocol = as.numeric(IterationProtocol)) %>% 
    as_tibble

# Vline
df_vline <- df_iteration %>%
    group_by(IterationProtocol) %>%
    mutate(Xinter = c(NA,seq(n_transfer_round+n_transfer_round/2,
        n_transfer_round+n_transfer_round/2+n_transfer_round*(n_directed_selected-1),
        by = n_transfer_round), NA)) %>%
    mutate(Perturbation = ordered(Perturbation, level = c("bottleneck", "migration", "bottleneck+migration"))) %>% 
    filter(!is.na(Xinter))

max_func <- df_func_pool1 %>% filter(Transfer %in% seq(29, 429, 20)) %>%  pull(CommunityPhenotype) %>% max# Max function
max_func_screen <- filter(df_func_pool1, Transfer == 30) %>% pull(CommunityPhenotype) %>% max() # Max function of screen

p2 <- df_func_pool1 %>%
    left_join(df_iteration) %>%
    ggplot(aes(x = Transfer, y = CommunityPhenotype, group = Well)) +
    geom_line() +
    # Reference 
    geom_hline(yintercept = max_func, linetype = 2, color = "red") +
    geom_hline(yintercept = max_func_screen, linetype = 2, color = "grey") +
    # Directed selection
    geom_vline(data = df_vline, aes(xintercept = Xinter, color = Perturbation), size = 1) +
    facet_grid(IterationProtocol~., labeller = as_labeller(name_iteration_protocol)) +
    scale_x_continuous(breaks = seq(10, 460, 20), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(-1000, 1500, 500)) +
    #scale_color_manual(values = c("#E3C454", "#576ED6", "#DA3E52")) +
    coord_cartesian(xlim = c(0, 460)) +
    guides(color = F) +
    theme_bw() +
    theme(legend.position = "top", legend.title = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        strip.background = element_blank(), strip.text = element_blank()) +
    labs(x = "Generation", y = "F") +
    panel_border(color = 1, size = 1) 


# Boxplot of maxminmun function
df_func_final_max <- df_func_final %>%
    filter(Transfer == 460) %>% 
    group_by(IterationProtocol, SpeciesPool) %>%
    filter(CommunityPhenotype == max(CommunityPhenotype))
df_func_final_max$PlotProtocol <- NA
df_func_final_max$PlotProtocol[df_func_final_max$IterationProtocol == 1] <- "Bottleneck"
df_func_final_max$PlotProtocol[df_func_final_max$IterationProtocol == 2] <- "Migration"
df_func_final_max$PlotProtocol[df_func_final_max$IterationProtocol == 3] <- "Combined"

compare_means(CommunityPhenotype ~ IterationProtocol, data = df_func_final_max)
my_comparisons <- list(c("Bottleneck", "Migration"), c("Migration", "Combined"), c("Bottleneck", "Combined") )
p3 <- ggboxplot(df_func_final_max, x = "PlotProtocol", y = "CommunityPhenotype")+ 
    geom_jitter(shape = 21) +
    stat_compare_means(comparisons = my_comparisons, label.y = c(1700, 1800, 1900), method = "t.test") +
    panel_border(color = 1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.2)) +
    labs(x = "", y = expression(F[max])) 


# 
p_iteration <- plot_grid(p2, p3, labels = c("B", "C"), nrow = 1, align = "h", axis = "tb", rel_widths = c(3, 1))
p <- plot_grid(p1_cartoon, p_iteration, labels = c("A", ""), ncol = 1, rel_heights = c(1,1))

ggsave("../Plots/Fig4.png", plot = p, width = 10, height = 8)








