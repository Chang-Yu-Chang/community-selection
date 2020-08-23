#' Figure 5 resistance

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
input_csv_robustness <- fread("../data/input_additive_robustness.csv")

# Aggregate resistance data; this will take a while
df_robustness <- input_csv_robustness$exp_id %>% 
    paste0(data_directory, ., "_function.txt") %>%
    # Skip resource shift and knock_out
    #grep("migration|bottleneck", ., value = T) %>% 
    lapply(fread) %>% rbindlist() %>% as_tibble %>%
    separate(col = exp_id, sep = "-", into = c("SelectedFunction", "Protocol", "SpeciesPool", "InitialCommunity", "Perturbation"))

# Calculate resistance
## Find community that used to replicate 96. Remove it from after the perturbation
## Compute F and deltaF; f is the function before perturabtin, deltaF is the differenece between before/after perturabtion
df_comm_replicated_func <- df_robustness %>%
    filter(Transfer %in% c(20)) %>%
    group_by(SpeciesPool, Protocol, InitialCommunity, Perturbation) %>%
    filter(CommunityPhenotype == max(CommunityPhenotype)) %>%
    select(SpeciesPool, Perturbation, Protocol, InitialCommunity, SpeciesPool, Well, CommunityPhenotype)
df_comm_replicated <- select(df_comm_replicated_func, -CommunityPhenotype)

## Relicated communities that are perturbed
df_comm_perturbed <- df_robustness %>%
    filter(Transfer %in% c(20, 40)) %>%
    anti_join(df_comm_replicated)
df_comm_perturbed_T20 <- df_comm_perturbed %>%
    filter(Transfer == 20) %>%
    select(Protocol, SpeciesPool, InitialCommunity, Perturbation, Well, Function_20 = CommunityPhenotype)
df_comm_perturbed_func_diff <- df_comm_perturbed %>%
    filter(Transfer == 40) %>%
    group_by(SpeciesPool, Perturbation, Protocol, InitialCommunity, Transfer) %>%
    pivot_wider(names_from = Transfer, values_from = c(CommunityPhenotype), names_prefix = "Function_") %>%
    left_join(df_comm_perturbed_T20) %>% 
    # deltaF
    mutate(deltaF = Function_40 - Function_20, f20 = Function_20) %>%
    # Compute resistance according to resistance in Shade2012 et al
    mutate(resistance = (f20-abs(deltaF))/(f20+abs(deltaF))) %>%
    select(f20 = Function_20, f40 = Function_40, deltaF, resistance)

## Aggregated robusntess 
df_robustness_aggregated <- df_comm_perturbed_func_diff %>% 
    summarize(robustnessMean = mean(resistance), f20Max = max(f20), f40Mean = mean(f40))

## Standardize by screen
df_robustness_aggregated_screen <-
    df_robustness_aggregated %>%
    filter(Protocol == "simple_screening", InitialCommunity == "selected_community") %>%
    select(robustnessMean_screen = robustnessMean, f20Max_screen = f20Max, f40Mean_screen = f40Mean) %>%
    ungroup() %>% select(-Protocol)

df_robustness_aggregated_std <-
    df_robustness_aggregated %>%
    filter(Protocol != "simple_screening") %>%
    left_join(df_robustness_aggregated_screen) %>%
    mutate(robustnessMean_screen = robustnessMean - robustnessMean_screen,
        f20Max_std = f20Max - f20Max_screen,
        f40Mean_std = f40Mean - f40Mean_screen)

# Extract communtiy from  hybrid protocol and its synthetic community
df_hybrid <- df_robustness_aggregated %>%
    filter(Protocol == "iteration_3" | Protocol == "simple_screening") %>%
    filter(Perturbation == "migration") %>%
    filter(!(Protocol == "simple_screening" & InitialCommunity == "synthetic_community")) %>%
    unite(col = "Treatment", c(Protocol, InitialCommunity)) %>%
    ungroup() %>% 
    select(SpeciesPool, Treatment, Rmean = robustnessMean, Fmax = f20Max, Fstar = f40Mean)


# Plot
# Cartoon
p1_cartoon <- ggplot()


# Fmax 
p2 <- df_hybrid %>%
    ggplot(aes(x = Treatment, y = Fmax, color = Treatment)) +
    geom_boxplot(outlier.shape = 21, position = "identity") +
    geom_jitter(shape = 21) +
    scale_color_discrete(name = "", labels = c("Selected Community", "Synthetic Community", "Screen")) +
    theme_cowplot() +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "top") +
    labs(y = expression(F[max]))

# R, resistance
p3 <- df_hybrid %>%
    ggplot(aes(x = Treatment, y = Rmean, color = Treatment)) +
    geom_boxplot(outlier.shape = 21) +
    geom_jitter(shape = 21) +
    scale_color_discrete(name = "", labels = c("Selected Community", "Synthetic Community", "Screen")) +
    guides(color = F) +
    theme_cowplot() +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    labs(y = expression(R))

# Fstar; Fmean at T40
p4 <- df_hybrid %>%
    ggplot(aes(x = Treatment, y = Fstar, color = Treatment)) +
    geom_boxplot(outlier.shape = 21) +
    geom_jitter(shape = 21) +
    scale_color_discrete(name = "", labels = c("Selected Community", "Synthetic Community", "Screen")) +
    guides(color = F) +
    theme_cowplot() +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    labs(y = expression(paste("F"^"*")))

# 
p_robustness <- plot_grid(p2, p3, p4, labels = c("B", "C", "D"), nrow = 1, align = "h", axis = "tb", rel_widths = c(1,1,1))
p <- plot_grid(p1_cartoon, p_robustness, labels = c("A", ""), ncol = 1, rel_heights = c(1,1))

ggsave("../Plots/Fig5.png", plot = p, width = 10, height = 8)



if (FALSE) {
    # Example of resistance in density plot
    df_robustness_density_vline <- df_comm_replicated_func %>%
        filter(SpeciesPool == example_pool_for_plot, Protocol == "iteration_3" | Protocol == "simple_screening") %>%
        filter(Perturbation == "migration") %>%
        filter(!(Protocol == "simple_screening" & InitialCommunity == "synthetic_community")) %>%
        unite(col = "Treatment", c(Protocol, InitialCommunity))
    
    df_robustness_density <- df_robustness %>%
        filter(SpeciesPool == example_pool_for_plot, Protocol == "iteration_3" | Protocol == "simple_screening") %>%
        filter(Perturbation == "migration") %>%
        filter(!(Protocol == "simple_screening" & InitialCommunity == "synthetic_community")) %>%
        unite(col = "Treatment", c(Protocol, InitialCommunity)) %>%
        anti_join(select(df_robustness_density_vline, -CommunityPhenotype))
    
    # 
    p2 <- df_robustness_density %>%
        filter(Transfer != 0) %>%
        ggplot(aes(x = Transfer, y = CommunityPhenotype, color = Treatment, alpha = Well)) +
        geom_line() +
        geom_vline(xintercept = 20, color = 1, size = 1, linetype = 2) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(limits = c(-610, 2300), breaks = seq(-500, 2500, 500), expand = c(0,0)) + 
        scale_color_discrete(name = "", labels = c("Selected Community", "Synthetic Community", "Screen")) +
        guides(alpha = F) +
        theme_cowplot() +
        theme(legend.position = "top") +
        panel_border(color = 1) +
        labs(x = "Generation", y = "F")
    
    
    p3 <- df_robustness_density %>% 
        filter(Transfer == 40) %>% 
        ggplot(aes(x = CommunityPhenotype)) +
        geom_density(aes(fill = Treatment, color = Treatment), alpha = 0.4, position = "identity") +
        geom_vline(data = df_robustness_density_vline, aes(xintercept = CommunityPhenotype, color = Treatment), linetype = 2, size = 1) +
        scale_x_continuous(limits = c(-610, 2300), breaks = seq(-500, 2500, 500), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        #scale_fill_discrete(name = "", labels = c("Selected Community", "Synthetic Community", "Screen")) +
        coord_flip() +
        theme_cowplot() +
        guides(color = F, fill = F) +
        theme(legend.position = "top", legend.title = element_blank(), axis.title.y = element_blank(),
            axis.text.x = element_blank()) +
        panel_border(color = 1, size = 1) +
        labs(x = "F", y = "Density") 
    
    
    # resistance versus f
    temp <- df_robustness_aggregated %>%
        filter(Protocol == "iteration_3" | Protocol == "simple_screening") %>%
        filter(Perturbation == "migration") %>%
        filter(!(Protocol == "simple_screening" & InitialCommunity == "synthetic_community")) %>%
        unite(col = "Treatment", c(Protocol, InitialCommunity))
    
    p7 <-  temp %>% 
        ggplot(aes(x = f20Median, y = robustnessMedian, color = Treatment)) +
        geom_point(size = 2, shape = 21) +
        scale_color_discrete(name = "", labels = c("selected_community", "synthetic_community", "screen")) +
        coord_cartesian(xlim = c(0, 2500)) +
        theme_cowplot() +
        guides(color = F) +
        theme(legend.position = "top") + #, plot.margin = unit(c(0,0,0,0), "cm")) +
        panel_border(color = 1, size = 1) +
        labs(x = "F", y = "RS")
    
    
    # Boxplot of resistance
    p8 <- df_robustness_aggregated_std %>%
        filter(Protocol == "iteration_3") %>% 
        filter(Perturbation == "migration") %>% 
        ggplot(aes(x = Protocol, y = robustnessMedian_std, color = InitialCommunity)) +
        geom_hline(yintercept = 0, linetype = 2) +
        geom_boxplot(outlier.size = 2., outlier.shape = 21) +
        geom_point(position = position_jitterdodge(), size = 2, shape = 21) +
        scale_color_manual(values = c("#F8766D", "#00BA38"), labels = name_iteration_protocol[2:5]) +
        theme_cowplot() +
        theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
            legend.title = element_blank(), legend.position = "top") +
        panel_border(color = 1, size = 1) +
        guides(color = F) +
        labs(y = expression(RS[DS]-RS[screen]))
    
}








