#' Selection matrix 
library(tidyverse)
library(data.table)
library(cowplot)

# Read protocols
sm <- fread("../data/TableS1_matrix_data.txt") %>% mutate(Pipetting = factor(Pipetting)) %>% as_tibble
sm$Col <- 25-sm$Col # So rank 1st = the one with highest function

# Panel A. Identity matrix
sm_eye <- tibble(Row = rep(1:24, 24), Col = rep(1:24, each = 24), Pipetting = 0, SelectionAlgrithm = "identity", Protocol = "identity")
sm_eye$Pipetting[(sm_eye$Row==sm_eye$Col)] <- 1 
d = 0.001 # Dilution factor
sm_eye$Pipetting <- factor(sm_eye$Pipetting * d)

plot_selection_matrix <- function(sm, show_label = FALSE) {
    ggplot(sm, aes(x = Col, y = Row, fill = Pipetting)) +
        geom_tile(color = "black") +
        scale_fill_manual(values = c("black", "white")) +
        scale_x_continuous(expand = c(0,0), breaks = 1:24, position = "top") +
        scale_y_reverse(expand = c(0,0), breaks = 1:24) +
        theme(legend.title = element_blank()) +
        {if (show_label == FALSE) {theme_void() + theme(legend.position = "none", axis.title = element_blank(), axis.text = element_blank())}} +
        panel_border(size = 1, color = 1) +
        labs(x = "Rank of Parent Community (v)", y = "Offspring Community (u)") +
        NULL 
}

p_A <- plot_selection_matrix(sm_eye, show_label = T) + 
    theme(legend.position = "bottom") + 
    ggtitle("No-selection (identity matrix)")


# Panel B. Example of propagule, migran-pool strategies
p_propagule <- plot_selection_matrix(filter(sm, SelectionAlgorithm == "select_top25percent"))
p_migrant <- plot_selection_matrix(filter(sm, SelectionAlgorithm == "pool_top25percent"))
p_propagule_subline <- plot_selection_matrix(filter(sm, SelectionAlgorithm == "Raynaud2019a")) + 
    geom_hline(yintercept = seq(0.5, 23.5, by = 8), color = "red") + 
    geom_vline(xintercept = seq(0.5, 23.5, by = 8), color = "red")
p_migrant_subline <- plot_selection_matrix(filter(sm, SelectionAlgorithm == "Raynaud2019b")) +
    geom_hline(yintercept = seq(0.5, 23.5, by = 8), color = "red") + 
    geom_vline(xintercept = seq(0.5, 23.5, by = 8), color = "red")

p_B <- plot_grid(p_propagule, p_migrant, p_propagule_subline, p_migrant_subline,
    labels = c("Propagule", "Migrant-pool", "Propagule with sublines", "Migrant-pool with sublines"), 
    nrow = 2, scale = c(.7, .7, .7, .7), align = "hv", axis = "tb", label_fontface = "plain", label_x = c(0,-.03,-.2, -.23))

p_top_row <- plot_grid(p_A, p_B, nrow = 1, labels = c("A", "B"), scale = c(.9, 1))

# Panel C. Example of protocol with propagule
p_migrant <- plot_selection_matrix(filter(sm, Protocol == "Swenson2000a"))
p_migrant_control <- plot_selection_matrix(filter(sm, Protocol == "Swenson2000a_control"))
p_eye <- plot_selection_matrix(sm_eye)
p_C <- ggdraw() + draw_image("../Plots/Cartoons/FigS13C.png")


#
p_S13 <- plot_grid(p_top_row, p_C, ncol = 1, labels = c("", "C"))
ggsave("../Plots/FigS13.png", p_S13, width = 12, height = 12)


