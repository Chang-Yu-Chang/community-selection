#' Table S1 Experimental protocols
library(tidyverse)
library(data.table)
library(cowplot)
library(flextable)
library(officer)

# Read selection matrices and protocols
t1 <- fread("../Data/Tables/TableS1.csv") # Curated protocols 
sm <- fread("../Data/Tables/TableS1_matrix_data.txt") %>% mutate(Pipetting = factor(Pipetting))  # Selection matrix data
list_protocols = c("Swenson2000a", "Blouin2015", "Panke-Buisse2015", "Jochum2019", "Raynaud2019b", "Mueller2019", "Wright2019", "Swenson2000b", "Arora2019", "Raynaud2019a", "Chang2020a", "Chang2020b", "Swenson2000a_control", "Blouin2015_control", "Raynaud2019b_control", "Wright2019_control", "Swenson2000b_control", "Arora2019_control", "Raynaud2019a_control", "Chang2020a_control", "Chang2020b_control")
sm$Col <- 25-sm$Col # So rank 1st = the one with highest function

# Plot selection matrix
plot_selection_matrix <- function(sm, show_label = FALSE) {
    g <- ggplot(sm, aes(x = Col, y = Row, fill = Pipetting)) +
        geom_tile(color = "black") +
        scale_fill_manual(values = c("black", "white")) +
        scale_x_continuous(expand = c(0,0), breaks = 1:24, position = "top") +
        scale_y_reverse(expand = c(0,0), breaks = 1:24) +
        theme(legend.title = element_blank()) +
        {if (show_label == FALSE) theme_void() + theme(legend.position = "none", axis.title = element_blank(), axis.text = element_blank())} +
        panel_border(size = 1, color = 1) +
        labs(x = "Rank of Parent Community (v)", y = "Offspring Community (u)") +
        NULL 
    
    if (sm$Protocol[1] %in% c("Raynaud2019a", "Raynaud2019b", "Raynaud2019a_control", "Raynaud2019b_control")) g <- g + geom_hline(yintercept = seq(0.5, 23.5, by = 8), color = "red", lwd = 2) + geom_vline(xintercept = seq(0.5, 23.5, by = 8), color = "red", lwd = 2)
    if (sm$Protocol[1] %in% c("Arora2019", "Arora2019_control")) g <- g + geom_hline(yintercept = seq(0.5, 23.5, by = 3), color = "red", lwd = 2) + geom_vline(xintercept = seq(0.5, 23.5, by = 3), color = "red", lwd = 2)
    
    return(g)
}

p_list <- rep(list(NA), length(list_protocols))
for (i in 1:length(p_list)) p_list[[i]] <- plot_selection_matrix(filter(sm, Protocol == list_protocols[i]))
names(p_list) <- list_protocols
for (i in 1:length(p_list)) ggsave(filename = paste0("../Plots/Cartoons/TableS1_", names(p_list)[i], ".png"), plot = p_list[[i]], width = 7, height = 7)

# Make flextable
ft1 <- t1 %>%
    select(-Reference) %>% 
    mutate(`Selection matrix` = "") %>%
    mutate(`Random selection matrix` = "") %>%
    flextable()


# Insert selection matices
for (k in 1:12) {
    selection_matrix <- list_protocols[k] 
    random_matrix <- paste0(selection_matrix, "_control")
    
    ft1 <- ft1 %>% mk_par(i = k, j = "Selection matrix", value = as_paragraph(as_image(src = paste0("../Plots/Cartoons/TableS1_", selection_matrix,".png"), width = 1, height = 1)), part = "body")
    if (length(list.files("../Plots/Cartoons/", pattern = random_matrix)) > 0) {
        ft1 <- mk_par(ft1, i = k, j = "Random selection matrix", value = as_paragraph(as_image(src = paste0("../Plots/Cartoons/TableS1_", random_matrix,".png"), width = 1, height = 1)), part = "body") 
    }
}

ft1 <- ft1 %>% 
    merge_v(j = "Strategy") %>% 
    valign(j = 1:13, valign = "top", part = "all") %>% 
    align(j = 1:13, align = "left", part = "all") %>% 
    width(j = 3:13, width = 4)

# Add foodnote
ft1 <- ft1 %>% 
    footnote(i = 1, j = "Selection matrix",
        value = as_paragraph("For illustration convenience, the seletion matrices shown here are designed for 24 communities rather than 96 that are used otherwise in the main text."),
        ref_symbols = c("a"), part = "header") %>%
    footnote(i = 1, j = "Random selection matrix",
        value = as_paragraph("In our simulation, a new random selection matrix for a protocol is drawn every time it needs to transfer from parents to offsprings, so they differ from generation to generation."),
        ref_symbols = c("b"), part = "header") %>% 
    footnote(i = c(5,9,10), j = c("Selection matrix", "Random selection matrix"),
        value = as_paragraph("The red lines indicate the division of multiple parallel sublines."),
        ref_symbols = c("c"), part = "body")

save_as_image(ft1, "../Plots/TableS1.png")












