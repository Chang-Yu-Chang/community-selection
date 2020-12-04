# Supplement method figure plot

library(tidyverse)
library(data.table)
library(cowplot)

# Functional response
t1 <- function(x, a=1) a*x
t2 <- function(x, a=1, sigma=1) x / (1 + x/sigma)
t3 <- function(x, a=1, sigma=1, n=2) (x^n) / (1 + (x^n)/sigma) 

p1 <- tibble(x = seq(0, 5, by = 0.1)) %>% 
    mutate(type1 = t1(x), type2 = t2(x), type3 = t3(x)) %>%
    pivot_longer(cols = starts_with("type"), names_to = "Type", values_to = "Value") %>%
    ggplot(aes(x = x, y = Value, color = Type)) +
    geom_line() +
    scale_color_discrete(labels = c("Type I", "Type II", "Type III")) +
    theme_cowplot() +
    theme(legend.position = c(0.1, 0.8), legend.title = element_blank()) +
    labs(x = "Resource concentration", y = "Per-capita growth rate")
p1

# Metacommunity sampling
power_law <- function(x, a=0.01) a *x^(a-1)
log_normal <- function(x, mean=8, sd=8) dlnorm(x, meanlog = mean, sdlog = sd)

p2 <- tibble(x = seq(1, 100, by = 1)) %>% 
    mutate(power_law  = power_law(x), log_normal = log_normal(x)) %>%
    pivot_longer(cols = -x, names_to = "Distribution", values_to = "Value") %>%
    ggplot(aes(x = x, y = Value, color = Distribution)) +
    geom_line() +
    scale_y_log10() +
    scale_color_discrete(labels = c("Log-normal", "Power law")) +
    theme_cowplot() +
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank()) +
    labs(x = "Rank", y = expression(paste(log[10], "(Relative Abundance)")))
p2

# Phi distribution
phi1 <- function(x, mean=0, sd=1) dnorm(x, mean, sd)
phi2 <- function(x, mean=1, sd=1) dnorm(x, mean, sd)
phi3 <- function(x, min=0, max=1) dunif(x, min, max)

p3 <- tibble(x = seq(-3, 3, by = 0.1)) %>% 
    mutate(phi1  = phi1(x), phi2 = phi2(x), phi3 = phi3(x)) %>%
    pivot_longer(cols = -x, names_to = "Distribution", values_to = "Value") %>%
    ggplot() +
    geom_line(aes(x = x, y = Value, color = Distribution)) +
    scale_y_continuous(limits = c(0,1), breaks = c(0, 0.5, 1)) +
    scale_color_discrete(labels = c(expression(paste(phi[i], "~Norm(mean=0, sd=1)")), 
        expression(paste(phi[i], "~Norm(mean=1, sd=1)")),
        expression(paste(phi[i], "~Unit(min=0, max=1)")))) +
    theme_cowplot() +
    theme(legend.position = c(0.05, 0.8), legend.title = element_blank()) +
    labs(x = expression(phi[i]), y = "Density")
p3

# Cost function
df <- fread('cost_function-1.txt')

p4 <- df %>%
    ggplot(aes(x = PerCapitaFunction, y = g)) +
    geom_point(shape = 21, size = 2) +
    scale_x_continuous(limits = c(0,1), breaks = c(0, 0.5, 1)) +
    scale_y_continuous(limits = c(0,1), breaks = c(0, 0.5, 1)) +
    theme_cowplot() +
    labs(x = expression(phi[i]), y = "g")
p4


# 
p <- plot_grid(p1,p2,p3,p4, nrow = 2, align = "hv")


ggsave("../Plots/Supp_Method.png", p, width = 10, height = 10)












