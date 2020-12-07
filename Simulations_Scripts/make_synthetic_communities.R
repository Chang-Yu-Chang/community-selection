#' Subset the selected community

suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(data.table)))

args <- commandArgs(TRUE)
input_csv <- args[1] # Input iteration csv

df_input <- fread(input_csv) %>% as_tibble
seeds <- unique(df_input$seed)
#selected_function <- unique(df_input$selected_function)
data_directory <- unique(df_input$output_dir)
name_prefix <- strsplit(data_directory, split = "/")[[1]] %>% `[`(length(.)) %>% gsub("(iteration_)|(independent_)|(robustness_)", "", .)

for (seed in seeds){
    for (protocol in c("iteration_simple_screening", paste0("iteration_", c(1,5,6)))){
        cat("seed =", seed, "\t", protocol, "\n")
        
        # Overall settings
        n_directed_selected <- length(grep(paste0(protocol, "_round\\d+-", seed, "$"), df_input$exp_id, value = T)) - 3 # Total round of directed selection; default = 20
        n_transfer_round <- unique(df_input$n_transfer) # Number of transfer between two selection rounds; default = 20
        
        
        # Find high performing isolates; read monoculture data 
        df_mono <- fread(paste0(sub("iteration", "independent", data_directory), name_prefix, "-monoculture-", seed, "_function.txt")) %>% as_tibble
        df_per_capita <- df_mono %>%
            filter(Transfer == 0) %>%
            mutate(PerCapitaFunction = CommunityPhenotype / Biomass,
                ID = as.numeric(sub("W", "", Well))) %>%
            select(ID, PerCapitaFunction)
        
        # List of data of selected community from each protocol
        # Automatcally finds the last rounds
        if (grepl("screen", protocol)) {
            data_algorithms_func <- list.files(data_directory, pattern = paste0(protocol, "-", seed, "_function.txt"), full.names = T)
            data_algorithms_comp <- list.files(data_directory, pattern = paste0(protocol, "-", seed, "_composition.txt"), full.names = T)
        } else {
            data_algorithms_func <- list.files(data_directory, pattern = paste0(protocol, "_round", n_directed_selected+3, "-", seed, "_function.txt"), full.names = T)
            data_algorithms_comp <- list.files(data_directory, pattern = paste0(protocol, "_round", n_directed_selected+3, "-", seed, "_composition.txt"), full.names = T)
        }
        
        # Make pairs of selected and synthetic communities
        j = 1
        # Selected community, the one with max function
        df_selected <- fread(data_algorithms_func[j]) %>%
            filter(Transfer == max(Transfer)) %>%
            filter(CommunityPhenotype == max(CommunityPhenotype)) %>%
            select(exp_id, Well, Transfer)
        
        ## Subset for seleted community's composition
        df_selected_comm <- fread(data_algorithms_comp[j]) %>% as_tibble() %>%
            right_join(df_selected, by = c("exp_id", "Transfer", "Well"))
        
        
        ## Write file
        temp_name <- paste0(sub("_function.txt", "", data_algorithms_func[j]), "-", "selected_community.txt")
        df_selected_comm %>%
            mutate(exp_id = paste0("selected_community-", exp_id)) %>%
            fwrite(file = temp_name)
        
        # Synthetic community
        df_synthetic_comm <- df_selected_comm
        
        ## Subset for the the best isolates in the pool
        best_isolates <- 
            df_per_capita %>%
            arrange(desc(PerCapitaFunction)) %>%
            # Subset the highest performing isoaltes
            slice(1:sum(df_selected_comm$Type == "consumer")) %>%
            pull(ID)
        
        ## Consumer
        richness <- sum(df_synthetic_comm$Type == "consumer")
        total_biomass <- sum(df_selected_comm$Abundance[df_synthetic_comm$Type == "consumer"])
        df_synthetic_comm$Abundance[df_synthetic_comm$Type == "consumer"] <- rep(total_biomass/richness, richness)
        df_synthetic_comm$ID[df_synthetic_comm$Type == "consumer"] <- best_isolates
        
        ## Make the compostition df (consumer + resource)
        df_synthetic_comm <- bind_rows(df_synthetic_comm %>% filter(Type == "consumer"),
            df_synthetic_comm %>% filter(Type == "R0"))
        
        ## Write file
        temp_name <- paste0(sub("_function.txt", "", data_algorithms_func[j]), "-", "synthetic_community.txt")
        df_synthetic_comm %>%
            mutate(exp_id = paste0("synthetic_community-", exp_id)) %>%
            fwrite(file = temp_name)
        
    }}
