#' Make the input mapping files

suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(data.table)))

test_small_set <- F
test_small_pool <- F
pool_csv <- T

seeds = 1:20 # Random seed. Default 1:100
cat("\nTotal seeds are = ", seeds, "\n")
#data_directory = "/Users/chang-yu/Desktop/Lab/community-selection/Data/test/"
data_directory = "/home/cc2553/project/community-selection/data/"
mapping_file_directory = "../Data/Mapping_Files/"
list_treatments <- tibble(
    exp_id = c("f1_additive", "f1a_additive", "f1b_additive_cost", "f1b_additive_phi1", "f1b_additive_phi2",
               "f1c_additive_sampling1", "f1c_additive_sampling2", "f1d_additive_medium1", "f1d_additive_medium2", "f1e_additive_response1",
               "f1e_additive_response2", "f2_interaction", "f2a_interaction", "f5_invader_suppression", "f6_target_resource"),
    selected_function = c("f1_additive", "f1a_additive", rep("f1_additive", 9),
                          "f2_interaction", "f2a_interaction",
                          "f5_invader_suppression","f6_target_resource"),
    ruggedness = c(NA, 0.8, rep(NA, 10), 0.8, rep(NA, 2)),
    rich_medium = c(rep(T, 8), F, rep(T, 6)),
    n_inoc = c(rep(10^6, 8), 10^3, rep(10^6, 6)),
    n_migration = c(rep(10^6, 8), 10^3, rep(10^6, 6)),
    l = c(rep(0, 7), 0.5, 0.5, rep(0, 6)),
    dilution = c(rep(0.001, 8), 0.001, rep(0.001, 6)),
    n_propagation = c(rep(1, 8), 20, rep(1, 6)),
    phi_distribution = c(rep("Norm", 2), "Uniform", "Norm", "Uniform", rep("Norm", 10)),
    phi_mean = c(rep(0,3), 1, rep(0, 11)),
    phi_sd = c(rep(1, 15)),
    phi_lower = c(rep(0, 15)),
    phi_upper = c(rep(1, 15)),
    cost_distribution = c(rep("Norm", 2), "Uniform", rep("Norm", 12)),
    cost_mean = c(rep(0, 15)),
    cost_sd = c(rep(0, 15)),
    cost_lower = c(rep(0, 15)),
    cost_upper = c(rep(1, 15)),
    metacommunity_sampling = c(rep("Power", 5), "Lognormal", "Default", rep("Power", 8)),
    power_alpha = rep(0.01, 15),
    lognormal_mean = rep(8, 15),
    lognormal_sd = rep(8, 15),
    response = c(rep("type III", 8), "type III", "type I", "type II", rep("type III", 4)),
    sigma_max = c(rep(1,8), 1, rep(1, 6)) # default = 1. sigma max for functional response
)
#list_treatments <- filter(list_treatments, selected_function == "f5_invader_suppression")

make_input_csv <- function(...){
    args = list(...)

    # List of parameters
    df_default <-
        data.frame(
            stringsAsFactors = FALSE,

            selected_function = "f1_additive", #Function that is under selection
            protocol = "simple_screening", #protocol to implement
            seed = 1, #Seed for species poo l
            exp_id = paste("f1_additive", "simple_screening", 1, sep = "-"), # ID for simulation (will determine filenames
            overwrite_plate = NA, # If not NA, then must be a text file. Overwrite the initial plate with this composititon saved in this text file
            passage_overwrite_plate = F, # If overwrite_plate != NA, set TRUE if the overwrite_plate is at equilibrium and need an addititonal transfer
            output_dir = "data/", # Output directory. Default is a data subfolder
            save_function = T, # Save Function data
            save_composition = T, # Save Composition Data
            save_plate = F, #Save initial plate
            function_lograte = 1, #How often do you save the function in transfers
            composition_lograte = 20, #How often do you save the compoistion in transfers

            #Experiment Paramaters (applies to for all protocols)

            scale = 1000000, # Number of cells when N_i = 1
            n_inoc = 1000000, # Number of cells sampled from the regional species at start
            rich_medium = T, #Whether to generate a rich medium sampled from a a random distribution or a minimal media with only a single resource
            monoculture = F, # whether to run simple screening with monoculture
            dilution = 0.001, #Dilution factor at every transfers
            n_wells = 96, # Number of welss on a plate
            n_propagation = 1, # Incubation time
            n_transfer = 40, #Number of Transfers total number of transfers
            n_transfer_selection = 20, #Number of tranfers implementing selection regime
            metacommunity_sampling = "Power", # {"Power", "Lognormal", "Default"} Sampling method for initial metacommunity
            power_alpha = NA, # Default = 0.01
            lognormal_mean = NA, # Default = 8
            lognormal_sd = NA, # Default = 8


            #Paramaters for community function, #paramaters that determine properties of function
            phi_distribution = "Norm", # {"Norm", "Uniform"}
            phi_mean = 0, #
            phi_sd = 1, # Standard deviation for drawing specifc speices/interaction function
            phi_lower = 0,
            phi_upper = 1,
            ruggedness = 0.8, # (1-ruggedness) percent of function are set to 0
            function_ratio = 1, # Scaling factor between species- and interaction-specific function variances
            binary_threshold = 1, #Threshold for binary functions
            g0 = 1, # The baseline conversion factor of biomass per energy
            cost_distribution = "Norm", # {"Norm", "Uniform"}
            cost_mean = 0, # Mean fraction of cost feeded into a gamma distribution. Suggested up to 0.05
            cost_sd = 0, # Sd fraction of cost feeded into a gamma distribution. cost_sd = 0 if cost_mean = 0, cost_sd= 0.01 if cost_mean >0
            cost_lower = 0, # Lower bound for cost if cost_distribution="Uniform"
            cost_upper = 1, # Upper bound for cost if cost_distribution="Uniform"
            invader_index =  2,
            invader_sampling = "Gamma",
            invader_strength = 10,
            target_resource = NA, # Target resource production when selected_function=f6_target_resourece

            #Paramaters for Directed Selection (for directed selection protocols that can't be coded up in experiment paramaters)

            directed_selection = F, # If true whenever select_top is selected the highest performing Community is propagated asexually and some kind of pertubations can ne applied
            knock_out = F, #If True performs knock out pertubations
            knock_in = F, #If True performs knock in pertubation
            knock_in_threshold = NA, # value determines threshold for isolates to knock in, #If NA isolates are chosen at random.
            bottleneck = F, #If True perform bottleneck pertubations
            bottleneck_size = NA, #Magnitude of bottleneck. If not set it default to dilution
            migration = F, #If true perform migration pertubations
            n_migration = NA, # Number of cells to migration in the directed selection
            s_migration = NA, # Number of species to migrate. If s_migration is NA defaults to power law migration (so this is normal).
            coalescence = F, #If true perform coalescence pertubation
            frac_coalescence = NA, # fraction of coalesced community that is champion. Defaults to 0.5 if NA
            resource_shift = F, #If true performs resource pertubations
            r_type = NA, # Type of resource pertubation. rescale_add, rescale_remove, add, remove, old. if NA defaults to resource swap
            r_percent = NA, # Tunes the magnitude of resource pertubation if NA does not perform resource pertubation

            #Paramaters for community simulator package, note that we have split up a couple of paramaters that are inputed as list (SA and SGen). In the mapping file
            #if paramater is set as NA it takes the default value in community_simulator package. Also some paramaters could actually be inputed as lists but this is beyond the scope of this structure of mapping file i.e m, w,g r

            sampling = "Binary_Gamma", #{'Gaussian','Binary','Gamma', 'Binary_Gamma'} specifies choice of sampling algorithm
            sn = 2100, #number of species per specialist family
            sf = 1, #number of specialist families, # note SA = sn *np.ones(sf)
            Sgen = 0, #number of generalist species
            rn = 90, #number of resources per resource clas
            rf = 1, #number of resource classes, #Note RA = rn*np.ones(rf)
            R0_food = 1000, #Total amount of supplied food
            food = NA, #index of food source (when a single resource is supplied externally). ONly work for single-resource medium
            supply = NA, #resource supply (see dRdt)
            muc = NA, #mean sum of comnsumption rate
            sigc = NA, #Standard deviation of sum of consumption rates for Gaussian and Gamma models
            c0 = NA, #Sum of background consumption rates in binary model
            c1 = NA, #Specific consumption rate in binary model
            q = NA, #preference strength
            sparsity = NA, #Effective sparsity of metabolic matrix (between 0 and 1)
            fs = NA, #Fraction of secretion flux with same resource type
            fw = NA , #Fraction of secretion flux to 'waste' resource
            g = NA, #energy to biomass conversion
            w = NA, #resource energy value
            l = 0, # Leakage fraction
            m = 0, # Minimal resurce uptake; mortality
            n = NA, #hill coefficient when n= 1 response is type 2
            response = "type III", #functional response (see dRdt)
            sigma_max = NA, # default = 1. sigma max for functional response
            regulation = NA, #metabolic regulation (see dRdt)
            nreg = NA, #Hill coefficient that tunes steepness. of metabolic regulation.
            tau = NA, # external resource supply  rate (for chemostat)
            r = NA, #renewal rate for self renewing resources
            S = 100 # number of species in the initial community, legacy of the community-simulator

        )


    argument_names <- colnames(df_default)
    # If there is no change, output a line of default
    if (length(args) == 0) {
        output_row <- df_default
    } else { # If there is any variable specified, change that
        output_row <- df_default
        # The variable has to be in the argument list
        to_changed_args <- names(args)
        if (!all(to_changed_args %in% argument_names)) stop("Errors: arguments do not exist in the list")
        for (i in 1:length(to_changed_args)) output_row[,to_changed_args[i]] <- args[[i]][1]

        # Dependency
        ## Set exp_id names by the seed, selected function, and protocol (and directed selection type if protocol is directed selection)
        output_row$exp_id = paste(output_row$selected_function, output_row$protocol, output_row$seed, sep = "-")

        ## Monoculture
        if (output_row$monoculture == TRUE) {
            if (output_row$protocol != "simple_screening") stop("Errors: monoculture plate has to be simple_screening")
            #output_row$protocol = "monoculture"
            output_row$exp_id = paste(output_row$selected_function, "monoculture", output_row$seed, sep = "-")
        }

        ## Cost per function. Fixed sd of cost is specified
        if (output_row$cost_mean != 0) output_row$cost_sd <- 0.01

        ## Check on the dependency of arguments on directed selection
        list_directed_selections <- c("knock_out", "knock_in", "bottleneck", "migration", "coalescence", "resource_shift")

        if (any(unlist(output_row[,list_directed_selections]))) {
            # Set the flag TRUE
            #output_row$protocol <- "directed_selection"
            output_row$directed_selection <- TRUE

            # Check on the dependency of arguments. For example, bottleneck_size is not NA when bottleneck is TRUE
            if (all(output_row[list_directed_selections] == FALSE)) stop("Errors: A directed selection approach must be speicified")

            # exp_id
            if (!("exp_id" %in% names(args))) output_row$exp_id = paste(output_row$selected_function, output_row$protocol, list_directed_selections[unlist(output_row[,list_directed_selections])], output_row$seed, sep = "-")

            if (output_row$knock_in == TRUE) {
                temp1 <- output_row$knock_in_threshold
                if (is.na(temp1)) temp1 <- 0.95
                output_row$exp_id = paste(output_row$selected_function, output_row$protocol, "knock_in", paste0("p", temp1*100), output_row$seed, sep = "-")
            } else if (output_row$bottleneck == TRUE) {
                temp1 <- output_row$bottleneck_size
                if (is.na(temp1)) temp1 <- output_row$dilution
                output_row$exp_id = paste(output_row$selected_function, output_row$protocol, "bottleneck", 1/temp1, output_row$seed, sep = "-")
            } else if(output_row$migration == TRUE) {
                temp1 <- output_row$s_migration
                temp2 <- output_row$n_migration
                if (is.na(temp1)) temp1 <- "log"
                if (is.na(temp2)) temp2 <- 1000000
                output_row$exp_id = paste(output_row$selected_function, output_row$protocol, "migration", temp1, temp2, output_row$seed, sep = "-")
            } else if(output_row$coalescence == TRUE) {
                temp1 <- output_row$frac_coalescence
                if (is.na(temp1)) temp1 <- 0.5
                output_row$exp_id = paste(output_row$selected_function, output_row$protocol, "coalescence", paste0("p", temp1*100), output_row$seed, sep = "-")
            } else if(output_row$resource_shift == TRUE) {
                temp1 <- output_row$r_type
                temp2 <- output_row$r_percent
                if (is.na(temp1)) temp1 <- "add"
                if (is.na(temp2)) temp2 <- 0.01
                output_row$exp_id = paste(output_row$selected_function, output_row$protocol, "resource_shift", temp1, paste0("p", temp2*100), output_row$seed, sep = "-")
            }
        }

        # Check on the possible bug. For example, n_transfer must be larger than n_transfer_selection
        if (output_row$n_transfer < output_row$n_transfer_selection) stop("Errors: n_transfer must be greater than n_transfer_selection")
        if (output_row$n_transfer < output_row$function_lograte) stop("Errors: n_transfer must be greater than the function_lograte")
        if (output_row$n_transfer < output_row$composition_lograte) stop("Errors: n_transfer must be greater than the composition_lograte")
        #if (output_row$protocol != "directed_selection" & output_row$directed_selection == T) stop("protocol name needs to be changed to directed selection")

        # Placeholder for checking whether the protocol is available

    }

    # Modify the parameter format so it's readible by python
    output_row[sapply(output_row, isTRUE)] <- "True"
    output_row[sapply(output_row, isFALSE)] <- "False"

    # If exp_id speficied in the arguments, use it
    if ("exp_id" %in% names(args)) output_row$exp_id <- args$exp_id

    # Turn off scientific notation printout for large / small fractions numeric paramters
    for (i in 1:length(output_row[1,])) if (is.numeric(output_row[1,i])) output_row[1,i] <- format(output_row[1,i], scientific = FALSE)

    #
    output_row$seed <- as.numeric(output_row$seed)
    output_row$composition_lograte <- as.numeric(output_row$composition_lograte)
    output_row$n_transfer <- as.numeric(output_row$n_transfer)
    output_row$n_transfer_selection  <- as.numeric(output_row$n_transfer_selection)

    return(output_row)
}
input_independent_wrapper <- function (i, treatment) {
    selected_function = treatment$selected_function

    list_algorithms <- c(
        "select_top25", "select_top10", "pool_top25", "pool_top10",
        "Blouin2015", "Blouin2015_control", "Jochum2019", "Mueller2019", "Panke_Buisse2015", "Swenson2000a", "Swenson2000a_control", "Swenson2000c", "Wright2019", "Wright2019_control",
        "Swenson2000b", "Swenson2000b_control", "Chang2020a", "Chang2020a_control", "Chang2020b", "Chang2020b_control",
        "Arora2019", "Arora2019_control", "Raynaud2019a", "Raynaud2019a_control", "Raynaud2019b", "Raynaud2019b_control"
    )
    df <- make_input_csv(); df <- df[-1,]

    # Screen
    temp <- make_input_csv(selected_function = selected_function, seed = i)
    df <- bind_rows(df, temp)

    # Monoculture
    temp <- make_input_csv(selected_function = selected_function, monoculture = T, seed = i)
    df <- bind_rows(df, temp)

    # Experimental protocol
    temp_list <- rep(list(NA), length(list_algorithms))
    for (j in 1:length(list_algorithms)) temp_list[[j]] <- make_input_csv(selected_function = selected_function, protocol = list_algorithms[j], seed = i)
    df <- bind_rows(df, temp_list)


    # Directed selection
    ## Knock out
    df <- bind_rows(df, make_input_csv(selected_function = selected_function, protocol = "directed_selection", knock_out = TRUE, composition_lograte = 1, seed = i) %>%
                        mutate(exp_id = paste(selected_function, "knock_out", i, sep = "-")))

    ## Knockin
    knock_in_threshold <- c(0, 0.95)
    for (j in 1:length(knock_in_threshold)) {
        df <- bind_rows(df, make_input_csv(selected_function = selected_function, protocol = "directed_selection", knock_in = TRUE, knock_in_threshold = knock_in_threshold[j], seed = i) %>%
                            mutate(exp_id = paste(selected_function, "knock_in", j, i, sep = "-")))
    }

    ## Bottleneck
    #bottleneck_size <- c(0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001)
    bottleneck_size <- c(-5:-1 %>% 10^., 1e-6, 2e-6, 4e-6, 8e-6 , 1.6e-5,3.2e-5,6.4e-5,1.28e-4, 2.56e-4,5.12e-4, 1.024e-3, 5e-7, 2.5e-7, 1.25e-7)
    for (j in 1:length(bottleneck_size)) {
        df <- bind_rows(df, make_input_csv(selected_function = selected_function,protocol = "directed_selection", bottleneck = TRUE, bottleneck_size = bottleneck_size[j], seed = i) %>%
                            mutate(exp_id = paste(selected_function, "bottleneck", j, i, sep = "-")))
        df <- bind_rows(df, make_input_csv(selected_function = selected_function, protocol = "select_top25", bottleneck = TRUE, bottleneck_size = bottleneck_size[j], seed = i) %>%
                            mutate(exp_id = paste(selected_function, "bottleneck_select_top25", j, i, sep = "-")))
        df <- bind_rows(df, make_input_csv(selected_function = selected_function, protocol = "select_top10", bottleneck = TRUE, bottleneck_size = bottleneck_size[j], seed = i) %>%
                            mutate(exp_id = paste(selected_function, "bottleneck_select_top10", j, i, sep = "-")))
        df <- bind_rows(df, make_input_csv(selected_function = selected_function, protocol = "pool_top25", bottleneck = TRUE, bottleneck_size = bottleneck_size[j], seed = i) %>%
                            mutate(exp_id = paste(selected_function, "bottleneck_pool_top25", j, i, sep = "-")))
        df <- bind_rows(df, make_input_csv(selected_function = selected_function, protocol = "pool_top10", bottleneck = TRUE, bottleneck_size = bottleneck_size[j], seed = i) %>%
                            mutate(exp_id = paste(selected_function, "bottleneck_pool_top10", j, i, sep = "-")))
    }

    ## Migration
    s_migration <- c(NA, 1, 2, 4, 8, 16, 32)
    n_migration <- rep(1000000, length(s_migration))
    for (j in 1:length(s_migration)) {
        df <- bind_rows(df, make_input_csv(selected_function = selected_function, protocol = "directed_selection", migration = TRUE, s_migration = s_migration[j], n_migration = n_migration[j], seed = i) %>%
                            mutate(exp_id = paste(selected_function, "migration", j, i, sep = "-")))
    }

    ## Coalescence
    frac_coalescence <- c(0.5)
    for (j in 1:length(frac_coalescence)) {
        df <- bind_rows(df, make_input_csv(selected_function = selected_function, protocol = "directed_selection", coalescence = TRUE, frac_coalescence = frac_coalescence[j], seed = i) %>%
                            mutate(exp_id = paste(selected_function, "coalescence", j, i, sep = "-")))

    }

    ## Resource shift
    r_percent <- c(0.01, 0.1, 0.5, 1)
    r_type <- rep("add", length(r_percent)) # resource type is resource_add by default
    for (j in 1:length(r_percent)) {
        df <- bind_rows(df, make_input_csv(selected_function = selected_function, protocol = "directed_selection", resource_shift = TRUE, r_type = r_type[j], r_percent = r_percent[j], seed = i) %>%
                            mutate(exp_id = paste(selected_function, "resource_shift", j, i, sep = "-")))
    }


    df$output_dir <- paste0(data_directory, "independent_", treatment$exp_id, "/")
    df$exp_id <- sub(selected_function, treatment$exp_id, df$exp_id)

    for (j in 3:ncol(treatment)) {
        df[,names(treatment)[j]] = treatment[,names(treatment)[j]]
    }
    # df$cost_mean <- as.character(treatment$cost_mean)
    # df$cost_sd <- as.character(treatment$cost_sd)
    df[is.na(df)] <- "NA"
    return(df)
}
input_iteration_wrapper <- function (i, treatment) {
    selected_function = treatment$selected_function
    n_directed_selected <- 20 # Total round of directed selection; default = 20
    n_transfer_round <- 20 # Number of transfer between two selection rounds; default = 20
    list_seq_ds <- list(
        c("bottleneck", "bottleneck"), #1
        c("migration", "migration"),
        c("bottleneck-and-migration"), 
        c("bottleneck-and-migration"),
        c("bottleneck-and-migration"), #5
        c("bottleneck-and-migration"), #6
        c("bottleneck-and-migration")
    ) %>%
        # Repeat them to the length of number of directed selection rounds
        # Add two rounds of screen in the end
        lapply(function(x) c("simple_screening",rep(x, n_directed_selected/length(x)), rep("simple_screening", 2)))

    df <- make_input_csv(); df <- df[-1,]

    # Screen
    df <- bind_rows(df, make_input_csv(seed = i, exp_id = paste0(treatment$exp_id, "-iteration_simple_screening-", i)))

    # Iterative protocols
    for (k in 1:length(list_seq_ds)) {
        seq_ds <- list_seq_ds[[k]]

        # Tune the perturbation size
        s_migration <- NA
        if (k <= 3) { bottleneck_size <- 1e-4; migration = T;n_migration <- 1e6
        } else if (k == 4) { bottleneck_size <- 1e-13; migration = T; n_migration <- 1e6
        } else if (k == 5) { bottleneck_size <- 1e-4; migration = T; n_migration <- 1e2
        } else if (k == 6) { bottleneck_size <- 1; migration = T; n_migration <- 1e2
        } else if (k == 7) { bottleneck_size <- 1e-13; migration = F; n_migration <- NA}

        # Iterations
        for (j in 1:length(seq_ds)) {
            # Output file id
            exp_id <- paste0(treatment$exp_id, "-iteration_", k, "_round", j, "-", i)


            # Make the protocol
            if (seq_ds[j] == "simple_screening") {
                temp <- make_input_csv(exp_id = exp_id, seed = i)
            } else if (seq_ds[j] == "migration") {
                temp <- make_input_csv(exp_id = exp_id, protocol = "directed_selection", migration = migration, s_migration = s_migration, n_migration = n_migration, seed = i)
            } else if (seq_ds[j] == "bottleneck") {
                temp <- make_input_csv(exp_id = exp_id, protocol = "directed_selection", bottleneck = TRUE, bottleneck_size = bottleneck_size, seed = i)
            } else if (seq_ds[j] == "bottleneck-and-migration") {
                temp <- make_input_csv(exp_id = exp_id, protocol = "directed_selection", bottleneck = TRUE, bottleneck_size = bottleneck_size,
                                       migration = migration, s_migration = s_migration, n_migration = n_migration, seed = i)
            }

            # Remove stabilization phase
            temp$n_transfer <- n_transfer_round
            temp$n_transfer_selection <- n_transfer_round/2
            temp$composition_lograte <- n_transfer_round/2

            # If not the first round, overwrite the plate by previous round, write the plate
            if (j>=2) temp$overwrite_plate <- paste0(data_directory, "iteration_", treatment$exp_id, "/", treatment$exp_id, "-iteration_", k, "_round", j-1, "-", i, "_composition.txt")

            # Overerite_plate is at equilibrium so it has to be passaged one more times before starting the next expeirmental round
            if (!is.na(temp$overwrite_plate)) temp$passage_overwrite_plate <- "True"

            df <- bind_rows(df, temp)
        }

    }

    df$output_dir <- paste0(data_directory, "iteration_", treatment$exp_id, "/")
    df$exp_id <- sub(selected_function, treatment$exp_id, df$exp_id)
    df$selected_function <- selected_function

    for (j in 3:ncol(treatment)) {
        df[,names(treatment)[j]] = treatment[,names(treatment)[j]]
    }
    # df$cost_mean <- as.character(treatment$cost_mean)
    # df$cost_sd <- as.character(treatment$cost_sd)
    df[is.na(df)] <- "NA"
    return(df)
}
input_robustness_wrapper <- function(i, treatment) {
    selected_function = treatment$selected_function
    n_directed_selected <- 20 # Total round of directed selection; default = 20
    n_transfer_round <- 20 # Number of transfer between two selection rounds; default = 20
    list_protocols <- c("iteration_simple_screening", paste0("iteration_", c(3,5)))
    target_communities <- c("selected_community", "synthetic_community")
    list_perturbations <- c("migration", "migration2", "bottleneck", "resource_shift", "knock_out")
    make_input_perturbation <- function (data_directory, sf, id, protocol, n_directed_selected, target_community, perturbation, seed) {
        # Naming convention
        if (grepl("screen", protocol)) {
            overwrite_plate <- paste0(data_directory, "iteration_", id, "/", id, "-", protocol, "-", seed, "-", target_community, ".txt")
        } else {
            overwrite_plate <- paste0(data_directory, "iteration_", id, "/", id, "-", protocol, "_round", n_directed_selected+3, "-", seed, "-", target_community, ".txt")
        }
        exp_id <- paste0(sf, "-", protocol,"-", seed, "-", target_community, "-", perturbation)

        #
        if (perturbation == "migration") {
            temp <- make_input_csv(protocol = "directed_selection", migration = T)
        } else if (perturbation == "migration2") {
            temp <- make_input_csv(protocol = "directed_selection", migration = T, n_migration = 1e2)
        } else if (perturbation == "bottleneck") {
            temp <- make_input_csv(protocol = "directed_selection", bottleneck = T, bottleneck_size = 1e-4)
        } else if (perturbation == "knock_out") {
            temp <- make_input_csv(protocol = "directed_selection", knock_out = T)
        } else if (perturbation == "resource_shift") {
            temp <- make_input_csv(protocol = "directed_selection", resource_shift = T)
        }

        temp$exp_id <- exp_id
        temp$overwrite_plate <- overwrite_plate
        temp$passage_overwrite_plate <- "True"
        temp$composition_lograte = 1
        temp$seed = seed

        return(temp)

    }

    df <- make_input_csv(); df <- df[-1,]
    df <- df %>% mutate(overwrite_plate = as.character(overwrite_plate), passage_overwrite_plate = as.character(passage_overwrite_plate))

    for (j in 1:length(list_protocols)) {
        for (tc in 1:length(target_communities)) {
            for (lp in 1:length(list_perturbations)) {
                temp <- make_input_perturbation(
                    data_directory = data_directory,
                    sf = selected_function,
                    id = treatment$exp_id,
                    protocol = list_protocols[j],
                    n_directed_selected = n_directed_selected,
                    target_community = target_communities[tc],
                    perturbation = list_perturbations[lp],
                    seed = i)
                df <- bind_rows(df, temp)
            }
        }
    }
    df$output_dir <- paste0(data_directory, "robustness_", treatment$exp_id, "/")
    df$exp_id <- sub(selected_function, treatment$exp_id, df$exp_id)
    df$selected_function <- selected_function

    for (j in 3:ncol(treatment)) {
        df[,names(treatment)[j]] = treatment[,names(treatment)[j]]
    }
    # df$cost_mean <- as.character(treatment$cost_mean)
    # df$cost_sd <- as.character(treatment$cost_sd)
    df[is.na(df)] <- "NA"
    return(df)
}

input_independent_list <- rep(list(rep(list(NA), length(seeds))), nrow(list_treatments))
input_iteration_list <- rep(list(rep(list(NA), length(seeds))), nrow(list_treatments))
input_robustness_list <- rep(list(rep(list(NA), length(seeds))), nrow(list_treatments))

for (k in 1:nrow(list_treatments)) {
    cat("\nMaking csv for", list_treatments$exp_id[k], "\n")

    input_independent_list[[k]][[1]] <- input_independent_wrapper(i = 1, treatment = list_treatments[k,])
    input_iteration_list[[k]][[1]] <- input_iteration_wrapper(i = 1, treatment = list_treatments[k,])
    input_robustness_list[[k]][[1]] <- input_robustness_wrapper(i = 1, treatment = list_treatments[k,])

    for (i in seeds) {
        input_independent_list[[k]][[i]] <- input_independent_list[[k]][[1]] %>% mutate(seed = i, exp_id = sub("-\\d$", paste0("-", i), exp_id))
        input_iteration_list[[k]][[i]] <- input_iteration_list[[k]][[1]] %>%
            mutate(seed = i, exp_id = sub("-\\d$", paste0("-", i), exp_id),
                   overwrite_plate = sub("-\\d_composition.txt$", paste0("-", i, "_composition.txt"), overwrite_plate))
        input_robustness_list[[k]][[i]] <- input_robustness_list[[k]][[1]] %>% mutate(seed = i, exp_id = sub("-\\d-s", paste0("-", i, "-s"), exp_id), overwrite_plate = sub("-\\d-s", paste0("-", i, "-s"), overwrite_plate))
    }

    input_independent <- rbindlist(input_independent_list[[k]])
    input_iteration <- rbindlist(input_iteration_list[[k]])
    input_robustness <- rbindlist(input_robustness_list[[k]])

    if (test_small_set) {
        input_independent$n_wells <- 10
        input_iteration$n_wells <- 10
        input_robustness$n_wells <- 10
        if (test_small_pool) {
            input_independent$S <- 10
            input_iteration$S <- 10
            input_robustness$S <- 10
            input_independent$sn <- 100
            input_iteration$sn <- 100
            input_robustness$sn <- 100
        }
        # input_independent$rn <- 20
        # input_iteration$rn <- 20
        # input_robustness$rn <- 20
    }

    input_independent$save_composition <- FALSE
    input_iteration$save_composition <- TRUE
    input_robustness$save_composition <- FALSE

    fwrite(input_independent, paste0(mapping_file_directory, paste0("input_independent_", list_treatments$exp_id[k],".csv")))
    fwrite(input_iteration, paste0(mapping_file_directory, paste0("input_iteration_", list_treatments$exp_id[k],".csv")))
    fwrite(input_robustness, paste0(mapping_file_directory, paste0("input_robustness_", list_treatments$exp_id[k],".csv")))
}


if (pool_csv) {
    cat("\nMaking pooled input_independent.csv\n")
    input_independent <- input_independent_list %>%
        lapply(rbindlist) %>% rbindlist()
    fwrite(input_independent, paste0(mapping_file_directory, "input_independent.csv"))

    cat("\nMaking pooled input_iteration.csv\n")
    input_iteration <- input_iteration_list %>%
        lapply(rbindlist) %>% rbindlist()
    fwrite(input_iteration, paste0(mapping_file_directory, "input_iteration.csv"))

    cat("\nMaking pooled input_robusntess.csv\n")
    input_robustness <- input_robustness_list %>%
        lapply(rbindlist) %>% rbindlist()
    fwrite(input_robustness, paste0(mapping_file_directory, "input_robustness.csv"))
}

# Internal dynamics
input_internal <- make_input_csv(exp_id = "f1_additive-simple_screening-1-internal", n_wells = 1, n_transfer = 20, n_transfer_selection = 10, composition_lograte = 10)
input_internal$output_dir <- data_directory
input_internal[is.na(input_internal)] <- "NA"
fwrite(input_internal, paste0(mapping_file_directory, "input_internal.csv"))
cat("\n")
