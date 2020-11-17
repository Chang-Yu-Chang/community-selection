suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(data.table)))

mapping_files <- c("input_independent_f1_additive", "input_iteration_f1_additive", "input_robustness_f1_additive",
                   "input_independent_f1a_additive", "input_iteration_f1a_additive", "input_robustness_f1a_additive",
                   "input_independent_f1b_additive_cost", "input_iteration_f1b_additive_cost", "input_robustness_f1b_additive_cost",
                   "input_independent_f1b_additive_phi1", "input_iteration_f1b_additive_phi1", "input_robustness_f1b_additive_phi1",
                   "input_independent_f1b_additive_phi2", "input_iteration_f1b_additive_phi2", "input_robustness_f1b_additive_phi2",
                   "input_independent_f1c_additive_sampling1", "input_iteration_f1c_additive_sampling1", "input_robustness_f1c_additive_sampling1",
                   "input_independent_f1c_additive_sampling2", "input_iteration_f1c_additive_sampling2", "input_robustness_f1c_additive_sampling2",
                   "input_independent_f1d_additive_medium1", "input_iteration_f1d_additive_medium1", "input_robustness_f1d_additive_medium1",
                   "input_independent_f1d_additive_medium2", "input_iteration_f1d_additive_medium2", "input_robustness_f1d_additive_medium2",
                   "input_independent_f1e_additive_response1", "input_iteration_f1e_additive_response1", "input_robustness_f1e_additive_response1",
                   "input_independent_f1e_additive_response2", "input_iteration_f1e_additive_response2", "input_robustness_f1e_additive_response2",
                   "input_independent_f2_interaction", "input_iteration_f2_interaction", "input_robustness_f2_interaction",
                   "input_independent_f2a_interaction", "input_iteration_f2a_interaction", "input_robustness_f2a_interaction",
                   "input_independent_f5_invader_suppression", "input_iteration_f5_invader_suppression", "input_robustness_f5_invader_suppression",
                   "input_independent_f6_target_resource", "input_iteration_f6_target_resource", "input_robustness_f6_target_resource")

check_mapping_file <- function(mapping_file) {
    cat("\n", paste0(rep("-", getOption("width")), collapse = ""))
    cat("\n", mapping_file)
    df <- fread(paste0("../Data/Mapping_Files/", mapping_file, ".csv"))
    cat("\nExp_id function should correspond to selected_function")
    exp_id_func <- strsplit(df$exp_id, "-") %>%
        lapply(function(x) {`[`(x, 1) %>% substr(1,2)}) %>%
        unlist()
    cat("\t", all(exp_id_func == (df$selected_function %>% substr(1,2))))

    if (grepl("independent", mapping_file)) {

    } else if (grepl("iteration", mapping_file)) {
        df_iteration <- filter(df, protocol != "simple_screening")
        cat("\nIteration round, overwrite should have the correct prefix")
        exp_id_func <- strsplit(df_iteration$exp_id, "-") %>% lapply(function(x) {`[`(x, 1)}) %>% unlist
        overwrite_id_func <- strsplit(df_iteration$overwrite_plate, "/") %>% lapply(function(x) {`[`(x, 8)}) %>% unlist %>%
            strsplit("-") %>% lapply(function(x) {`[`(x, 1)}) %>% unlist()
        cat("\t", all(exp_id_func == overwrite_id_func))

# cat("Iteration round reads the output of previous round")
# df_iteration %>%
#     select(exp_id, overwrite_plate)

    } else if (grepl("robustness", mapping_files[i])) {


    }
}

for (i in 1:length(mapping_files)) {
    check_mapping_file(mapping_files[i])
}

if (FALSE) {

df <- fread("../Data/Mapping_Files/input_independent.csv")
df <- fread("~/Dropbox/community-selection/Data/Mapping_Files_Rebuttal/input_independent.csv")
df %>%
    filter(seed == 1, protocol == "simple_screening", monoculture == F) %>%
    view()

df <- fread("../Data/Mapping_Files/input_iteration.csv")
df <- fread("~/Dropbox/community-selection/Data/Mapping_Files_Rebuttal/input_iteration.csv")
df %>%
    filter(seed == 1, grepl("iteration_1_round2-", exp_id)) %>%
    view()

df %>%
    filter(seed == 3) %>% view()



df <- fread("../Data/Mapping_Files/input_robustness.csv")
df <- fread("~/Dropbox/community-selection/Data/Mapping_Files_Rebuttal/input_robustness.csv")


df %>% filter(seed == 3) %>% view()

df %>%
    filter(seed == 1, grepl("iteration_simple_screening-1-selected_community-migration2", exp_id)) %>%
    view()



table(df$ruggedness)
}
