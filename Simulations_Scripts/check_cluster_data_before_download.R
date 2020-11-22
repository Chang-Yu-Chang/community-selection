# Check the download files on cluster before download
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(data.table)))


test_file_existence <- function (mapping_file_directory, mapping_file_name, simulation_file_directory) {
    stopifnot(!is.na(list.files(mapping_file_directory, mapping_file_name)))
    cat("\n", paste0(rep("-", getOption("width")), collapse = ""))
    cat("\nInput csv: ", mapping_file_name)
    #cat("\nInput csv: ", paste0(mapping_file_directory, mapping_file_name))
    input_csv <- fread(paste0(mapping_file_directory, mapping_file_name))
    x <- paste0(simulation_file_directory, input_csv$exp_id, "_function.txt")
    if (sum(file.exists(x)) == nrow(input_csv)) {
        cat("\tAll files exist")
    } else {
        cat("\tNumber of rows: ", nrow(input_csv))
        cat("\t", length(input_csv$exp_id[!file.exists(x)]), " files missing")
    }
}

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

mapping_file_directory <- "/home/cc2553/project/community-selection/wrapper/"
simulation_file_directory <- "/home/cc2553/project/community-selection/data/"

for (i in 1:length(mapping_files)) {
    test_file_existence(mapping_file_directory = paste0(mapping_file_directory, sub("input_", "", mapping_files[i]), "/"),
                        paste0(mapping_files[i], ".csv"),
                        simulation_file_directory = paste0(simulation_file_directory, sub("input_", "", mapping_files[i]), "/"))
}
cat("\n")





