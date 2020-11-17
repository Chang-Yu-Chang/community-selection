# Check the data name specified in mapping file and generate a job list file

suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(data.table)))

args = commandArgs(trailingOnly = T)
#data_directory = "/home/cc2553/project/community-selection/data/"
mapping_file_names <- args[1]
data_directory <- args[2]
joblist_name <- args[3]

#mapping_file_names <- c("../Data/Mapping_Files/input_independent_f1e_additive_response1.csv")
#data_directory = "~/Dropbox/community-selection/Data/Raw_Rebuttal/"
mapping_file <- fread(mapping_file_names)
file_names <- mapping_file$exp_id
file_existence <- file.exists(paste0(data_directory, file_names, "_function.txt"))

if (all(file_existence)) {
    cat("\nAll files exist")
} else {
    cat("\n", sum(!file_existence)," files are missing\n")
    cat(file_names[!file_existence], sep = "\n")
    file_open <- file(joblist_name)
    writeLines(paste0("ecoprospector ", mapping_file_names, " ", which(!file_existence)-1), con = file_open)
    close(file_open)
    cat("\nCreated", joblist_name)
}
cat("\n")

