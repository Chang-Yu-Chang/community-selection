#!/usr/bin/python
"""
Read the input_indepdent.csv and output a shell script for simulation 
arg1 = input csv file
arg2 = output file name
"""

import os
import sys
import subprocess
import csv

input_csv = str(sys.argv[1]) # Input file 
output_joblist = str(sys.argv[2]) # Output file name
cluster_envir_string = "" # cluster_envir_string = "module load miniconda; source activate py37_dev; "

# Read input csv
with open(input_csv,"r") as f:
    reader = csv.reader(f,delimiter = ",")
    df_input = list(reader)
df_input.pop(0)

# Number of experiments = number of jobs
with open(input_csv,"r") as f:
    reader = csv.reader(f,delimiter = ",")
    data = list(reader)
    total_experiments = len(data) - 1 # Not counting header

# Total number of seeds
n_seeds = int(df_input[len(df_input)-1][2])

# Extract the list of exp_id
list_exp_id = list()
list_seeds = list()
for i in range(len(df_input)): 
    exp_id = df_input[i][3] # expid 
    seed = df_input[i][2] # seed
    list_exp_id.append(exp_id)
    list_seeds.append(seed)

# Write job per line in the joblist text file
fout = open(output_joblist, "wt")
line_initial = cluster_envir_string

counter = 0
line_temp = cluster_envir_string
for i in range(total_experiments):
    # Monoculture takes longer, run one monoculture in one job
    if "monoculture" in list_exp_id[i]:
        line_monoculture = cluster_envir_string + "ecoprospector " + input_csv + " " + str(i) + ";\n"
        fout.write(line_monoculture)
        continue
        
    line_temp += "ecoprospector " + input_csv + " " + str(i) + ";"
    counter += 1

    # Group 10 simulations into one job; if the number of expeirment cannot be fully divided by 10, also print the residue
    if counter == 10 or i == (total_experiments-1): 
        counter = 0
        line_temp += "\n"
        fout.write(line_temp)
        line_temp = line_initial

fout.close()
