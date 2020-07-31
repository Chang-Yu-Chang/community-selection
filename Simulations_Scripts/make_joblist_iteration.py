#!/usr/bin/python
"""
Read the input_iteration.csv and output a shell script for simulation 
arg1 = input csv file
arg2 = output file name
"""

import os
import sys
import subprocess
import csv

input_csv = str(sys.argv[1]) # Input csv for iteration
output_joblist = str(sys.argv[2]) # Output file name
cluster_envir_string = "" # cluster_envir_string = "module load miniconda; source activate py37_dev; "
list_protocols = ["simple_screening"] + ["iteration_"+str(i) for i in range(1,8)]

# Read input csv
with open(input_csv,"r") as f:
    reader = csv.reader(f,delimiter = ",")
    df_input = list(reader)

df_input.pop(0)

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


fout = open(output_joblist, "wt")

# for each protocol
for ls in range(1, n_seeds+1):
    for lp in range(len(list_protocols)):
        line = cluster_envir_string
        
	    # Find rows of the iteration protocols (rather than screen)
        temp_index = list()
        for i in range(len(df_input)):
            temp = list_exp_id[i]
            if list_protocols[lp] in temp and temp.endswith("-" + str(ls)):
                temp_index.append(i)
            
        # Screen
        if len(temp_index) == 1:
            line_temp = "ecoprospector " + input_csv + " " + str(temp_index[0]) + "; "
        
        # Iteration protocols
        if len(temp_index) != 1:
            line_temp = ""
            for k in range(len(temp_index)):
                line_temp = line_temp + "ecoprospector " + input_csv + " " + str(temp_index[k]) + "; Rscript subset_plate_composition.R " + input_csv + " " + str(temp_index[k]) + "; "
        line = line + line_temp

            
        # Write job per line in the joblist text file
        line = line + "\n"

	# Skip empty job
        if line == cluster_envir_string + "\n":
            continue
        else:
            fout.write(line)
        
fout.close()
