#!/usr/bin/python
"""
Read the input_robustness.csv and output a shell script for simulation 
arg1 = input csv file
arg2 = output file name
"""

import os
import sys
import subprocess
import csv

input_csv_robustness = str(sys.argv[1]) # Input csv for robustness
output_joblist = str(sys.argv[2]) # Output file name
cluster_envir_string = "" # cluster_envir_string = "module load miniconda; source activate py37_dev; "
list_protocols = ["simple_screening"] + ["iteration_"+str(i) for i in [3,5]]

# Read input csv
with open(input_csv_robustness,"r") as f:
    reader = csv.reader(f,delimiter = ",")
    df_input_robustness = list(reader)
df_input_robustness.pop(0)

# Extract the list of overwrite_plate
listr_exp_id = list()
listr_seeds = list()
for i in range(len(df_input_robustness)): 
    plate = df_input_robustness[i][3] # overetie_plate
    seed = df_input_robustness[i][2] # seed
    listr_exp_id.append(plate)
    listr_seeds.append(seed)
    

fout = open(output_joblist, "wt")

# for each protocol
for ls in range(1, len(listr_seeds)+1):
    for lp in range(len(list_protocols)):
        line = cluster_envir_string
        
        ## Find rows of the perturbation protocols
        temp_index2 = list()
        for i in range(len(df_input_robustness)):
            temp = listr_exp_id[i]
            if (list_protocols[lp] + "-" + str(ls)) + "-" in temp:
                temp_index2.append(i)
    
        # Write perturbation protocols
        line_temp = ""
        for k in range(len(temp_index2)):
            line_temp = line_temp + "ecoprospector " + input_csv_robustness + " " + str(temp_index2[k]) + "; "
        line = line + line_temp

            
        # Write job per line in the joblist text file
        line = line + "\n"

	# Skip empty job
        if line == cluster_envir_string + "\n":
            continue
        else:
            fout.write(line)
        
fout.close()
