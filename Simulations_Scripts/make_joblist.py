#!/usr/bin/python
"""
Read the input csv and output a shell script or txt that contains jobs 
arg1 = {independent, iteration, robustness}
arg2 = input csv file
arg3 = output file name
"""

import os
import sys
import subprocess
import csv

simulation_job_type = str(sys.argv[1]) # Input csv file type
input_csv = str(sys.argv[2]) # Input file 
output_joblist = str(sys.argv[3]) # Output file name
iteration_rounds = range(1, 24)
#iteration_rounds = range(11, 24)
cluster_envir_string = "source activate py37_dev; "
commandline_tool = "ecoprospector "
list_protocols = ["simple_screening"] + ["iteration_"+str(i) for i in range(1,8)]
#list_protocols = ["simple_screening"] + ["iteration_"+str(i) for i in range(]

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


if simulation_job_type == "independent":
    fout = open(output_joblist, "wt")
    line_initial = cluster_envir_string
    
    counter = 0
    line_temp = cluster_envir_string
    for i in range(total_experiments):
        # Monoculture takes longer, run one monoculture in one job
        if "monoculture" in list_exp_id[i]:
            line_monoculture = cluster_envir_string + commandline_tool + input_csv + " " + str(i) + ";\n"
            fout.write(line_monoculture)
            continue
            
        line_temp += commandline_tool + input_csv + " " + str(i) + ";"
        counter += 1
    
        # Group 10 simulations into one job; if the number of expeirment cannot be fully divided by 10, also print the residue
        if counter == 5 or i == (total_experiments-1): 
            counter = 0
            line_temp += "\n"
            fout.write(line_temp)
            line_temp = line_initial
    
    fout.close()

elif simulation_job_type == "iteration": 
    fout = open(output_joblist, "wt")
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
                line_temp = commandline_tool + input_csv + " " + str(temp_index[0]) + "; "
            
            # Iteration protocols
            if len(temp_index) != 1:
                line_temp = ""
                exp_id_all = [df_input[i][3] for i in range(len(df_input))]
                exp_id = [exp_id_all[i] for i in temp_index]
                for k in range(len(temp_index)):
                    if exp_id[k].split("-")[1].split("_")[2] in ["round" + str(i) for i in iteration_rounds]: 
                        line_temp = line_temp + commandline_tool + input_csv + " " + str(temp_index[k]) + "; "
            line = line + line_temp
    
                
            # Write job per line in the joblist text file
            line = line + "\n"
    
    	# Skip empty job
            if line == cluster_envir_string + "\n":
                continue
            else:
                fout.write(line)
            
    fout.close()

elif simulation_job_type == "robustness": 
    fout = open(output_joblist, "wt")
    line_initial = cluster_envir_string
    counter = 0
    line_temp = cluster_envir_string
    for i in range(total_experiments):
        # Monoculture takes longer, run one monoculture in one job
        if "monoculture" in list_exp_id[i]:
            line_monoculture = cluster_envir_string + commandline_tool + input_csv + " " + str(i) + ";\n"
            fout.write(line_monoculture)
            continue
            
        line_temp += commandline_tool + input_csv + " " + str(i) + ";"
        counter += 1
    
        # Group 10 simulations into one job; if the number of expeirment cannot be fully divided by 10, also print the residue
        if counter == 10 or i == (total_experiments-1): 
            counter = 0
            line_temp += "\n"
            fout.write(line_temp)
            line_temp = line_initial
    
    
    # for ls in range(1, len(list_seeds)+1):
    #     for lp in range(len(list_protocols)):
    #         line = cluster_envir_string
    #         
    #         ## Find rows of the perturbation protocols
    #         temp_index2 = list()
    #         for i in range(len(df_input)):
    #             temp = list_exp_id[i]
    #             if (list_protocols[lp] + "-" + str(ls)) + "-" in temp:
    #                 temp_index2.append(i)
    #     
    #         # Write perturbation protocols
    #         line_temp = ""
    #         for k in range(len(temp_index2)):
    #             line_temp = line_temp + commandline_tool + input_csv + " " + str(temp_index2[k]) + "; "
    #         line = line + line_temp
    # 
    #             
    #         # Write job per line in the joblist text file
    #         line = line + "\n"
    # 
    # 	# Skip empty job
    #         if line == cluster_envir_string + "\n":
    #             continue
    #         else:
    #             fout.write(line)
            
    fout.close()

print("Created " + output_joblist)
