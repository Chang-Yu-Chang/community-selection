#!/usr/local/bin/python3

import sys
import os
from community_selection.usertools import *
import timeit
import time

#input_csv = "~/Desktop/Lab/community-selection/Data/Mapping_Files/input_independent_f1d_additive_medium2.csv" 
input_csv = "~/Desktop/Lab/community-selection/Data/Mapping_Files/input_independent_f5_invader_suppression.csv" 
row_number = 0

assumptions = make_assumptions(input_csv, row_number)
assumptions.update({
    "n_inoc": 100,
    "n_wells": 96,
    "l": 0.5,
    "dilution": 0.001,
    "n_propagation": 10,
    "response": "type III",
    "n_transfer": 5,
    "n_transfer_selection": 5
    })

start_time = timeit.default_timer()
{i: assumptions[i] for i in ("l", "dilution", "n_propagation", "response")}
print(str(round(timeit.default_timer() - start_time, 5)) + " seconds")
params, params_simulation , params_algorithm, plate = prepare_experiment(assumptions)
simulate_community(params = params, params_simulation = params_simulation, params_algorithm = params_algorithm, plate = plate)
print(str(round(timeit.default_timer() - start_time, 5)) + " seconds")


