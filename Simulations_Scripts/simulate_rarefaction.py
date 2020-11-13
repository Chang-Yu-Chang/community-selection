#!/usr/bin/env python

from community_selection.usertools import *
#from community_selection.usertools import *
input_csv = "/Users/chang-yu/Desktop/Lab/community-selection/Data/Mapping_Files/input_independent_f1_additive.csv"
row_number = 0 # Which row of experiment to run

assumptions = make_assumptions(input_csv, row_number)
params, params_simulation , params_algorithm, plate = prepare_experiment(assumptions)
assumptions["metacommunity_sampling"] = "Power"
assumptions["power_alpha"] = 0.01
plate_N_power = sample_from_pool(plate.N, assumptions)
plate_N_power.to_csv("../Data/test/plate_power.txt", index = False)


assumptions["metacommunity_sampling"] = "Lognormal"
assumptions["lognormal_mean"] = 8
assumptions["lognormal_sd"] = 8
plate_N_lognormal = sample_from_pool(plate.N, assumptions)
plate_N_lognormal.to_csv("../Data/test/plate_lognormal.txt", index = False)

assumptions["metacommunity_sampling"] = "Default"
plate_N_lognormal = sample_from_pool(plate.N, assumptions)
plate_N_lognormal.to_csv("../Data/test/plate_default.txt", index = False)
