#!/usr/bin/env python

from community_selection.usertools import *
input_csv = "/Users/chang-yu/Desktop/Lab/community-selection/Data/Mapping_Files/input_independent_f1_additive.csv"
row_number = 0

assumptions = make_assumptions(input_csv, row_number)
assumptions["metacommunity_sampling"] = "Default"
assumptions["S"] = 225
params, params_simulation , params_algorithm, plate_default = prepare_experiment(assumptions)
plate_default.N.to_csv("../Data/test/plate_default.txt", index = False)


assumptions["metacommunity_sampling"] = "Power"
assumptions["power_alpha"] = 0.01
params, params_simulation , params_algorithm, plate_power = prepare_experiment(assumptions)
plate_power.N.to_csv("../Data/test/plate_power.txt", index = False)


assumptions["metacommunity_sampling"] = "Lognormal"
assumptions["lognormal_mean"] = 8
assumptions["lognormal_sd"] = 8
params, params_simulation , params_algorithm, plate_lognormal = prepare_experiment(assumptions)
plate_lognormal.N.to_csv("../Data/test/plate_lognormal.txt", index = False)

