mapping_file_1 = fread('../data/input_independent.csv')
mapping_file_1$file = paste('../data/raw/',mapping_file_1$exp_id,'_function.txt',sep='')

mapping_file_2a = fread('../data/input_additive_iteration.csv')
mapping_file_2a$file = paste('../data/raw/',mapping_file_2a$exp_id,'_function.txt',sep='')

mapping_file_2b = fread('../data/input_additive_iteration_screen.csv')
mapping_file_2b$file = paste('../data/raw/iteration_NS/',mapping_file_2b$exp_id,'_function.txt',sep='')


mapping_file_3a = fread('../data/input_additive_robustness.csv')
mapping_file_3a$file = paste('../data/raw/',mapping_file_3a$exp_id,'_function.txt',sep='')

mapping_file_3b = fread('../data/input_additive_robustness_screen.csv')
mapping_file_3b$file = paste('../data/raw/robustness/',mapping_file_3b$exp_id,'_function.txt',sep='')

mapping_file_4 = fread('../data/input_composition.csv')

work = seq()