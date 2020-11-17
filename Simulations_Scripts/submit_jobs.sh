#!/usr/bin/env bash


temp_func1 = function(){
upload_to_cluster ~/Desktop/Lab/community-selection/Data/Mapping_Files/input_iteration_$1.csv project/community-selection/wrapper/iteration_$1/
upload_to_cluster ~/Desktop/Lab/community-selection/Data/Mapping_Files/input_robustness_$1.csv project/community-selection/wrapper/robustness_$1/
}

submit_job iteration $1
make_synthetic_communities2 $1
submit_job robustness $1

temp_func3 = function() {
scp 'cc2553@transfer-grace.hpc.yale.edu:~/project/community-selection/data/iteration_$1/*_function.txt' ~/Dropbox/community-selection/Data/Raw_Rebuttal/
scp 'cc2553@transfer-grace.hpc.yale.edu:~/project/community-selection/data/robustness_$1/*' ~/Dropbox/community-selection/Data/Raw_Rebuttal/
}

for i in f1b_additive_cost f1b_additive_phi1 f1b_additive_phi2 f1c_additive_sampling1 f1c_additive_sampling2 f1d_additive_medium1 f1d_additive_medium2 f1e_additive_response1 f1e_additive_response2
do
    #temp_func1 $i
    submit_job iteration $i
done
