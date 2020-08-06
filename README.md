# community-selection

Scripts for the manuscripts entitled "Top-down engineering of complex communities by directed evolution"

Preprint [[link](https://www.biorxiv.org/content/10.1101/2020.07.24.214775v2)]

Contact: chang-yu.chang@yale.edu and jean.vila@yale.edu


## Data

`Data/Mapping_Files/` contains the input `csv` files taken by ecoprospector. These files can be used to generate all the data used in the main and supplementary figures.

`Data/Tables/` contains table-related data used to make selection matrices in TableS1, or auto-generated from the Rscripts to make the TableS2-5.

`Data/Raw/` is available upon request, because the raw data is too huge to be saved here. Alterantively you can run the simulation using the scripts described in the next section and save all data in this folder. 

## Simulations_Scripts

Scripts used to run the simulation. Note that the all simulations are based on the python package ecoprospector, the source script for the package version used in this paper are saved in `Simulations_Scripts/ecoprospector`. The latest version of ecoprospector can be found [here](https://github.com/Chang-Yu-Chang/ecoprospector). 

Download this repository, and go to the ecoprospector directory, and install ecoproespector using the following commands

```{bash}
$ cd yourDirectory/community-selection/ecoprospector
$ pip install -e .
```
Questions regarding development environment and dependency should be referred to the [ecoprospector package documentation](https://ecoprospector.readthedocs.io/en/latest/index.html)

To repeat the simulation used to generate the figures of the paper, run the shell scripts below sequentially. These shells scripts take the mapping files saved in `Data/Mapping_files/` and run simulation row by row. The output data will be saved in `Data/Raw/`. 

```{bash}
$ source run_independent.sh # Figures 1 and 2
$ source run_iteration.sh # Figure 3
$ Rscript make_synthetic_communities.R ../Data/Mapping_Files/input_iteration.csv
$ source run_robustness.sh # Figure 4
```
Note that run_robustness.sh requires the directed evolved community and synthetic community to have been generated. The synthetic community requires the result of monoculture simulations run by run_independent.sh. As such it is important that these scripts are run sequentially.

Ecoprospector only saves the compositional data for communitities at the end of each generation.Therefore to generate the data for Figure S3 where we plot the intra-generational dynamics,simply run this code in bash

```{bash}
$ python run_internal.py
```

## Plotting_Scripts

All script names correspond to the figure it generates. The output figures go in `Plots/`.

## Plots

`Plots/Cartoons/` saves the cartoon used in the figures.

All plots are generated from the `Plotting_Scripts/*.R` scripts.

