# community-selection

Scripts for the manuscripts entitled "Top-down engineering of complex communities by directed evolution"

Preprint [[link](https://www.biorxiv.org/content/10.1101/2020.07.24.214775v2)]

Contact: chang-yu.chang@yale.edu and jean.vila@yale.edu


## Data

`Data/Mapping_Files/` contains input mapping `csv` files taken by ecoprospector to generate all data used in the main figures.

`Data/Tables/` contains table-related data used to make selection matrices in TableS1, or auto-generated from the Rscripts to make the TableS2-5.

`Data/Raw/` is empty upon request, because the raw data is too huge to be saved here. Or one can run the simulation using the scripts described in the next section and save all data in this folder. 

## Simulations_Scripts

Scripts used to run the simulation. Note that the all simulations are based on customized python package ecoprospector, the source script used in this paper in save in `Simulations_Scripts/ecoprospector`. The latest version of ecoprospector can be found [here](https://github.com/Chang-Yu-Chang/ecoprospector). 

Download this repository, and browsing to ecoprospector directory, and install ecoproespector by commands

```{bash}
$ cd yourDirectory/community-selection/ecoprospector
$ pip install .
```
Questions regarding development environment and dependency should be referred to the [ecoprospector package documentation](https://ecoprospector.readthedocs.io/en/latest/index.html)

To repeat the simulation used in the main figures of the paper, run the shell scripts below sequentially. These shells scripts take the mapping files saved in `Data/Mapping_files/` and run simulation row by row. The output data will be saved in `Data/Raw/`. 

```{bash}
$ source run_independent.sh # Figures 1 and 2
$ source run_iteration.sh # Figure 3
$ Rscript make_synthetic_communities.R ../Data/Mapping_Files/input_iteration.csv
$ source run_robustness.sh # Figure 4
```
Note that the robustness simulation takes directed evolved community and synthetic community, which require outcome of iteration simulation and the result of monoculture (to extract per-capita species features) from independent simulations.

To run the simulation for Figure S3 the internal dynamics, simply run this code in bash

```{bash}
$ python run_internal.py
```

## Plotting_Scripts

All script names correspond to the figure it generate. The output figures go in `Plots/`.

## Plots

`Plots/Cartoons/` saves the cartoon used in the figures.

All plots are generated from the `Plotting_Scripts/*.R` scripts.

