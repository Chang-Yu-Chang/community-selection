# community-selection

Scripts for the manuscripts entitled "Top-down engineering of complex communities by directed evolution"

Preprint [[link](https://www.biorxiv.org/content/10.1101/2020.07.24.214775v2)]

Contact: chang-yu.chang@yale.edu and jean.vila@yale.edu

## Folders

### Data

`Data/Mapping_Files/` contains input mapping `csv` files taken by ecoprospector to generate all data used in the main figures.

`Data/Tables/` contains table-related data used to make selection matrices in TableS1, or auto-generated from the Rscripts to make the TableS2-5.

`Data/Raw/` is empty. Because the raw data is huge, it is upon request, or one can run the simulation using the scripts described in the next section and save all data in this folder. 

### Simulations_Scripts

Scripts used to run the simulation. Note that the all simulations are based on customized python package ecoprospector, the source script used in this paper in save in `Simulations_Scripts/ecoprospector`. The latest version of ecoprospector can be found [here](https://github.com/Chang-Yu-Chang/ecoprospector). 

Download this repo, and browsing to ecoprospector directory, and install ecoproespector.

```{bash}
$ cd yourDirectory/community-selection/ecoprospector
$ pip install .
```

To repeat the simulation used in the main figures of the paper, run the below shell scripts. In these scripts, file will take the mapping files saved in `Data/Mapping_files/` and run simulation line by line. The output data files will all be save in the `Data/Raw/` folder.

```{bash}
$ source run_independent.sh
$ source run_iteration.sh
$ source run_robustness.sh
```

To run the simulation for Figure S3 the internal dynamics, simply run this code in bash

```{bash}
$ python run_internal.py
```

### Plotting_Scripts

All script names correspond to the figure it generate. The output figures go in `Plots/`.

### Plots

`Plots/Cartoons/` saves the cartoon used in the figures.

All plots are generated from the `Plotting_Scripts/*.R` scripts.

