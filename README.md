# Working-age mortality is still an important driver of stagnating life expectancy in the United States

## Purpose of this Repository
The repository **StagnateWA-US** was created to enable the replication of findings reported in:

*Working-age mortality is still an important driver of stagnating life expectancy in the United States*,

hereafter *our manuscript*.

## Repository Structure
The repository **StagnateWA-US** contains two folders:

### 1. data
This folder stores all data files necessary to replicate our findings. 

We use age-, sex-, and country-specific death and exposure counts provided by the [Human Mortality Database](https://mortality.org) (HMD), version 06 October 2023, available in the .zip file `hmd_statistics_20231006`. This .zip file does **not** need to be unzipped before running the analysis files in the **scripts** folder (see below).

HMD data are distributed under a [Creative Commons Attribution 4.0 International License][cc-by]. Please note that our reported findings are based on HMD version 06 October 2023 and that data distributed by HMD may have been updated or revised in the meantime.

### 2. scripts
This folder contains all analysis files necessary to replicate our findings: 

- The file `08_get-hmd.R` loads the HMD input data from the **data** folder.

- The file `00_analysis.R` installs all `R` packages necessary to replicate our findings. It automatically executes the file `08_get-hmd.R` described above. Furthermore, the file `00_analysis.R` (a) carries out counterfactual life expectancy projections, and (b) creates plots of rates of mortality improvement (Figure 1) & counterfactual life expectancy values (Figure 2) for all available countries or country comparisons based on HMD version 06 October 2023. The file `00_analysis.R` saves these plots &ndash; in *.svg*, *.pdf*, and *.png* format &ndash; in automatically generated folders.  

## How to Use this Repository
In order to run the `R` code provided in the repository **StagnateWA-US**, please proceed in the following order:

1. Download the repository from `github`. If applicable, unzip the downloaded folder and place it in a location convenient for you. 
2. Double click on the file `StagnateWA-US.Rproj`. This should open `RStudio` on your machine.  
3. Within `RStudio`, click on `File`/`Open File...` and select the analysis file `00_analysis.R` located in the **scripts** folder.
4. You should now be able to run our code without adjusting any directories.

## License
This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
