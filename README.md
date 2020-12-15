# __NJ Maternal Mortality Pilot__

![NJMMBuild](https://github.com/johnsonandjohnson/njmm/workflows/NJMMBuild/badge.svg?branch=main)
[![NJMMDeploy](https://github.com/johnsonandjohnson/njmm/workflows/NJMMDeploy/badge.svg?branch=main)](https://johnsonandjohnson.shinyapps.io/njmm/)

This project is a collaboration with the Johnson & Johnson Women's Health Group
to build an interactive web application that visualizes maternal mortality
in the state of New Jersey in order to raise awareness of maternal mortality
and make it easier to see how it relates to other potential factors
(e.g. access to healthcare).

Learn more about this effort here - https://medium.com/johnson-johnson-open-source/maternal-mortality-in-new-jersey-680a52fb5bc5.

You can find the application here - https://johnsonandjohnson.shinyapps.io/njmm/.


##  Repository Structure
- `Root Folder`
  - This contains the necessary project setup files (e.g. gitignore) as well as our shiny application files
- `data`
  - This contains almost all necessary data files (raw, external, and processed)
  - `raw`
    - This contains all raw data from our data sources
  - `processed`
    - This contains all data files generated from the raw data
  - `reference`
    - This contains all reference datasets
- `renv`
  - This contains the package management/virtual environment files
- `explore`
  - This contains scripts meant to explore usage of different packages or datasets
- `etl`
  - This contains all scripts and functions used to convert the raw data into processed data
  - `layers`
    - This contains scripts and functions used solely convert raw layer data into processed data
  - `maternal_mortality`
    - This contains scripts and functions used soley to convert raw maternal mortality measurement data
    into processed data
- `app_utils`
  - This contains functions being used in shiny application broken out into separate files by functionality
- `test`
  - This contains unit tests and test data used to test our functions within utils
- `logs`
  - This contains all application logs (that are autogenerated)


## Pre-Requisites
### R
In order to help with R development, you'll need to install `renv` which manages the dependencies for this project.

If you're on a Mac, please follow the steps here to configure your computer for package compilation beforehand - https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/.

The following script will check if you have renv and then install any packages you may not have.

```r
pacman::p_install(renv, force = F, try.bioconductor = F)
renv::restore()
```

For package dependencies, you may additionally need to follow these steps:

- Install udunits with homebrew `homebrew install udunits`
- Install the `units` library outside p_load with `install.packages("units")`
- Install the `systemfonts` library outside of p_load with `install.packages("systemfonts")`
- (re)install XQuartz from https://www.xquartz.org/ 
- Restart your computer/R

## Testing
We use `testthat` for testing our utility functions. In order to run our test, just run the following command
in the shell.

```bash
Rscript test/testthat.R
```

## Styling
We use the `styler` package to ensure our code adheres to the [tidyverse code style guide](https://style.tidyverse.org/). A quick guide for installation and usage can be found [here](https://styler.r-lib.org/).

For the purposes of this project, we utilize the `style_file` and `style_dir` functions which automatically style individual files and entire directories, respectively. 

```r
pacman::p_load(styler)
style_file(path_to_file)
style_dir(path_to_directory)
```

## Our Workflow
- Our Methods and Tools
  - Static Code Analysis - [lintr](https://github.com/jimhester/lintr)
  - Git Strategy - Git Flow via Pull Requests
