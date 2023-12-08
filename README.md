# Modelling Drivers of Economic Inactivity: A package and other resources

## Introduction

This github repository contains both an R package and a series of notebooks which make use of the package. This is part of a programme of work at Public Health Scotland to which aims to investigate the determinants (drivers) of various forms of economic inactivity and activity, and the factors that determine transitions between different types of economic (in)activity category.

## Setting up

The repository makes extensive use of the UK Household Longitudinal Study (UKHLS), also known as Understanding Society. Most of the functions developed as part of the package are designed to make working with this data more convenient. However for both size and ownership reasons the data used are not included as part of this repository.

To set up this project in the way expected by the package functions:

-   Register with the [UK Data Archive](https://www.data-archive.ac.uk/)
-   Find and download the **stata** version of [Study ID 6614](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614), Understanding Society: Waves 1-13, 2009-2022 and Harmonised BHPS: Waves 1-18, 1991-2009
    -   This is the publicly accessible version of the UKHLS, in which fields have been harmonised between waves, and which also includes records from the British Household Panel Survey (BHPS), which is the predecessor to Understanding Society
-   Unzip the above file into the location `big_data` within this folder. Within this folder, make sure the unzipped data are within the folder `UKDA-6614-stata`.
    -   Within `UKDA-6615-stata`, there should be the following files and folders:
        -   `6614_file_information.rtf`
        -   `read6614.htm`
        -   `mrdoc`, containing further documentation
        -   `stata`, containing the data files themselves
-   Once the data files are in the appropriate relative location, open up the RStudio project by double clicking on the `economic_inactivity.rproj` file, or selecting the project from within RStudio
-   To load the package, while within RStudio, run `devtools::load_all(".")` in the console. If devtools is installed correctly, you should also be able to run the above with the keyboard shortcut `SHIFT + cmd/cntrl + L`

## Function documentation and test suite.

The functions created as part of this project are visible by selecting `package:economic_inactivity` in the drop-down menu near the top of the environment pane in RStudio. Each of these functions contains some documentation but not all is complete. More than 100 unit tests relating to the functions are currently included in the package and at the time of writing all pass!

## Note

-   This repo is provided as-is, and is frequently updated. At present, the repository is both a package and an active workspace. In due course I may aim to separate these two roles more explicitly.

## Further information

For further information please contact me at [jon.minton\@phs.scot](mailto:jon.minton@phs.scot){.email}
