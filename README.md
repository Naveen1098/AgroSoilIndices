# AgroSoilIndices
# `AgroSoilIndices` Soil parameters useful in agronomy practices
<!-- badges: start -->
### Authors: Naveen Kumar Purushothaman, Prabhu Govindasamy, Tarun Sharma, and Rishi Raj
<!-- badges: end -->
<em><p align="right"> Calculate everything in a go!!! </p></em>

## About
The `AgroSoilIndices` package provides high-performing functionality for 
calculating different soil paramters in a single run.

Several soil parameters such as soil textural parameters (sand, silt, and clay), USDA soil textural classes,
bulk density and particle density, porosity, water paramters, particle size distribution, carbon stock paramters, and root parameters

 User can also calculate based on only one parameter or all at a time based on their inputs provided.
 Details about the formula is provided in the "SoilParameters_Calculation.pdf" file attached above.

## Installation 
If you want to install the package and try its functionality, it is very simple,
just type the following line in your `R` console:

Step 1: Install the pre-required packages: 
```
list.of.packages <- c("ggplot2", "soiltexture","devtools","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```
Step 2: Call the required libraries: 
```
library(devtools)
library(openxlsx)
library(soiltexture)
```
Step 3: Installing the AgroSoilIndices package: 
```
install_github("Naveen1098/AgroSoilIndices")
```
## Example 
After installing `AgroSoilIndices` you should be also able to run the following lines:
```
# Required libraries
library(AgroSoilIndices)

# Calculating all indices
Soil_Indices( )
```
A new window opens for selecting an excel containing the data as per the prescribed format. 
The data file format is attached above named __`"Example_soil_datasheet.xlsx"`__ [click here to download](https://github.com/Naveen1098/AgroSoilIndices/blob/main/Example_soil_datasheet.xlsx "download")


**Note: If the data is not saved in the prescribed format the program won't run properly. Be cautious**

Results will be stored in the same folder where you have the data. The name of excel file is **"SoilIndices_Results.xlsx"**.

## Citing the package
Simply type and you will get the info you need:
```
citation(package = "AgroSoilIndices")
```
## Bug report and development version

You can create an [issue](https://github.com/Naveen1098/AgroSoilIndices/issues) on github. 
