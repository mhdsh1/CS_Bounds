/******************************************************************************
Authors: Mahdi Shams
Date:          
Last modified: Aug 06, 2024
Description: 



*******************************************************************************/


********************************************
* SET UP USERS AND DIRECTORIES
********************************************

clear all

* Extract current username
local user = "mahdi"

* Current working folder 
local folder = "CS_Bounds"

* Directory to Dropbox folder, which depends on machine type:
if "`c(os)'" == "Windows" global path = "C:/Users/`user'/Dropbox/`folder'"
if "`c(os)'" == "Unix"    global path = "/home/`user'/Dropbox/`folder'"
else global path = "/Users/`user'/Dropbox/Research/`folder'"

global alvaro = "${path}/input/23_02_13_download_data"
global output = "${path}/code"

********************************************


use "${alvaro}/data/CPS_cleaned_deflated_merged_yearly.dta", clear 


//"${alvaro}/data/CPS_cleaned_merged_deflated_07_15.dta"





