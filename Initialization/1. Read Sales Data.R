## Read all csv files in the sales folder, combine them and save as a "current_sales.csv" in the end

# clear the environment
rm(list= ls())
gc() # garbage collection

# load packages and set options
options(stringsAsFactors = FALSE)

# install packages if not available
packages <- c("readr","data.table", #read data
              "lubridate", "zoo", #date time conversion
              "tidyverse", # full set of pkgs
              "dplyr" #data exploratory + manipulation
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# Read all files in folder
# create a list of the files from your target directory
file_list <- list.files(path="./Data/sales data/")


# initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
sales_data <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- fread(paste0('./Data/sales data/',file_list[i])) #read in files using the fread function from the data.table package
  sales_data <- rbindlist(list(sales_data, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


# Save current sales file
write.csv(sales_data,'./Data/sales data/current_sales.csv', row.names = F)