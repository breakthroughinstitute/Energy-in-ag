#Downlooad FAOSTAT production quantity for all primary crops, all years

# Load necessary libraries
library(dplyr)
library(readr)
library(httr)
library(zip)
library(tidyr)

# Define the URL for the zip file
url <- "https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_All_Data.zip"

# Define the local destination file path for the zip file
zip_file <- "Production_Crops_Livestock_E_All_Data.zip"

# Download the zip file
download.file(url, zip_file, mode = "wb")

# Unzip the file and extract the specific CSV file
unzip(zip_file, files = "Production_Crops_Livestock_E_All_Data_NOFLAG.csv", exdir = ".")

# Read the CSV file into a data frame
data <- read_csv("Production_Crops_Livestock_E_All_Data_NOFLAG.csv", locale = locale(encoding = "UTF-8"))

# Read in list of crops included by FAOSTAT as "Crops, Primary" (downloaded manually)
items_data <- read_csv("raw_data/faostat/production_crops_primary_names.csv")

# Extract the list of unique items from the second dataset
items_list <- unique(items_data$Item)

# Filter the main dataset for "Production Quantity" where "Item" is in the items_list
filtered_data <- data %>%
  filter(Element == "Production" & Item %in% items_list)

#check for mismatches
missing_items <- setdiff(items_list, unique(filtered_data$Item))
print(missing_items)
#only mate leaves is missing. insignificant since we only use caloric crops moving forward

# Pivot the data to have one 'Year' column and one 'Production Quantity' column
long_format_data <- filtered_data %>%
  pivot_longer(
    cols = starts_with("Y"),   # Adjust this if your year columns have a different prefix
    names_to = "year",
    names_prefix = "Y",         # Remove any prefix like "Y" from the year columns, if applicable
    values_to = "production"
  ) %>% 
  filter(is.na(production)==F) #remove NAs created by pivoting

#Remove irrelevant columns and make column names consistent with other code/data
long_format_data <- long_format_data %>% select(country = Area, crop = Item, year, production) #units are all in metric tons

#Fix country names
long_format_data <- long_format_data %>%
  mutate(country = case_when(
    country == "T\xfcrkiye" ~ "Türkiye",
    country == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
    country == "R\xe9union" ~ "Réunion",
    TRUE ~ country  # Keep the original value if no match is found
  ))

# Save the pivoted data to a new CSV file
write_csv(long_format_data, "raw_data/faostat/production.csv")

# Clean up by removing the downloaded and extracted files if desired
file.remove(zip_file)
file.remove("Production_Crops_Livestock_E_All_Data_NOFLAG.csv")