############################ Description Start #################################
# Carlos Rodriguez, PhD. MRN
# 05-20-2021
# Get GUIDS
#
# This script is currently setup to get the GUIDs of the subjects used as input
# for the ica_300 project. The GUIDs need to be in a specific format to be able
# to filter out any demographic or neurocognitive measures from ABCD data.

############################ Description End ###################################


# Load libraries ----
library(tidyverse)

# Get the GUIDs (sub-NDARINV*) of the subjects used as ica input ----
# Set input path
ica_input_path <- "T:/research/analysis/human/jhouck/abcd/ica_300/ica_input"

# Get a list of the subjects
subjects <- list.files(path = ica_input_path, 
                    pattern = "sub-NDAR*", 
                    full.names = FALSE, 
                    recursive = FALSE)

# get INV* # which is in the last 11 characters of each directory, convert to
# tibble for binding columns, and rename the column to GUID
guids <- str_sub(subjects, -11) %>% 
  as_tibble() %>% 
  rename("GUID" = "value") 

# Create a tibble of "NDAR_" of 255 rows
ndar <- as_tibble(rep("NDAR_", length(subjects)))

# Bind the two columns, then merge their contents, name it guids and save the
# dataframe as guids
guids <- bind_cols(ndar, guids) %>% 
  unite(., GUID, sep = "")

# Save to a .csv file ----
write_csv(guids, 
          file = "./data/ica_300_guids.csv")
