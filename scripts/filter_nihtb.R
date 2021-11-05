############################ Description Start #################################
# Carlos Rodriguez, PhD. MRN
# 05-20-2021
# Use GUIDs to filter ABCD data for ica_300 project
#


############################ Description End ###################################

# Load Libraries
library(tidyverse)

# Load GUIDs
guids <- read.csv("./data/ica_300_guids.csv")

# Set the abcd data path
abcd_path = "T:/research/analysis/human/jhouck/abcd/collection_2573/"

# Read in .txt files
nihtb <- read.delim(paste(abcd_path, "abcd_tbss01.txt", sep = "/"))

# Filter nihtb by the guids, select the columns of interest, and save as 
# proc_speed. note: eventname used to ensure all are baseline measures
proc_speed <- filter(nihtb, 
                     subjectkey %in% guids$GUID) %>% 
  select(subjectkey, 
         interview_age, 
         sex, 
         nihtbx_pattern_agecorrected,
         eventname)

# Save to a .csv file ----
write_csv(proc_speed, 
          file = "./data/ica_300_proc_speed.csv")