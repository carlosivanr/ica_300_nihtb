# Load Packages ####
library(tidyverse)  # for filtering and selecting

# Get the subject keys ---------------------------------------------------------
# Set input path
ica_input_path <- "T:/research/analysis/human/jhouck/abcd/ica_300/ica_input"

# Get a list of the subjects
subjects <- list.files(path = ica_input_path, 
                       pattern = "sub-NDAR*", 
                       full.names = FALSE, 
                       recursive = FALSE)

# get INV* # which is in the last 11 characters of each directory, convert to
# tibble for binding columns, and rename the column to GUID
keys <- str_sub(subjects, -11) %>% 
  as_tibble() %>% 
  rename("subjectkey" = "value") 

# Create a tibble of "NDAR_" of 255 rows
ndar <- as_tibble(rep("NDAR_", length(subjects)))

# Bind the two columns, then merge their contents, name it guids and save the
# dataframe as guids
keys <- bind_cols(ndar, keys) %>% 
  unite(., subjectkey, sep = "")




# Load Data --------------------------------------------------------------------
# List and load the cognitive data files
files <- list.files(
  path = "./data",
  pattern = "filtered",
  full.names = TRUE,
  recursive = FALSE
)

# Read in the abcd files -------------------------------------------------------
ravlt_met <- c("pea_ravlt_sd_trial_i_tc",
               "pea_ravlt_sd_trial_ii_tc",
               "pea_ravlt_sd_trial_iii_tc",
               "pea_ravlt_sd_trial_iv_tc",
               "pea_ravlt_sd_trial_v_tc",
               "pea_ravlt_sd_trial_vi_tc",
               "pea_ravlt_ld_trial_vii_tc")


ps01 <- read_csv(files[1], col_types = cols()) %>% 
  filter(collection_id == 2573) %>%
  select(subjectkey, interview_age, sex, all_of(ravlt_met), pea_wiscv_tss)

# Convert the ravlt metrics to numeric
ps01[ravlt_met] <- sapply(ps01[ravlt_met], as.numeric)

# Create columns of the ravlt measures: immediate, learn, forgetting, % forgetting
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5233798/
ps01 <- ps01 %>%
  mutate(ravlt_imm = rowSums(select(.,pea_ravlt_sd_trial_i_tc:pea_ravlt_sd_trial_v_tc)),
         ravlt_lrn = pea_ravlt_sd_trial_v_tc - pea_ravlt_sd_trial_i_tc,
         ravlt_fgt_sd = pea_ravlt_sd_trial_v_tc - pea_ravlt_sd_trial_vi_tc,
         ravlt_fgt_ld = pea_ravlt_sd_trial_v_tc - pea_ravlt_ld_trial_vii_tc) %>%
  mutate(ravlt_pcnt_fgt_sd = (ravlt_fgt_sd / pea_ravlt_sd_trial_v_tc) * 100,
         ravlt_pcnt_fgt_ld = (ravlt_fgt_ld / pea_ravlt_sd_trial_v_tc) * 100)

# Take care of negative values
ps01 <- ps01 %>% mutate(ravlt_fgt_sd = replace(ravlt_fgt_sd, ravlt_fgt_sd<0, NA))
ps01 <- ps01 %>% mutate(ravlt_fgt_ld = replace(ravlt_fgt_ld, ravlt_fgt_ld<0, NA))
ps01 <- ps01 %>% mutate(ravlt_pcnt_fgt_sd = replace(ravlt_pcnt_fgt_sd, ravlt_pcnt_fgt_sd<0, NA))
ps01 <- ps01 %>% mutate(ravlt_pcnt_fgt_ld = replace(ravlt_pcnt_fgt_ld, ravlt_pcnt_fgt_ld<0, NA))
ps01 <- ps01 %>% mutate(ravlt_lrn = replace(ravlt_lrn, ravlt_lrn<0, NA))



# NIH Toolbox Measures
tbss01 <- read_csv(files[2], col_types = cols()) %>% 
  filter(collection_id == 2573) %>%
  select(subjectkey,
         nihtbx_picvocab_uncorrected, 
         nihtbx_reading_uncorrected, 
         nihtbx_list_uncorrected,
         nihtbx_picture_uncorrected,
         nihtbx_cardsort_uncorrected,
         nihtbx_pattern_uncorrected,
         nihtbx_flanker_uncorrected)

lmtp201 <- read_csv(files[4], col_types = cols()) %>% 
  filter(collection_id == 2573) %>%
  select(subjectkey, 
         lmt_scr_num_correct, 
         lmt_scr_efficiency)

cct <- read_csv(files[3], col_types = cols()) %>% 
  filter(collection_id == 2573) %>%
  select(subjectkey, 
         cash_choice_task)

# Stitch together the neurocognitive battery scores
data <- left_join(ps01, tbss01, lmtp201, by = "subjectkey")



# Remove the neurocog dataframes
rm(cct, lmtp201, ps01, tbss01)


# Convert the neurocog scores to numeric ---------------------------------------
neurocog_met <- c("pea_wiscv_tss",
                  "nihtbx_picvocab_uncorrected", 
                  "nihtbx_reading_uncorrected", 
                  "nihtbx_list_uncorrected", 
                  "nihtbx_picture_uncorrected", 
                  "nihtbx_cardsort_uncorrected", 
                  "nihtbx_pattern_uncorrected", 
                  "nihtbx_flanker_uncorrected",
                  "ravlt_imm",
                  "ravlt_lrn",
                  "ravlt_fgt_sd",
                  "ravlt_fgt_ld",
                  "ravlt_pcnt_fgt_sd",
                  "ravlt_pcnt_fgt_ld")

# Convert all measures to numeric
data[neurocog_met] <- sapply(data[neurocog_met], as.numeric)


# Stitch together the neurocognitive battery scores
output <- left_join(keys, data, by = "subjectkey")

# Convert NAs to NaNs
#output[is.na(output)] <- NaN


# Write out to a .csv file
write.csv(output, "./data/ica_300_nihtb_data.csv")





