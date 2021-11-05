# Description ####
# Carlos Rodriguez, Ph.D., Mind Research Network
# This script will compute correlations between graph theory metrics and the 
# wiscv scores from the abcd_ps01 data.

# There are some significant differences between graph theory metrics and wiscv
# score for run-3 only, however, results are not consistent with other runs. 
# This leads one to believe that the association between graph connectivity is
# not reliably associated with measures of fluid intelligence


# Load Packages ####
library(tidyverse)  # for filtering and selecting
library(rstatix)    # for performing statistics
library(ggpubr)     # for creating graphs


# Load Data --------------------------------------------------------------------
# List and load the graph data, will be fed in wide format
graph_met <- read.csv("./data/ica_300_graph_metrics.csv")


# List and load the cognitive data. This was set up in case multiple filtered
# files will be used in a for loop
files <- list.files(
  path = "./data",
  pattern = "filtered",
  full.names = TRUE,
  recursive = FALSE
)

# Read in the pearson data, will be fed in long format
ps01 <- read_csv(files[1], col_types = cols())

# Process the data, ie remove the questions, convert columns to numeric
data <- ps01 %>% filter(collection_id == 2573)
data$pea_wiscv_tss <- as.numeric(data$pea_wiscv_tss)
data$interview_age <- as.numeric(data$interview_age)


# EDA --------------------------------------------------------------------------
# Histogram of age
ggplot(data, aes(x = interview_age, color = sex, fill = sex)) +
  geom_histogram(binwidth = 1) +
  facet_grid(sex ~ .)

# Histogram of wiscv total scaled score
ggplot(data, aes(data$pea_wiscv_tss, color = sex, fill = sex)) +
  geom_histogram(binwidth = 1) +
  facet_grid(sex ~ .)


# Correlations  ----------------------------------------------------------------
data <- data %>% arrange(subjectkey)
graph_met <- graph_met %>% arrange(subject)

runs <- c("run-1", "run-2", "run-3", "run-4")
k <- c(0.1, 0.2, 0.3, 0.4, 0.5)

for (i in 1:length(runs)){
  for (j in 1:length(k)){
    # Get each run and k data
    y <- graph_met %>% filter(run == runs[i] & k == k[j])
    
    # join the two data frames by subjectkey and subject columns
    run <- left_join(data, y, by = c("subjectkey" = "subject"))
    
    print(paste(runs[i], k[j]))
    results <- cor_test(run, 
                        vars = c("pea_wiscv_tss"), 
                        vars2 = c("mod", "clust", "cpath", "geff"), 
                        method = "pearson", use = "pairwise.complete.obs")
    print(results)  
    print(" ")
  }
}