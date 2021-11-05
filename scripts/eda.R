# Load Packages ####
library(tidyverse)  # for filtering and selecting
library(rstatix)    # for performing statistics
library(ggpubr)     # for creating graphs


# Load Data --------------------------------------------------------------------
# List and load the graph data, will be fed in wide format
graph_met <- read.csv("./data/ica_300_graph_metrics.csv")

# Read in the pearson data, will be fed in long format
ps01 <- read_csv("./data/filtered_abcd_ps01.csv", col_types = cols())

# Process the data, ie remove the questions, convert columns to numeric
data <- ps01 %>% filter(collection_id == 2573)
data$pea_wiscv_tss <- as.numeric(data$pea_wiscv_tss)
data$interview_age <- as.numeric(data$interview_age)

## Join the data by run and by k
plot_data <- left_join(data, (graph_met %>% filter(.,  run == "run-1" & k == 0.3)), by = c("subjectkey" = "subject"))

ggplot(plot_data, aes(x = interview_age, y = mod, color = as.factor(sex))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(plot_data, aes(x = interview_age, y = mod, color = as.factor(sex))) +
  geom_boxplot()


t_test(plot_data, formula = mod ~ sex)


### convert long to wide and calculate the mean
plot_data <- graph_met %>% 
  select(., subject, run, k, cpath) %>% 
  spread(run, cpath) %>%
  group_by(subject, k) %>%
  mutate(mean = mean(c(`run-1`, `run-2`, `run-3`, `run-4`))) %>%
  filter(., k == 0.5)

# Join data and plot data to include sex and other variables
plot_data <- left_join(data, plot_data, by = c("subjectkey" = "subject"))


t_test(plot_data, formula = mean ~ sex)

ggplot(plot_data, aes(x = interview_age, y = mean, color = as.factor(sex))) +
  geom_point() +
  geom_smooth(method = "lm")