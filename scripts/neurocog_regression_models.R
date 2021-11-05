# Description ####
# Carlos Rodriguez, Ph.D., Mind Research Network
# This script will compute regression models between graph theory metrics and the 
# scores from the abcd neurocognitive battery

# Load Packages ####
library(tidyverse)  # for filtering and selecting
library(rstatix)    # for performing statistics
library(ggpubr)     # for creating graphs
library(car)

# Load Data --------------------------------------------------------------------
# List and load the graph data, will be fed in wide format
graph_met <- read.csv("./data/ica_300_graph_metrics.csv")
nihtb <-  read.csv("./data/ica_300_nihtb_data.csv")


# Process the gaph_met data
# Group by run-1, group by measures, then average across thresholds
widen <- function(arg1){
  graph_met %>% 
    select(subject, run, k, all_of(arg1)) %>% 
    group_by(run) %>% spread(run, all_of(arg1)) %>% 
    group_by(subject, k) %>% 
    mutate(mean = mean(c(`run-1`, `run-2`, `run-3`, `run-4`))) %>%
    select(subject, k, mean) %>%
    group_by(subject) %>% spread(k, mean) %>%
    mutate(mean = mean(c(`0.1`, `0.2`, `0.3`, `0.4`, `0.5`))) %>%
    select(subject, mean)
}

# Set the graph measures -------------------------------------------------------
graph_measures <- c("mod", "clust", "cpath", "geff")

# For loop to go through each graph measure and compute correlations -----------
for (i in graph_measures){
  print(i)
  df <- widen(all_of(i))
  assign(i, df)
}

# Join graph metric data frames together
mean_graph_measures <- left_join(mod, clust, by = "subject") %>%
  left_join(., cpath, by = "subject") %>%
  left_join(., geff, by = "subject")

# Rename data frame columns
mean_graph_measures <- rename(mean_graph_measures, 
       mod = mean.x, 
       clust = mean.y, 
       cpath =  mean.x.x, 
       geff = mean.y.y)

# create the final flat data file
data <- left_join(mean_graph_measures, nihtb, by = c("subject" = "subjectkey"))

# Clean up the workspace
rm(df, graph_met, clust, cpath, geff, mod, nihtb, mean_graph_measures)

# ---------------------------Perform stats -------------------------------------

# RAVLT percent forgetting short delay ----------------------------------------
# Histogram for the dependent variable, shows it's skewed positively and zero
# inflated, and one outlier that demonstrated 100% forgetting
ggplot(data, aes(x = ravlt_pcnt_fgt_sd)) +
  geom_histogram(color = "#1565c0", fill = "#1565c0", alpha = 0.7)


# Create a new data frame with the outlier removed
pcnt_fgt_sd <- data %>% filter(ravlt_pcnt_fgt_sd < 100)
# Models can be run with measures of segregation vs integration
# clustering had a significant coefficient, but not when the outlier is removed
# taking out one outlier, results in null effects
model <- glm(formula = ravlt_pcnt_fgt_sd ~ mod + clust, 
             family = "poisson", 
             data = pcnt_fgt_sd)
summary(model)

vif(model)

# Scatter plot
data %>% filter(ravlt_pcnt_fgt_sd < 100) %>%
ggplot(., aes(x = clust, y = ravlt_pcnt_fgt_sd)) +
  geom_point(alpha = 0.7, color = "#1565c0") +
  geom_smooth(method = "lm", se = FALSE, color = "#1565c0")


# RAVLT percent forgetting long delay ---------------------------------------
# ggplot(data, aes(x = ravlt_pcnt_fgt_ld)) +
#   geom_histogram(color = "#1565c0", fill = "#1565c0", alpha = 0.7)
# 
# model <- glm(formula = ravlt_pcnt_fgt_ld ~ mod + clust + cpath + geff, 
#              family = "poisson", 
#              data = data)
# summary(model)


### Count data for the RAVLT forgetting sd
ggplot(data, aes(x = ravlt_fgt_sd)) +
  geom_histogram(color = "#1565c0", fill = "#1565c0", alpha = 0.7)

fgt_sd <- data %>% filter(ravlt_fgt_sd < 100)
# Models can be run with measures of segregation vs integration
# seems like clustering is highly associated with forgetting
# taking out one outlier, results in null effects
model <- glm(formula = ravlt_fgt_sd ~ geff, 
             family = "poisson", 
             data = fgt_sd)
summary(model)

vif(model)

# Scatter plot
  ggplot(data, aes(x = geff, y = ravlt_fgt_sd)) +
  geom_point(alpha = 0.7, color = "#1565c0") +
  geom_smooth(method = "lm", se = FALSE, color = "#1565c0")













# RAVLT immediate learning  ---------------------------------------
# Prepare long data for plotting
  long.data <- data %>% select(X, mod, clust, cpath, geff, ravlt_imm) %>%
    gather(key = measure,
           value = mean, -X, -subject, -ravlt_imm)
  
# plot all of the measures except characteristic patch length  
long.data %>% filter(measure != 'cpath') %>%
  ggplot(., aes(x = mean, y = ravlt_imm, color = as.factor(measure))) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE)    
  

ggplot(data, aes(x = ravlt_imm)) +
  geom_histogram(color = "#1565c0", fill = "#1565c0", alpha = 0.7)
  
ggplot(data, aes(x = geff, y = ravlt_imm)) +
  geom_point(alpha = 0.7, color = "#1565c0") +
  geom_smooth(method = "lm", se = FALSE, color = "#1565c0")

# Modularity
model <- glm(formula = ravlt_imm ~ mod, data = data, family = "quasipoisson")
summary(model)

# Clustering
model <- glm(formula = ravlt_imm ~ clust, data = data, family = "quasipoisson")
summary(model)

# C Path Length
model <- glm(formula = ravlt_imm ~ cpath, data = data, family = "quasipoisson")
summary(model)

# Global Efficiency
model <- glm(formula = ravlt_imm ~ geff, data = data, family = "quasipoisson")
summary(model)



# Poisson model show evidence of over dispersion


# Do a median split on the measures
# There are significant differences between high and low performers in the ravlt
# test of immediate memory, clustering, and global efficiency.
#median(data$ravlt_imm, na.rm = TRUE)
test <- data %>% 
  group_by(ravlt_imm) %>%
  mutate(ravlt_imm_medn_split = c("hi", "lo")[1 + (ravlt_imm <= median(ravlt_imm, na.rm=TRUE))])
test$ravlt_imm_medn_split <- as.factor(test$ravlt_imm_medn_split)

test$ravlt_imm_medn_split <- ifelse(data$ravlt_imm <= median(data$ravlt_imm, na.rm = TRUE), "low","high")
test$ravlt_imm_medn_split <- as.factor(test$ravlt_imm_medn_split)

ttest_data <- test %>% select(ravlt_imm_medn_split) %>%
  drop_na()

ttest_data %>% t_test(data = ., ravlt_imm ~ ravlt_imm_medn_split)


t.test(test$ravlt_imm ~ test$ravlt_imm_medn_split)
t.test(test$clust ~ test$ravlt_imm_medn_split)
t.test(test$cpath ~ test$ravlt_imm_medn_split)
t.test(test$geff ~ test$ravlt_imm_medn_split)
