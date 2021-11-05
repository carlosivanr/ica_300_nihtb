# Description ####
# Carlos Rodriguez, Ph.D., Mind Research Network
# This script will compute correlations between graph theory metrics and the 
# scores from the abcd neurocognitive battery

# Load Packages ####
library(tidyverse)  # for filtering and selecting
library(rstatix)    # for performing statistics
library(ggpubr)     # for creating graphs

# Load Data --------------------------------------------------------------------
# List and load the graph data, will be fed in wide format
graph_met <- read.csv("./data/ica_300_graph_metrics.csv")

# List and load the cognitive data
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
  select(subjectkey, all_of(ravlt_met), pea_wiscv_tss)

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

# Create a helper function to help widen the graph measure data ----------------
# Function to convert long to wide, arg1 = the graph measure
# Results in a data frame that includes the mean graph measures from 4 runs
widen <- function(arg1){
  graph_met %>% 
    select(subject, run, k, all_of(arg1)) %>% 
    group_by(run) %>% spread(run, all_of(arg1)) %>% 
    group_by(subject, k) %>% 
    mutate(mean = mean(c(`run-1`, `run-2`, `run-3`, `run-4`)))
}

# Set the graph measures -------------------------------------------------------
graph_measures <- c("mod", "clust", "cpath", "geff")

# For loop to go through each graph measure and compute correlations -----------
for (i in graph_measures){
  print(i)
  df <- widen(all_of(i))
  #print(head(df))
  df <- left_join(df, data, c("subject" = "subjectkey"))
  results <- df %>% 
    group_by(k) %>%
    cor_test(., 
             vars = c("mean"), 
             vars2 = all_of(neurocog_met), 
             method = "pearson", 
             use = "pairwise.complete.obs")
  
  assign(i, results)
}

# Select variables for 
# General Ability / Intelligence------------------------------------------------
general_intelligence <- c("nihtbx_picvocab_uncorrected", 
                  "nihtbx_reading_uncorrected",
                  "pea_wiscv_tss")

# Learning & Memory ------------------------------------------------------------
learning_memory <- c("nihtbx_list_uncorrected", 
                  "nihtbx_picture_uncorrected", 
                  "ravlt_imm",
                  "ravlt_lrn",
                  "ravlt_fgt_sd",
                  "ravlt_fgt_ld",
                  "ravlt_pcnt_fgt_sd",
                  "ravlt_pcnt_fgt_ld")

# Executive Functioning/ Cognitive Control -------------------------------------
executive_function <- c("nihtbx_cardsort_uncorrected", 
                  "nihtbx_pattern_uncorrected", 
                  "nihtbx_flanker_uncorrected")

# Plotting Functions  -------------------------------------------------------
# This guy here adds a layer underneath rather than on top of the existing plot
`-.gg` <- function(plot, layer) {
  if (missing(layer)) {
    stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
  }
  if (!is.ggplot(plot)) {
    stop('Need a plot on the left side')
  }
  plot$layers = c(layer, plot$layers)
  plot
} 

# Function to plot the correlation between graph metric and neurocog score
# as a function of threshold.
# arg1 = list of variables, arg2 = the output filename
make_plots <- function(arg1, arg2){
  # Set the nudge factor for significance markers
  nudge = .0015
  pthresh = 0.05/5
  
  p_num <- 0
  for (i in graph_measures){
    p_num <- p_num + 1
    print(i)
    plot_data <- get(i) %>%
      filter(var2 %in% arg1) 
    lpt <- 
      ggline(plot_data,
             x = "k", y = "cor", color = "var2",
             xlab = "Threshold",
             ylab = paste("r", i, sep = " "),
             legend.title = "Variable")
    p <- lpt
    
    for (j in 1:dim(plot_data)[1]) {
      if (plot_data$p[j] < pthresh) {
        ann_text <- data.frame(
          k = (plot_data$k[j] * 10),  # this is for the y axis
          cor = plot_data$cor[j])                    # this is for the x axis
        #var2 = get(i)$var2) # multiply 0.1 by 10 to get 1 for plotting the asterisk
        p <- p + geom_text(
          data = ann_text, label = "*",
          position = position_nudge(y = nudge)
          )
      }
    }
    
    p <- p - geom_hline(yintercept = 0, linetype = "dotted")
    assign(paste("plot_", p_num, sep = "") , p)
  }
  
  figure <- ggarrange(plot_1, plot_2, plot_3, plot_4,
                      labels = c("A", "B", "C", "D"),
                      ncol = 1, common.legend = TRUE)
  
  ggsave(paste(arg2, ".pdf", sep = ''), plot = figure, units = "mm", 
         width = 240, height = 240, dpi = 600)
  rm(plot_data)
}

# Make Plots -------------------------------------------------------------------
make_plots(executive_function, "executive_function")
make_plots(general_intelligence, "general_intelligence")
make_plots(learning_memory, "learning memory")


###
# Function to make scatter plots -----------
make_scatter_plots <- function(arg1){
  p_num <- 0
  for (i in graph_measures){
    print(i)
    df <- widen(all_of(i))
    t <- c(0.1, 0.2, 0.3, 0.4, 0.5)
    #for each k threshold
    for (j in 1:length(t)){
      
      for (l in 1:length(arg1)){
        p_num <- p_num + 1
          scatter_plot <- df %>% 
          filter(k == t[j]) %>% 
          left_join(., data, by = c("subject" = "subjectkey")) %>%
          select(mean, arg1[l]) %>% 
          drop_na() %>%
          ggplot(., aes_string(x = arg1[l], y = "mean")) +
          geom_point() +
          geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
          labs(y = paste(i, "k", t, sep ='_'))
        #print(scatter_plot)
        #assign(paste("plot_", p_num, sep = ""), scatter_plot)
        ggsave(paste(i, "_", t[j], "_", arg1[l] ,'.pdf', sep = ''), plot = scatter_plot)
      }
    }
  }
}

make_scatter_plots(general_intelligence)
make_scatter_plots(learning_memory)
make_scatter_plots(executive_function)
### Cash Choice is categorical, would need a different set of analysis


### Histograms for the neurocognitive battery measures -------------------------
for (i in column_names){
plt <- ggplot(data, aes_string(x = i)) +
  geom_histogram()
  print(plt)
  Sys.sleep(3)
}



### Histograms for the graph measures at each threshold level
for (i in graph_measures){
  print(i)
  df <- widen(all_of(i))
  t <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  for (j in 1:length(t)){
    temp <- df %>% filter(k == t[j])
    plt <- ggplot(temp, aes(x = mean)) +
      geom_histogram(aes(y = ..density..), color = "#1565c0", fill = "#1565c0", alpha = .7) +
      stat_function(fun = dnorm, color = "#440154FF", 
                    args = list(mean = mean(temp$mean, na.rm = TRUE), 
                                sd = sd(temp$mean, na.rm = TRUE))) + 
      labs(x = paste(i, t[j], sep = "_"))
    print(plt)
    Sys.sleep(3)
  }
}


