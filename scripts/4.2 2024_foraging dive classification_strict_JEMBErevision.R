
#-----------------------------------------------------------------------------
# Chris Oosthuizen
# April 2024
# Penguin wiggles
#-----------------------------------------------------------------------------

# Changing the definition of a foraging dive

#-----------------------------------
# Setup
#-----------------------------------

# Set system time zone
Sys.setenv(TZ = "GMT")

# Load libraries
library(tidyverse)
library(patchwork)
library(ggbeeswarm) 

source('./scripts/0.1 ggplot theme.R') 

#-----------------------------------------------------------
# Dive classification (foraging dive or non-foraging dive)
#-----------------------------------------------------------
# strict_def et al 2021

# Foraging and exploratory dives both were at least 10 m. To be classified as a foraging dive, 
# a dive needed to have one of the following set of characteristics: 
# (a) bottom time of at least 20 s, 
#max depth of at least 15 m and 
# more than 4 undulations

strict_def = readRDS("./output/supplement_result_df_strict.rds")

wiggledepth <- strict_def %>%
  group_by(id) %>%
  mutate(
    foraging_dive = foraging_dive,
    pred_wiggleN_Halsey_1m = ifelse(wiggleN_Halsey_1m > 2, "foraging", "non-foraging"),
    pred_wiggleN_Halsey_2m = ifelse(wiggleN_Halsey_2m > 2, "foraging", "non-foraging"),
    pred_wiggleN_Simeone = ifelse(wiggle_SW_bottom > 4, "foraging", "non-foraging"),
    pred_wiggleN_Takahashi = ifelse(wiggleN_Takahashi > 3, "foraging", "non-foraging"))  # exclude the '1' compulsory wiggle per dive

# Kim et al 2025 and Lescroel et al. 2021 use 'more than 4' wiggles in their analyis
# But this results in very bad classification in this example. 
# pred_wiggleN_Halsey_1m = ifelse(wiggleN_Halsey_1m > 4, "foraging", "non-foraging"),
# pred_wiggleN_Halsey_2m = ifelse(wiggleN_Halsey_2m > 4, "foraging", "non-foraging"),
# pred_wiggleN_Simeone = ifelse(wiggle_SW_bottom > 4, "foraging", "non-foraging"),
# pred_wiggleN_Takahashi = ifelse(wiggleN_Takahashi > 4, "foraging", "non-foraging"))  # exclude the '1' compulsory wiggle per dive


wiggledepth = as.data.frame(wiggledepth)
head(wiggledepth)


# Create summary table
Halsey_1m_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Halsey_1m) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Halsey_1m,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  ungroup() %>%
  group_by(Reference) %>%
  mutate(prop = round(Freq/sum(Freq),3))

# Create summary table
Halsey_2m_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Halsey_2m) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Halsey_2m,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  ungroup() %>%
  group_by(Reference) %>%
  mutate(prop = round(Freq/sum(Freq),3))

# Create summary table
Simeone_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Simeone) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Simeone,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  ungroup() %>%
  group_by(Reference) %>%
  mutate(prop = round(Freq/sum(Freq),3))

# Create summary table
Takahashi_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Takahashi) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Takahashi,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  ungroup() %>%
  group_by(Reference) %>%
  mutate(prop = round(Freq/sum(Freq),3))

#-------------------------
# ggplot confusion matrices
#-------------------------
# Python way round:
# https://medium.com/analytics-vidhya/visually-interpreting-the-confusion-matrix-787a70b65678
# this is the format of the very popular Python library for ML: sklearn. 

CM_Halsey_1m = 
  ggplot(data = Halsey_1m_table, 
         aes(x = Prediction, y = Reference,  fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = Freq), vjust = 0.8, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
  #  xlim(levels(Halsey_1m_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  #ggtitle("Halsey et al. 2007 (1m)") + 
  ggtitle("Three-fold changepoint wiggles (1m)") + 
  xlab("Predicted (wiggles)")+
  ylab("Reference (video)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels

#CM_Halsey_1m

CM_Halsey_2m = 
  ggplot(data = Halsey_2m_table, 
         aes(x = Prediction, y = Reference,  fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = Freq), vjust = 0.8, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
  # xlim(levels(Halsey_2m_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
 # ggtitle("Halsey et al. 2007 (2m)") + 
  ggtitle("Three-fold changepoint wiggles (2m)") + 
  xlab("Predicted (wiggles)")+
  ylab("Reference (video)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels

#CM_Halsey_2m


# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups 
CM_Simeone = 
  ggplot(data = Simeone_table, 
         aes(x = Prediction, y = Reference,  fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = Freq), vjust = 0.8, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
  #  xlim(levels(Simeone_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
#  ggtitle("Simeone and Wilson 2003 (0.3m)") + 
  ggtitle("Relative vertical velocity wiggles (0.35m)") + 
  xlab("Predicted (wiggles)")+
  ylab("Reference (video)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels
#CM_Simeone

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups 
CM_Takahashi = 
  ggplot(data = Takahashi_table, 
         aes(x = Prediction, y = Reference,  fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = Freq), vjust = 0.8, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
  # xlim(levels(Takahashi_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
#  ggtitle("Takahashi et al. 2004") + 
  ggtitle("Single changepoint wiggles") + 
  xlab("Predicted (wiggles)")+
  ylab("Reference (video)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels  
#CM_Takahashi

Figure3_pyth = cowplot::plot_grid(CM_Simeone, CM_Takahashi, CM_Halsey_1m, CM_Halsey_2m, ncol = 2,
                                  align = "h", axis = "tblr") 

Figure3_pyth

# Save as PNG
ggsave("./figures/Figure3_confusion_matrix_pyth_strict.png", Figure3_pyth, width = 4, height = 4, dpi = 1200)

#-------------------------------------------------
# Plot confusion matrices - Approach 2 and stats
#-------------------------------------------------

#library(cvms) plots reference and prediction cells on the opposite axis than python.
# this is not wrong, but I used the python orientation.
# We can still use the cvmr measures of accuracy, etc.

#library(cvms)
# plot_confusion_matrix(Halsey_1m_table, 
#                       target_col = "Reference", 
#                       prediction_col = "Prediction",
#                       counts_col = "Freq") 

library(caret)

cm_Halsey_1m = confusionMatrix(
  data = as.factor(wiggledepth$pred_wiggleN_Halsey_1m), # a factor of predicted classes
  reference = as.factor(wiggledepth$foraging_dive), # a factor of classes to be used as the true results
  positive = "foraging") 
cm_Halsey_1m$byClass

cm_Halsey_2m = confusionMatrix(
  data = as.factor(wiggledepth$pred_wiggleN_Halsey_2m), # a factor of predicted classes
  reference = as.factor(wiggledepth$foraging_dive), # a factor of classes to be used as the true results
  positive = "foraging") 
cm_Halsey_2m$byClass

cm_Simeone = confusionMatrix(
  data = as.factor(wiggledepth$pred_wiggleN_Simeone), # a factor of predicted classes
  reference = as.factor(wiggledepth$foraging_dive), # a factor of classes to be used as the true results
  positive = "foraging") 
cm_Simeone$byClass

cm_Takahashi = confusionMatrix(
  data = as.factor(wiggledepth$pred_wiggleN_Takahashi), # a factor of predicted classes
  reference = as.factor(wiggledepth$foraging_dive), # a factor of classes to be used as the true results
  positive = "foraging") 
cm_Takahashi$byClass

metrics = c('Sensitivity',
            'Specificity',
            'Pos Pred Value',
            'Neg Pred Value',
            'Precision',
            'Recall',
            'F1',
            'Prevalence',
            'Detection Rate',
            'Detection Prevalence',
            'Balanced Accuracy')

tableS2 = cbind(metrics, 
                round(as.data.frame(cm_Simeone$byClass),2),
                round(as.data.frame(cm_Takahashi$byClass),2),
                round(as.data.frame(cm_Halsey_1m$byClass),2),
                round(as.data.frame(cm_Halsey_2m$byClass),2))

names(tableS2) = c("Performance metrics", "Simeone and Wilson 2003 (0.3m)", "Takahashi et al. 2004",
                   "Halsey et al. 2007 (1m)", "Halsey et al. 2007 (2m)")
tableS2

write.table(tableS2, './supplement/tableS2_strict.csv', sep = ",", row.names = F)

#---------------aside----------------------------
# By hand
TP = 658
FP = 357
FN = 29
TN = 1427

# sensitivity = recall: measures the proportion of positive responses correctly identified
# (The ability of the classifier to find all the positive samples) 
(sensitivity = TP / (TP + FN)) # also known as recall

# Specificity: measures the proportion of the negative responses correctly identified
(specificity = TN / (FP + TN))

# Precision: The ability of the classifier not to label a negative sample as positive.
(precision = TP / (TP + FP))

(accuracy = (TP + TN) / (TP + FP + TN + FN))


