# Clear the Global Environment
rm(list = ls())

#install.packages("pacman")
# Load necessary packages
pacman::p_load(ggplot2, tidyverse, stringr, googledrive, 
               psych, ez, reshape2, Rmisc, ggsignif, car, emmeans,
               lme4, lmerTest, sjPlot,corrplot, irr, gridExtra, 
               viridis, cowplot,ggplotify)

getwd()
setwd("/data/Kathryn/Projects/Covid_Diary/October_CD_23")

#load in CSV files 
free_recall_df <- read.csv('all_scored_parsed_cleaned.csv')
pre_assess_collapsed_df <- read.csv('collapsed_scores_pre.csv')
post_assess_collapsed_df <- read.csv('collapsed_scores_post.csv')
category_counts_df <- read.csv('category_counts_df_cleaned.csv')
pre_assess_non_collapsed_df <- read.csv('non_collapsed_scores_pre.csv')
post_assess_non_collapsed_df <- read.csv('non_collapsed_scores_post.csv')
B_human_scored_df <- read.csv('B_cleaned_human.csv')
category_counts_df_humanB <- read.csv('cat_counts_human_B.csv')

###Quick CLEANING### 

#removing row 1 category_counts_df_humanB
category_counts_df_humanB <- category_counts_df_humanB[-1, ]
##adding group col to collapsed dfs 
pre_assess_collapsed_df$group<-pre_assess_non_collapsed_df$group
post_assess_collapsed_df$group<-post_assess_non_collapsed_df$group

# Remove the first and last columns
category_counts_df <- category_counts_df[, -c(1)]

# Remove leading and trailing spaces in the column name
colnames(pre_assess_collapsed_df) <- trimws(colnames(pre_assess_collapsed_df))
colnames(post_assess_collapsed_df) <- trimws(colnames(post_assess_collapsed_df))
# Assuming you have a data frame named 'pre_assess_collapsed_df'
# Rename the columns
names(pre_assess_collapsed_df)[names(pre_assess_collapsed_df) == "PANAS.Neg"] <- "PANAS_Neg"
names(pre_assess_collapsed_df)[names(pre_assess_collapsed_df) == "PANAS.Pos"] <- "PANAS_Pos"
names(post_assess_collapsed_df)[names(post_assess_collapsed_df) == "PANAS.Neg"] <- "PANAS_Neg"
names(post_assess_collapsed_df)[names(post_assess_collapsed_df) == "PANAS.Pos"] <- "PANAS_Pos"

##rename col in B_human_scored_df
colnames(B_human_scored_df)[1] <- "code"

# Merge category_counts_df_humanB with subject_id from category_counts_df based on matching columns
humanB_scored_categories_df <- merge(category_counts_df_humanB, category_counts_df[, c("title", "group", "timepoint", "subject_id")], 
                                     by = c("title", "group", "timepoint"), all.x = TRUE)

humanB_scored_categories_df$row_sum <- rowSums(humanB_scored_categories_df[, 4:11])


###Correlation with collapsed DASS and IUS###################################################### 
# Select the columns you want for correlation analysis
selected_data_pre_corr <- pre_assess_collapsed_df %>%
  select(STAI, PANAS_Neg, PANAS_Pos, DASS, IUS)
# Select the columns you want for correlation analysis
selected_data_post_corr <- post_assess_collapsed_df %>%
  select(STAI, PANAS_Neg, PANAS_Pos, DASS, IUS)

#PRE
# Calculate the correlation matrix
correlation_matrix_pre <- cor(selected_data_pre_corr, use = "complete.obs")

# Set your significance level (alpha)
alpha <- 0.05  # You can adjust this value as needed

# Loop through all pairs of columns and compute correlations with p-values
for (i in 1:(ncol(selected_data_pre_corr) - 1)) {
  for (j in (i + 1):ncol(selected_data_pre_corr)) {
    # Compute correlation test
    cor_test_result <- cor.test(selected_data_pre_corr[[i]], selected_data_pre_corr[[j]], method = "pearson")
    
    # Determine if the correlation is statistically significant
    is_significant <- cor_test_result$p.value < alpha
    
    # Print results with True/False based on significance
    cat(
      sprintf(
        "Correlation between %s and %s: %.4f (p-value: %.4f) Significant: %s\n",
        colnames(selected_data_pre_corr)[i],
        colnames(selected_data_pre_corr)[j],
        cor_test_result$estimate,
        cor_test_result$p.value,
        is_significant
      )
    )
  }
}

# Set up the PNG device with path to the subfolder
#png("plots/corrplot_pre_assessments_c.png", width=12, height=7, res=800, units="in")
# Generate and plot the correlation plot
reversed_magma_colors <- rev(magma(100))
corrplot(correlation_matrix_pre, method = "number", type = "lower", order = "hclust",
         tl.col = "black", tl.srt = 45, col = reversed_magma_colors)
#title("Corr Plot of Pre Assessments", line = 1, cex.main = 1.2)
# Close the PNG device
#dev.off()

#POST 
# Calculate the correlation matrix
correlation_matrix_post <- cor(selected_data_post_corr, use = "complete.obs")
# Set your significance level (alpha)
alpha <- 0.05  # You can adjust this value as needed

# Loop through all pairs of columns and compute correlations with p-values
for (i in 1:(ncol(selected_data_post_corr) - 1)) {
  for (j in (i + 1):ncol(selected_data_post_corr)) {
    # Compute correlation test
    cor_test_result <- cor.test(selected_data_post_corr[[i]], selected_data_post_corr[[j]], method = "pearson")
    
    # Determine if the correlation is statistically significant
    is_significant <- cor_test_result$p.value < alpha
    
    # Print results with True/False based on significance
    cat(
      sprintf(
        "Correlation between %s and %s: %.4f (p-value: %.4f) Significant: %s\n",
        colnames(selected_data_post_corr)[i],
        colnames(selected_data_post_corr)[j],
        cor_test_result$estimate,
        cor_test_result$p.value,
        is_significant
      )
    )
  }
}
# Set up the PNG device with path to the subfolder
#png("plots/corrplot_post_assessments_c.png", width=12, height=7, res=800, units="in")
# Generate and plot the correlation plot
reversed_magma_colors <- rev(magma(100))
corrplot(correlation_matrix_post, method = "number", type = "lower", order = "hclust",
         tl.col = "black", tl.srt = 45, col = reversed_magma_colors)
#title("Corr Plot of post Assessments", line = 1, cex.main = 1.2)
# Close the PNG device
#dev.off()


# Perform a paired t-test
STAI_c <- t.test(selected_data_pre_corr$STAI, selected_data_post_corr$STAI, paired = TRUE)
PANAS_Neg_c <- t.test(selected_data_pre_corr$PANAS_Neg, selected_data_post_corr$PANAS_Neg, paired = TRUE)
PANAS_Pos_c <- t.test(selected_data_pre_corr$PANAS_Pos, selected_data_post_corr$PANAS_Pos, paired = TRUE)
DASS_c <- t.test(selected_data_pre_corr$DASS, selected_data_post_corr$DASS, paired = TRUE)
IUS_c <- t.test(selected_data_pre_corr$IUS, selected_data_post_corr$IUS, paired = TRUE)


# Perform paired t-tests for each variable and store the results
results_ttest_c <- data.frame(
  Variable = c("STAI_c", "PANAS_Neg_c", "PANAS_Pos_c", "DASS_c", "IUS_c"),
  t_statistic = c(STAI_c$statistic, PANAS_Neg_c$statistic, PANAS_Pos_c$statistic, DASS_c$statistic, IUS_c$statistic),
  p_value = c(STAI_c$p.value, PANAS_Neg_c$p.value, PANAS_Pos_c$p.value, DASS_c$p.value, IUS_c$p.value)
)

# Print the results
print(results_ttest_c)


###Non collapsed################################################################## 
# Select the columns you want for correlation analysis
selected_data_pre_corr_nc <- pre_assess_non_collapsed_df %>%
  select(stai_pre_scores, panas_pre_neg_scores, panas_pre_pos_scores, ius_F1_pre_scores
         , iusF2_pre_scores,dassA_pre_scores,dassS_pre_scores,dassD_pre_scores)
#PRE
# Calculate the correlation matrix
correlation_matrix_pre_nc <- cor(selected_data_pre_corr_nc, use = "complete.obs")

# Set your significance level (alpha)
alpha <- 0.05  # You can adjust this value as needed

# Loop through all pairs of columns and compute correlations with p-values
for (i in 1:(ncol(selected_data_pre_corr_nc) - 1)) {
  for (j in (i + 1):ncol(selected_data_pre_corr_nc)) {
    # Compute correlation test
    cor_test_result <- cor.test(selected_data_pre_corr_nc[[i]], selected_data_pre_corr_nc[[j]], method = "pearson")
    
    # Determine if the correlation is statistically significant
    is_significant <- cor_test_result$p.value < alpha
    
    # Print results with True/False based on significance
    cat(
      sprintf(
        "Correlation between %s and %s: %.4f (p-value: %.4f) Significant: %s\n",
        colnames(selected_data_pre_corr_nc)[i],
        colnames(selected_data_pre_corr_nc)[j],
        cor_test_result$estimate,
        cor_test_result$p.value,
        is_significant
      )
    )
  }
}

# Set up the PNG device with path to the subfolder
#png("plots/corrplot_pre_assessments_nc.png", width=12, height=7, res=800, units="in")
# Generate and plot the correlation plot
reversed_magma_colors <- rev(magma(100))
correlation_matrix_pre_nc<-corrplot(correlation_matrix_pre_nc, method = "number", type = "lower", order = "hclust",
         tl.col = "black", tl.srt = 45, col = reversed_magma_colors)
#title("Corr Plot of Pre Assessments Non-Collapsed", line = 1, cex.main = 1.2)
# Close the PNG device
#dev.off()

#POST 
# Select the columns you want for correlation analysis
selected_data_post_corr_nc <- post_assess_non_collapsed_df %>%
  select(stai_post_scores, panas_post_neg_scores, panas_post_pos_scores, ius_F1_post_scores
         , iusF2_post_scores,dassA_post_scores,dassS_post_scores,dassD_post_scores)
#post
# Calculate the correlation matrix
correlation_matrix_post_nc <- cor(selected_data_post_corr_nc, use = "complete.obs")

# Set your significance level (alpha)
alpha <- 0.05  # You can adjust this value as needed

# Loop through all pairs of columns and compute correlations with p-values
for (i in 1:(ncol(selected_data_post_corr_nc) - 1)) {
  for (j in (i + 1):ncol(selected_data_post_corr_nc)) {
    # Compute correlation test
    cor_test_result <- cor.test(selected_data_post_corr_nc[[i]], selected_data_post_corr_nc[[j]], method = "pearson")
    
    # Determine if the correlation is statistically significant
    is_significant <- cor_test_result$p.value < alpha
    
    # Print results with True/False based on significance
    cat(
      sprintf(
        "Correlation between %s and %s: %.4f (p-value: %.4f) Significant: %s\n",
        colnames(selected_data_post_corr_nc)[i],
        colnames(selected_data_post_corr_nc)[j],
        cor_test_result$estimate,
        cor_test_result$p.value,
        is_significant
      )
    )
  }
}

# Set up the PNG device with path to the subfolder
#png("plots/corrplot_post_assessments_nc.png", width=12, height=7, res=800, units="in")
# Generate and plot the correlation plot
reversed_magma_colors <- rev(magma(100))
correlation_matrix_post_nc<-corrplot(correlation_matrix_post_nc, method = "number", type = "lower", order = "hclust",
         tl.col = "black", tl.srt = 45, col = reversed_magma_colors)
#title("Corr Plot of post Assessments Non-Collapsed", line = 1, cex.main = 1.2)
# Close the PNG device
#dev.off()

#paired t test NC 
# Perform a paired t-test with pre and post 
STAI_nc <- t.test(selected_data_pre_corr_nc$stai_pre_scores, selected_data_post_corr_nc$stai_post_scores, paired = TRUE)
PANAS_Neg_nc <- t.test(selected_data_pre_corr_nc$panas_pre_neg_scores, selected_data_post_corr_nc$panas_post_neg_scores, paired = TRUE)
PANAS_Pos_nc <- t.test(selected_data_pre_corr_nc$panas_pre_pos_scores, selected_data_post_corr_nc$panas_post_pos_scores, paired = TRUE)
DASS_A_nc <- t.test(selected_data_pre_corr_nc$dassA_pre_scores, selected_data_post_corr_nc$dassA_post_scores, paired = TRUE)
DASS_S_nc <- t.test(selected_data_pre_corr_nc$dassS_pre_scores, selected_data_post_corr_nc$dassS_post_scores, paired = TRUE)
DASS_D_nc <- t.test(selected_data_pre_corr_nc$dassD_pre_scores, selected_data_post_corr_nc$dassD_post_scores, paired = TRUE)
IUS_F1_nc <- t.test(selected_data_pre_corr_nc$ius_F1_pre_scores, selected_data_post_corr_nc$ius_F1_post_scores, paired = TRUE)
IUS_F2_nc <- t.test(selected_data_pre_corr_nc$iusF2_pre_scores, selected_data_post_corr_nc$iusF2_post_scores, paired = TRUE)

# Perform paired t-tests for each variable and store the results
results_ttest_nc <- data.frame(
  Variable = c("STAI_nc", "PANAS_Neg_nc", "PANAS_Pos_nc", "DASS_A_nc", "DASS_S_nc","DASS_D_nc","IUS_F1_nc","IUS_F2_nc"),
  t_statistic = c(STAI_nc$statistic, PANAS_Neg_nc$statistic, PANAS_Pos_nc$statistic, DASS_A_nc$statistic,
                  DASS_S_nc$statistic, DASS_D_nc$statistic, IUS_F1_nc$statistic,IUS_F2_nc$statistic),
  p_value = c(STAI_nc$p.value, PANAS_Neg_nc$p.value, PANAS_Pos_nc$p.value, DASS_A_nc$p.value,
              DASS_S_nc$p.value, DASS_D_nc$p.value, IUS_F1_nc$p.value,IUS_F2_nc$p.value)
)

# Print the results
print(results_ttest_nc)
####################################################################################
###Anxiety and Detail Count Correlations 
####START### 

#collapsing pre and post survey scores 
avg_assess_scores_AB <- data.frame(subject_id = pre_assess_non_collapsed_df$subject_id)
avg_assess_scores_AB$group <-pre_assess_non_collapsed_df$group
avg_assess_scores_AB$STAI <- (pre_assess_non_collapsed_df$stai_pre_scores + post_assess_non_collapsed_df$stai_post_scores) / 2
avg_assess_scores_AB$PANAS_neg <- (pre_assess_non_collapsed_df$panas_pre_neg_scores + post_assess_non_collapsed_df$panas_post_neg_scores) / 2
avg_assess_scores_AB$PANAS_pos <- (pre_assess_non_collapsed_df$panas_pre_pos_scores + post_assess_non_collapsed_df$panas_post_pos_scores) / 2
avg_assess_scores_AB$IUS_F1 <- (pre_assess_non_collapsed_df$ius_F1_pre_scores + post_assess_non_collapsed_df$ius_F1_post_scores) / 2
avg_assess_scores_AB$IUS_F2 <- (pre_assess_non_collapsed_df$iusF2_pre_scores + post_assess_non_collapsed_df$iusF2_post_scores) / 2
avg_assess_scores_AB$DASS_A <- (pre_assess_non_collapsed_df$dassA_pre_scores + post_assess_non_collapsed_df$dassA_post_scores) / 2
avg_assess_scores_AB$DASS_S <- (pre_assess_non_collapsed_df$dassS_pre_scores + post_assess_non_collapsed_df$dassS_post_scores) / 2
avg_assess_scores_AB$DASS_D <- (pre_assess_non_collapsed_df$dassD_pre_scores + post_assess_non_collapsed_df$dassD_post_scores) / 2
avg_assess_scores_AB$DASS_col <- (pre_assess_collapsed_df$DASS + post_assess_collapsed_df$DASS) / 2
avg_assess_scores_AB$IUS_col <- (pre_assess_collapsed_df$IUS + post_assess_collapsed_df$IUS) / 2

#subtracting post from pre 
difference_df <- data.frame(
  subject_id = post_assess_non_collapsed_df$subject_id,
  group = post_assess_non_collapsed_df$group,
  
  # Subtracting the pre-scores from the post-scores for each measure
  stai_diff = post_assess_non_collapsed_df$stai_post_scores - pre_assess_non_collapsed_df$stai_pre_scores,
  panas_neg_diff = post_assess_non_collapsed_df$panas_post_neg_scores - pre_assess_non_collapsed_df$panas_pre_neg_scores,
  panas_pos_diff = post_assess_non_collapsed_df$panas_post_pos_scores - pre_assess_non_collapsed_df$panas_pre_pos_scores,
  ius_F1_diff = post_assess_non_collapsed_df$ius_F1_post_scores - pre_assess_non_collapsed_df$ius_F1_pre_scores,
  iusF2_diff = post_assess_non_collapsed_df$iusF2_post_scores - pre_assess_non_collapsed_df$iusF2_pre_scores,
  dassA_diff = post_assess_non_collapsed_df$dassA_post_scores - pre_assess_non_collapsed_df$dassA_pre_scores,
  dassS_diff = post_assess_non_collapsed_df$dassS_post_scores - pre_assess_non_collapsed_df$dassS_pre_scores,
  dassD_diff = post_assess_non_collapsed_df$dassD_post_scores - pre_assess_non_collapsed_df$dassD_pre_scores
)
###organizing dfs for easier analysis 
##adding_avg to col names 
#averaged between pre and post scores
colnames(avg_assess_scores_AB)[3:ncol(avg_assess_scores_AB)] <- paste0(colnames(avg_assess_scores_AB)[3:ncol(avg_assess_scores_AB)], "_avg")
##preassessment non collapsed rename 
pre_assess_non_collapsed_df$IUS_pre_col <- pre_assess_collapsed_df$IUS
pre_assess_non_collapsed_df$DASS_pre_col <- pre_assess_collapsed_df$DASS
assess_pre_df<-pre_assess_non_collapsed_df
##postassessment non collapsed rename 
post_assess_non_collapsed_df$IUS_post_col <- post_assess_collapsed_df$IUS
post_assess_non_collapsed_df$DASS_post_col <- post_assess_collapsed_df$DASS
assess_post_df<-post_assess_non_collapsed_df
##merging all assessments into one df 
assessment_scores_pre_post<-merge(assess_pre_df, assess_post_df, c(by="subject_id","group"))
assessment_scores_pre_post_diff<-merge(assessment_scores_pre_post, difference_df, c(by="subject_id","group"))
assessment_scores<-merge(assessment_scores_pre_post_diff, avg_assess_scores_AB, c(by="subject_id","group"))
view(assessment_scores)
#merge details and assessments 
assessANDdetails_merged_df<-merge(category_counts_df,assessment_scores, c(by="subject_id","group"))
#isolate by surevy or recall 
assessANDdetails_merged_survey_df <- assessANDdetails_merged_df[assessANDdetails_merged_df$timepoint == "survey", ]
assessANDdetails_merged_recall_df <- assessANDdetails_merged_df[assessANDdetails_merged_df$timepoint == "recall", ]

#CORRELATIONS|SURVEY #######################################################
#just simple correlation tests
names(assessANDdetails_merged_survey_df)

variables_survey <- c(
  "stai_pre_scores", "panas_pre_neg_scores", 
  "panas_pre_pos_scores", "ius_F1_pre_scores", "iusF2_pre_scores", 
  "dassA_pre_scores", "dassS_pre_scores", "dassD_pre_scores", 
  "IUS_pre_col", "DASS_pre_col", "stai_post_scores", "panas_post_neg_scores", 
  "panas_post_pos_scores", "ius_F1_post_scores", "iusF2_post_scores", 
  "dassA_post_scores", "dassS_post_scores", "dassD_post_scores", 
  "IUS_post_col", "DASS_post_col", "stai_diff", "panas_neg_diff", 
  "panas_pos_diff", "ius_F1_diff", "iusF2_diff", "dassA_diff", 
  "dassS_diff", "dassD_diff", "STAI_avg", "PANAS_neg_avg", 
  "PANAS_pos_avg", "IUS_F1_avg", "IUS_F2_avg", "DASS_A_avg", 
  "DASS_S_avg", "DASS_D_avg", "DASS_col_avg", "IUS_col_avg"
)


results_survey <- list()

for (var in variables_survey) {
  # check if both variables have no missing values, as cor.test doesn't handle NA values well
  if (all(!is.na(assessANDdetails_merged_survey_df$row_sum)) & all(!is.na(assessANDdetails_merged_survey_df[[var]]))) {
    results_survey[[var]] <- cor.test(assessANDdetails_merged_survey_df$row_sum, assessANDdetails_merged_survey_df[[var]])
  } else {
    results_survey[[var]] <- NA
  }
}
# Print results
results_survey
# Print result names 
names(results_survey)
#example to access individual results 
results_survey[["dassA_pre_scores"]]

##multilevel model
##normalizing detail count recall 
# Create a z-score transformation function
z_score <- function(x) {
  (x - mean(x)) / sd(x)
}

#Apply the z-score transformation to your variable
assessANDdetails_merged_recall_df$row_sum_z <- z_score(assessANDdetails_merged_recall_df$row_sum)
plot(density(assessANDdetails_merged_recall_df$row_sum_z), main="Density Plot of row_sum")
##log transformation
assessANDdetails_merged_recall_df$row_sum_log <- log1p(assessANDdetails_merged_recall_df$row_sum)  # log1p adds 1 before taking log to handle zeros
plot(density(assessANDdetails_merged_recall_df$row_sum_log), main="Density Plot of row_sum")
#sqrt transformation 
assessANDdetails_merged_recall_df$row_sum_sqrt <- sqrt(assessANDdetails_merged_recall_df$row_sum)
plot(density(assessANDdetails_merged_recall_df$row_sum_sqrt), main="Density Plot of row_sum")

##DASS-A Survey 
dassA_pre_survey_model <- lmer(row_sum ~ dassA_pre_scores + (dassA_pre_scores | subject_id), data = assessANDdetails_merged_survey_df)
summary(dassA_pre_survey_model)
anova(dassA_pre_survey_model)
view(assessANDdetails_merged_recall_df)
dassA_post_survey_model <- lmer(row_sum ~ dassA_post_scores + (dassA_post_scores | subject_id), data = assessANDdetails_merged_survey_df)
summary(dassA_post_survey_model)
anova(dassA_post_survey_model)

#DASS-A Recall 
model1<-lmer(row_sum ~ dassA_post_scores + (dassA_post_scores | subject_id), data = assessANDdetails_merged_recall_df)
summary(model1)
anova(model1)


model1<-lmer(row_sum ~ panas_post_pos_scores + (panas_post_pos_scores | subject_id), data = assessANDdetails_merged_recall_df)
summary(model1)
anova(model1)


dassA_avg_survey_model <- lmer(row_sum ~ DASS_A_avg + (DASS_A_avg | subject_id), data = assessANDdetails_merged_survey_df)
summary(dassA_avg_survey_model)
anova(dassA_avg_survey_model)


# Square root transformation of the column
assessANDdetails_merged_recall_df$row_sum_sqrt <- sqrt(assessANDdetails_merged_recall_df$row_sum)
model_8 <- lmer(row_sum_sqrt ~ dassA_post_scores + (dassA_post_scores | subject_id), data = assessANDdetails_merged_recall_df)
summary(model_8)

model_5 <- lmer(row_sum ~ dassA_post_scores + (dassA_post_scores | subject_id), data = assessANDdetails_merged_recall_df)
summary(model_5 )
anova(model_5)



dassA_post_recall_model <- lmer(row_sum ~ dassA_post_scores + (dassA_post_scores | subject_id), data = assessANDdetails_merged_recall_df)
summary(dassA_post_recall_model)
anova(dassA_post_recall_model)

##multilevel model
##STAI Survey 
stai_pre_survey_model <- lmer(row_sum ~ stai_pre_scores  + (1 | subject_id), data = assessANDdetails_merged_survey_df)
summary(stai_pre_survey_model)
anova(stai_pre_survey_model)

stai_post_survey_model <- lmer(row_sum ~ stai_post_scores + (1 | subject_id), data = assessANDdetails_merged_survey_df)
summary(stai_post_survey_model)
anova(stai_post_survey_model)

#STAI recall 
stai_pre_recall_model <- lmer(row_sum ~ stai_pre_scores + (1 | subject_id), data = assessANDdetails_merged_recall_df)
summary(stai_pre_recall_model)
anova(stai_pre_recall_model)

stai_post_recall_model <- lmer(row_sum ~ stai_post_scores + (stai_post_scores | subject_id), data = assessANDdetails_merged_recall_df)
summary(stai_post_recall_model)
anova(stai_post_recall_model)

view(assessANDdetails_merged_recall_df)
# Plotting with switched axes
ggplot(assessANDdetails_merged_recall_df, aes(x = row_sum, y = dassA_pre_scores)) +
  geom_point(aes(color = factor(subject_id)), alpha = 0.5) + # Raw data points colored by subject
  geom_line(aes(x = predicted), color = "blue") +             # Predicted regression line (x-axis is now "predicted")
  labs(title = "Predicted Regression Line vs. Raw Data",
       y = "DASS-A Pre Scores",
       x = "Row Sum") +
  theme_minimal() +
  theme(legend.position = "none")
#CORRELATIONS|RECALL #######################################################
names(assessANDdetails_merged_recall_df)

variables_recall <- c(
  "stai_pre_scores", "panas_pre_neg_scores", 
  "panas_pre_pos_scores", "ius_F1_pre_scores", "iusF2_pre_scores", 
  "dassA_pre_scores", "dassS_pre_scores", "dassD_pre_scores", 
  "IUS_pre_col", "DASS_pre_col", "stai_post_scores", "panas_post_neg_scores", 
  "panas_post_pos_scores", "ius_F1_post_scores", "iusF2_post_scores", 
  "dassA_post_scores", "dassS_post_scores", "dassD_post_scores", 
  "IUS_post_col", "DASS_post_col", "stai_diff", "panas_neg_diff", 
  "panas_pos_diff", "ius_F1_diff", "iusF2_diff", "dassA_diff", 
  "dassS_diff", "dassD_diff", "STAI_avg", "PANAS_neg_avg", 
  "PANAS_pos_avg", "IUS_F1_avg", "IUS_F2_avg", "DASS_A_avg", 
  "DASS_S_avg", "DASS_D_avg", "DASS_col_avg", "IUS_col_avg"
)

results_recall <- list()

for (var in variables_recall) {
  # check if both variables have no missing values, as cor.test doesn't handle NA values well
  if (all(!is.na(assessANDdetails_merged_recall_df$row_sum)) & all(!is.na(assessANDdetails_merged_recall_df[[var]]))) {
    results_recall[[var]] <- cor.test(assessANDdetails_merged_recall_df$row_sum, assessANDdetails_merged_recall_df[[var]])
  } else {
    results_recall[[var]] <- NA
  }
}
# Print results
results_recall
# Print result names 
names(results_recall)
#example to access individual results 
results_recall[["stai_post_scores"]]

########################################################################################
##plotting correlations with detail counts and assessment scores 
# Function to convert p-values to significance stars
p_to_star <- function(p) {
  if (is.na(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  if (p < .1) return(".")
  return("")
}

##plotting one singular before looping 
# Let's try plotting just the DASS-A (Pre | Survey) plot
dassA_pre_survey_plot <- ggplot(assessANDdetails_merged_survey_df, aes(x = row_sum, y = dassA_pre_scores)) +
  geom_point(aes(color = factor(subject_id)), size = 3) +
  geom_jitter(aes(color = factor(subject_id)), size = 3, width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "DASS-A (Pre | Survey)", x = "Number of Details", y = "dassA_pre_scores") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "grey"),
        panel.border = element_rect(colour = "grey", fill = NA, linewidth = 1)) +
  annotate("text", x = min(assessANDdetails_merged_survey_df$row_sum), 
           y = max(assessANDdetails_merged_survey_df$dassA_pre_scores), 
           label = sprintf("r = %.2f\np = %.3f%s", 
                           results_survey$dassA_pre_scores$estimate, 
                           results_survey$dassA_pre_scores$p.value, 
                           p_to_star(results_survey$dassA_pre_scores$p.value)), 
           hjust = 0.0, vjust = 1) +
  scale_color_viridis(option = "magma", discrete = TRUE)
dassA_pre_survey_plot


##creat plot above function 
create_plot <- function(data, x_var, y_var, title, results) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(aes(color = factor(subject_id)), size = 3) +
    geom_jitter(aes(color = factor(subject_id)), size = 3, width = 0.2, height = 0.2) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = title, x = "Number of Details", y = y_var) +
    theme_minimal() +
    theme(legend.position = "none", 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "grey"),
          panel.border = element_rect(colour = "grey", fill = NA, linewidth = 1)) +
    annotate("text", x = min(data[[x_var]]), 
             y = max(data[[y_var]]), 
             label = sprintf("r = %.2f\np = %.3f%s", 
                             results$estimate, 
                             results$p.value, 
                             p_to_star(results$p.value)), 
             hjust = 0.0, vjust = 1) +
    scale_color_viridis(option = "magma", discrete = TRUE)
}


# List of dataframes, variables, and titles
dataframes <- list(assessANDdetails_merged_survey_df, assessANDdetails_merged_survey_df, 
                   assessANDdetails_merged_recall_df, assessANDdetails_merged_recall_df)

######DASS-A plots### 
y_vars <- c("dassA_pre_scores", "dassA_post_scores", "dassA_pre_scores", "dassA_post_scores")
titles <- c("DASS-A (Pre | Survey)", "DASS-A (Post | Survey)", "DASS-A (Pre | Recall)", "DASS-A (Post | Recall)")
results_list_dassA <- list(results_survey$dassA_pre_scores, results_survey$dassA_post_scores, 
                           results_recall$dassA_pre_scores, results_recall$dassA_post_scores)

plots_dassA <- list()

# Loop through each dataframe and create a plot
for (i in 1:4) {
  plots_dassA[[i]] <- create_plot(dataframes[[i]], "row_sum", y_vars[[i]], titles[[i]], results_list_dassA[[i]])
}

# Print the plots (or save them, or arrange them in a grid, as you wish)
for (plot in plots_dassA) {
  print(plot)
}

##putting all 4 plots onto one page 
grid.arrange(plots_dassA[[1]], plots_dassA[[2]], plots_dassA[[3]], plots_dassA[[4]], ncol=2)
##exporting the plots
# combined_plot_DASS-A <- gridExtra::arrangeGrob(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol=2)
# ggplot2::ggsave("DASS-A_pre_post.png", combined_plot_DASS, width = 10, height = 8)

##############################################################################

# List of dataframes, variables, and titles for STAI scores
y_vars_stai <- c("stai_pre_scores", "stai_post_scores", "stai_pre_scores", "stai_post_scores")
titles_stai <- c("STAI (Pre | Survey)", "STAI (Post | Survey)", "STAI (Pre | Recall)", "STAI (Post | Recall)")
# Assuming you have results for STAI scores similar to the DASS ones
results_list_stai <- list(results_survey$stai_pre_scores, results_survey$stai_post_scores, 
                          results_recall$stai_pre_scores, results_recall$stai_post_scores)
plots_stai <- list()
# Loop through each dataframe and create a plot for STAI scores
for (i in 1:4) {
  plots_stai[[i]] <- create_plot(dataframes[[i]], "row_sum", y_vars_stai[[i]], titles_stai[[i]], results_list_stai[[i]])
}

# Print the plots (or save them, or arrange them in a grid, as you wish)
for (plot in plots_stai) {
  print(plot)
}
## Putting all 4 plots for STAI scores onto one page 
grid.arrange(plots_stai[[1]], plots_stai[[2]], plots_stai[[3]], plots_stai[[4]], ncol=2)
## Exporting the plots for STAI scores
combined_plot_STAI <- gridExtra::arrangeGrob(plots_stai[[1]], plots_stai[[2]], plots_stai[[3]], plots_stai[[4]], ncol=2)
#ggplot2::ggsave("STAI_pre_post.png", combined_plot_STAI, width = 10, height = 8)


##############################################################################

###PANAS Postive### 
# List of dataframes, variables, and titles for PANAS_Pos scores
y_vars_panas_pos <- c("panas_pre_pos_scores", "panas_post_pos_scores", "panas_pre_pos_scores", "panas_post_pos_scores")
titles_panas_pos <- c("PANAS_Pos (Pre | Survey)", "PANAS_Pos (Post | Survey)", "PANAS_Pos (Pre | Recall)", "PANAS_Pos (Post | Recall)")
# Assuming you have results for PANAS_Pos scores similar to the DASS ones
results_list_panas_pos <- list(results_survey$panas_pre_pos_scores, results_survey$panas_post_pos_scores, 
                          results_recall$panas_pre_pos_scores, results_recall$panas_post_pos_scores)
plots_panas_pos <- list()
# Loop through each dataframe and create a plot for PANAS_Pos scores
for (i in 1:4) {
  plots_panas_pos[[i]] <- create_plot(dataframes[[i]], "row_sum", y_vars_panas_pos[[i]], titles_panas_pos[[i]], results_list_panas_pos[[i]])
}

# Print the plots (or save them, or arrange them in a grid, as you wish)
for (plot in plots_panas_pos) {
  print(plot)
}
## Putting all 4 plots for PANAS_Pos scores onto one page 
grid.arrange(plots_panas_pos[[1]], plots_panas_pos[[2]], plots_panas_pos[[3]], plots_panas_pos[[4]], ncol=2)
## Exporting the plots for PANAS_Pos scores
combined_plot_PANAS_Pos <- gridExtra::arrangeGrob(plots_panas_pos[[1]], plots_panas_pos[[2]], plots_panas_pos[[3]], plots_panas_pos[[4]], ncol=2)
ggplot2::ggsave("plots/PANAS_Pos_pre_post.png", combined_plot_PANAS_Pos, width = 13.5, height = 15, dpi = 800)

names(assessANDdetails_merged_survey_df)



###importing vividness ratings ##########################################
# Set the directory where the CSV files are located
memory_test_dir <- ("/data/Kathryn/Projects/Covid_Diary/data/vividness_data")

# get vividness ratings from event test
all_test_files <- list.files(path = memory_test_dir, pattern = glob2rx("*test_events.csv"), full.names = TRUE)
test_events_vividness <- data.frame()
for (f in all_test_files) {
  sub_data <- read_csv(f, show_col_types = F)
  test_events_vividness <- rbind(test_events_vividness, sub_data)
}
test_events_vividness <- test_events_vividness %>%
  #select(subject_id, title, survey_day, test_vividness) %>%
  mutate(test_type = "event",
         subject_id = as.character(subject_id))

# get vividness ratings from pair test
all_pair_files <- list.files(path = memory_test_dir, pattern = glob2rx("*test_pairs.csv"), full.names = TRUE)
test_pair_vividness <- data.frame()
for (f in all_pair_files) {
  sub_data <- read_csv(f, show_col_types = F)
  test_pair_vividness <- rbind(test_pair_vividness, sub_data)
}
test_pair_vividness <- test_pair_vividness %>%
  #select(subject_id, title, survey_day, test_vividness) %>%
  mutate(test_type = "pair",
         subject_id = as.character(subject_id))






##re-running Mia's summer work 
#COLLAPSED B GPT SCORED ONLY SURVEY because b had better quality details  
##subsetting only group B and survey 
subset_df_c <- pre_asses_merge_and_cat_collapse[pre_asses_merge_and_cat_collapse$group == "B" & 
                                                  pre_asses_merge_and_cat_collapse$timepoint == "survey", ]
#*A Pearson's product-moment correlation analysis revealed a statistically significant positive association between the number of details and STAI pre-scores (r = 0.159, p < .001). This suggests a moderate, positive relationship, indicating that as the number of details increased, STAI pre-scores tended to increase as well.
STAI_pre_c_sub <- cor.test(subset_df_c$row_sum, subset_df_c$STAI)
STAI_pre_c_sub

PANAS_neg_pre_c_sub <- cor.test(subset_df_c$row_sum, subset_df_c$PANAS_Neg)
PANAS_neg_pre_c_sub

PANAS_pos_pre_c_sub <- cor.test(subset_df_c$row_sum, subset_df_c$PANAS_Pos)
PANAS_pos_pre_c_sub

IUS_pre_c_sub <- cor.test(subset_df_c$row_sum, subset_df_c$IUS)
IUS_pre_c_sub

DASS_pre_c_sub <- cor.test(subset_df_c$row_sum, subset_df_c$DASS)
DASS_pre_c_sub

##subsetting only survey group B assessments and merging with human scored non collapsed 
subsetB_df_assessments_pre_nc <- pre_asses_merge_and_cat_noncollapse[pre_asses_merge_and_cat_noncollapse$group == "B" & 
                                                                       pre_asses_merge_and_cat_noncollapse$timepoint == "survey", ]


IUS_pre_F1_NC_sub <- cor.test(subsetB_df_assessments_pre_nc$row_sum, subsetB_df_assessments_pre_nc$ius_F1_pre_scores)
IUS_pre_F1_NC_sub

IUS_pre_F2_NC_sub <- cor.test(subsetB_df_assessments_pre_nc$row_sum, subsetB_df_assessments_pre_nc$iusF2_pre_scores)
IUS_pre_F2_NC_sub

DASS_pre_A_NC_sub <- cor.test(subsetB_df_assessments_pre_nc$row_sum, subsetB_df_assessments_pre_nc$dassA_pre_scores)
DASS_pre_A_NC_sub

DASS_pre_S_NC <- cor.test(subsetB_df_assessments_pre_nc$row_sum, subsetB_df_assessments_pre_nc$dassS_pre_scores)
DASS_pre_S_NC

DASS_pre_D_NC_sub <- cor.test(subsetB_df_assessments_pre_nc$row_sum, subsetB_df_assessments_pre_nc$dassD_pre_scores)
DASS_pre_D_NC_sub

##checking Mia's work B and human scored 
# Add the "subject_id" column from df2 to df1 based on matching columns
# Merge category_counts_df_humanB with subject_id from category_counts_df based on matching columns
humanB_scored_categories_df <- merge(category_counts_df_humanB, category_counts_df[, c("title", "group", "timepoint", "subject_id")], 
                   by = c("title", "group", "timepoint"), all.x = TRUE)

humanB_scored_categories_df$row_sum <- rowSums(humanB_scored_categories_df[, 4:11])

#humanscored correlations 
humanB_merged_df_pre_nc<- merge(humanB_scored_categories_df, subsetB_df_assessments_pre_nc, c(by="subject_id","group"))
view(humanB_merged_df_pre_nc)

# Calculate the correlation while ignoring NAs
#X
STAI_pre_c_sub_hum <- cor.test(humanB_merged_df_pre_nc$row_sum, humanB_merged_df_pre_nc$stai_pre_scores)
STAI_pre_c_sub_hum
#X
IUS_pre_F1_NC_hum <- cor.test(humanB_merged_df_pre_nc$row_sum, humanB_merged_df_pre_nc$ius_F1_pre_scores)
IUS_pre_F1_NC_hum
#X
IUS_pre_F2_NC_hum <- cor.test(humanB_merged_df_pre_nc$row_sum, humanB_merged_df_pre_nc$iusF2_pre_scores)
IUS_pre_F2_NC_hum
#X
DASS_pre_A_NC_hum <- cor.test(humanB_merged_df_pre_nc$row_sum, humanB_merged_df_pre_nc$dassA_pre_scores)
DASS_pre_A_NC_hum
#*
DASS_pre_S_NC_hum <- cor.test(humanB_merged_df_pre_nc$row_sum, humanB_merged_df_pre_nc$dassS_pre_scores)
DASS_pre_S_NC_hum
#X
DASS_pre_D_NC_hum <- cor.test(humanB_merged_df_pre_nc$row_sum, humanB_merged_df_pre_nc$dassD_pre_scores)
DASS_pre_D_NC_hum


##comparing human to chatGPT#############################################          
total_sum <- sum(category_counts_df$row_sum, na.rm = TRUE)
total_sum
human_counts_df<-humanB_scored_categories_df
gpt_counts_df<-category_counts_df

names(human_counts_df)
colnames(human_counts_df)[colnames(human_counts_df) == "Event"] <- "event_human"
colnames(human_counts_df)[colnames(human_counts_df) == "Time"] <- "time_human"
colnames(human_counts_df)[colnames(human_counts_df) == "Place"] <- "place_human"
colnames(human_counts_df)[colnames(human_counts_df) == "Thought.Emotion"] <- "tho_emo_human"
colnames(human_counts_df)[colnames(human_counts_df) == "Perceptual"] <- "perceptual_human"
colnames(human_counts_df)[colnames(human_counts_df) == "Semantic"] <- "semantic_human"
colnames(human_counts_df)[colnames(human_counts_df) == "Reflection"] <- "reflection_human"
colnames(human_counts_df)[colnames(human_counts_df) == "Extraneous"] <- "extraneous_human"
colnames(human_counts_df)[colnames(human_counts_df) == "row_sum"] <- "row_sum_human"
names(gpt_counts_df)
colnames(gpt_counts_df)[colnames(gpt_counts_df) == "Event"] <- "event_gpt"
colnames(gpt_counts_df)[colnames(gpt_counts_df) == "Time"] <- "time_gpt"
colnames(gpt_counts_df)[colnames(gpt_counts_df) == "Place"] <- "place_gpt"
colnames(gpt_counts_df)[colnames(gpt_counts_df) == "Thought.Emotion"] <- "tho_emo_gpt"
colnames(gpt_counts_df)[colnames(gpt_counts_df) == "Perceptual"] <- "perceptual_gpt"
colnames(gpt_counts_df)[colnames(gpt_counts_df) == "Semantic"] <- "semantic_gpt"
colnames(gpt_counts_df)[colnames(gpt_counts_df) == "Reflection"] <- "reflection_gpt"
colnames(gpt_counts_df)[colnames(gpt_counts_df) == "Extraneous"] <- "extraneous_gpt"
colnames(gpt_counts_df)[colnames(gpt_counts_df) == "row_sum"] <- "row_sum_gpt"

human_and_gpt_counts_df <- merge(gpt_counts_df, human_counts_df, by=c('title', 'timepoint', 'group','subject_id'))

row_sum_cor<-cor.test(human_and_gpt_counts_df$row_sum_gpt,human_and_gpt_counts_df$row_sum_human)
event_cor<-cor.test(human_and_gpt_counts_df$event_gpt,human_and_gpt_counts_df$event_human)
time_cor<-cor.test(human_and_gpt_counts_df$time_gpt,human_and_gpt_counts_df$time_human)
place_cor<-cor.test(human_and_gpt_counts_df$place_gpt,human_and_gpt_counts_df$place_human)
tho_emo_cor<-cor.test(human_and_gpt_counts_df$tho_emo_gpt,human_and_gpt_counts_df$tho_emo_human)
perceptual_cor<-cor.test(human_and_gpt_counts_df$perceptual_gpt,human_and_gpt_counts_df$perceptual_human)
semantic_cor<-cor.test(human_and_gpt_counts_df$semantic_gpt,human_and_gpt_counts_df$semantic_human)
extraneous_cor<-cor.test(human_and_gpt_counts_df$extraneous_gpt,human_and_gpt_counts_df$extraneous_human)
reflection_cor<-cor.test(human_and_gpt_counts_df$reflection_gpt,human_and_gpt_counts_df$reflection_human)


# Prepare the data for ggplot
cor_df <- data.frame(
  category = c("Row Sum", "Event", "Time", "Place", "Thought Emotion", "Perceptual", "Semantic", "Extraneous", "Reflection"),
  human = c(sum(human_and_gpt_counts_df$row_sum_human), sum(human_and_gpt_counts_df$event_human), sum(human_and_gpt_counts_df$time_human),
            sum(human_and_gpt_counts_df$place_human), sum(human_and_gpt_counts_df$tho_emo_human), sum(human_and_gpt_counts_df$perceptual_human),
            sum(human_and_gpt_counts_df$semantic_human), sum(human_and_gpt_counts_df$extraneous_human), sum(human_and_gpt_counts_df$reflection_human)),
  gpt = c(sum(human_and_gpt_counts_df$row_sum_gpt), sum(human_and_gpt_counts_df$event_gpt), sum(human_and_gpt_counts_df$time_gpt),
          sum(human_and_gpt_counts_df$place_gpt), sum(human_and_gpt_counts_df$tho_emo_gpt), sum(human_and_gpt_counts_df$perceptual_gpt),
          sum(human_and_gpt_counts_df$semantic_gpt), sum(human_and_gpt_counts_df$extraneous_gpt), sum(human_and_gpt_counts_df$reflection_gpt)),
  correlation = c(row_sum_cor$estimate, event_cor$estimate, time_cor$estimate, place_cor$estimate, tho_emo_cor$estimate, perceptual_cor$estimate,
                  semantic_cor$estimate, extraneous_cor$estimate, reflection_cor$estimate),
  p_value = c(row_sum_cor$p.value, event_cor$p.value, time_cor$p.value, place_cor$p.value, tho_emo_cor$p.value, perceptual_cor$p.value,
              semantic_cor$p.value, extraneous_cor$p.value, reflection_cor$p.value)
)


# Convert to long format for ggplot2
cor_df_long <- cor_df %>%
  gather(key = "type", value = "value", -category, -correlation, -p_value)

# Function to convert p-values to significance stars
p_value_to_star <- function(p) {
  if (p < 0.001) return('***')
  if (p < 0.01) return('**')
  if (p < 0.05) return('*')
  return('')
}


# Order categories based on the 'human' type correlation strength
ordered_categories <- cor_df_long %>%
  filter(type == "human") %>%
  arrange(-abs(correlation)) %>%  # use abs() to order by magnitude of correlation
  pull(category)

cor_df_long$category <- factor(cor_df_long$category, levels = ordered_categories)

# Create the plot
humanVgpt<-ggplot(cor_df_long, aes(x = category, y = value, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  
  # Adjust vjust to place text just above bars
  geom_text(data = subset(cor_df_long, type == "human"), 
            aes(label = sprintf("%.2f", correlation)), 
            position = position_dodge(0.8), vjust = -0.25) +
  
  geom_text(data = subset(cor_df_long, type == "human"),
            aes(label = sapply(p_value, p_value_to_star)), 
            position = position_dodge(0.8), vjust = -1.75) +
  
  # Specify colors for bars
  scale_fill_manual(values = c("human" = "#931a75", "gpt" = "#d5c312")) +
  
  theme_minimal() +
  labs(title = "Human vs GPT", y = "Detail Count",x = "Category") +
  theme(legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "grey"),
        panel.border = element_rect(colour = "grey", fill = NA, linewidth = 1))
humanVgpt
#ggplot2::ggsave("plots/humanVgpt_corr.png", humanVgpt, width = 10, height = 8.5, dpi = 800)


