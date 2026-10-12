# Created by Michele Remer
# Last updated 1-12-26

# This code is to ensure reproducibility of the paper titled "Alaska Visitors’ Knowledge about Invasive Species in a Warming Arctic   and Sub-Arctic" by Remer et al., 2026, submitted to the journal Biological Invasions. 

# Load Packages and Describe Data Cleaning -----------------------------------------------------------
# Load packages
library(dplyr)
library(tidyverse)
library(ggplot2)

# Datasets from all locations (Anchorage, Seward, and Nome) were tidied and added into one master dataset csv. We have further data that can be granted upon request, but only report the  Here, we report on specific questions

# set path
folder_der <- "../Data_Derived/"

# call file 
know_df <- read.csv(paste0(folder_der, "ak_visitor_know_data.csv"))

know_df <- read.csv("./Data_Processed/ak_visitor_knowledge/ak_visitor_know_data.csv")

# Data Tidying ---------------------------------------------------

# Calculated count_visit and not_clean_b within Excel. Both are a new variable, where count_visit only includes countries they have visited, and then splits those counts into four different groups. The not_clean_b variable was calculated by classifying an omitted answer or cleaned all items as 0, while 1 indicates that at least one item brought to a new area was not cleaned. 

# General Data Tidying

# Replace non-answer values (-99, -88, -77) with NA
know_df[know_df == -99 | know_df == -88 | know_df == -77 | know_df == -55] <- NA

# Count valid answers for each respondent and for the specific group of four questions
know_df$valid_answers_group <- rowSums(!is.na(know_df[, c("watercraft", "camp", "remove", "invas")]))

# Exclude respondents who did not answer at least two questions
know_index <- know_df[know_df$valid_answers_group >= 2, ]

# Calculate the percentage of the valid answers for each respondent-this was done as some respondents only had access to two questions when distributed a shorter version of the survey 

# Specify columns you want to include in the row sums
k_questions <- c("watercraft", "invas", "camp", "remove") 

# Sum the valid answers and divide by the valid_answers_count
know_index$percent_know <- rowSums(know_index[k_questions], na.rm = TRUE) / know_index$valid_answers_group * 100

# Data Tidying for Hypothesis 2:
# for hypothesis two: also exclude respondents who did not answer any of the cleaning questions
know_df$clean_group <- rowSums(!is.na(know_df[, c("e_clean_1", "e_clean_2", "e_clean_3", "e_clean_4", "e_clean_5")]))

# exclude respondents who did not answer at least one of the cleaning questions
know_clean <- know_df[know_df$clean_group >= 1, ] 

# repeat knowledge code here with this filter applied
# Count valid answers for each respondent and for the specific group of four questions
know_clean$valid_answers_group <- rowSums(!is.na(know_clean[, c("watercraft", "camp", "remove", "invas")]))

# Exclude respondents who did not answer at least two questions
know_c_index <- know_clean[know_clean$valid_answers_group >= 2, ] # leaves 220 respondents

# Calculate the percentage of the valid answers for each respondent

# Sum the valid answers and divide by the valid_answers_count
know_c_index$percent_know <- rowSums(know_c_index[k_questions], na.rm = TRUE) / know_c_index$valid_answers_group * 100

# Data Tidying for Hypothesis 3:

# Exclude respondents who did not answer at least two questions
know_v_index <- know_df[know_df$valid_answers_group >= 2, ]

# Sum the valid answers and divide by the valid_answers_count
know_v_index$percent_know <- rowSums(know_v_index[k_questions], na.rm = TRUE) / know_v_index$valid_answers_group * 100

# Filter out NA's
know_v_index <- know_index %>% drop_na(count_day)

# Arcsine Transformation -------------------------------------------------------

# Hypothesis 1 Transformation
know_index <- know_index %>%
  mutate(percent_know = percent_know / 100)

# Apply arcsine transformation to percent column, equation is y = arcsin(sqrt(proportion))

know_index$percent_t <- asin(sqrt(know_index$percent_know))

# Backtransform-same values as before
know_index <- know_index %>%
  mutate(per_back_t = sin(percent_t)^2)

# Comparison plot between original proportions and transformed
ggplot(know_index, aes(x = percent_know, y = percent_t)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "red", alpha = 0.5) +
  labs(
    title = "Arcsine Transformation Visualization-H1",
    x = "Original Proportions",
    y = "Transformed Values"
  ) +
  theme_minimal()

# Hypothesis 2 Transformation
know_c_index <- know_c_index %>%
  mutate(percent_know = percent_know / 100)

# Apply arcsine transformation to percent column, equation is y = arcsin(sqrt(proportion))

know_c_index$percent_t <- asin(sqrt(know_c_index$percent_know))

# Backtransform-same values as before
know_c_index <- know_c_index %>%
  mutate(per_back_t = sin(percent_t)^2)

# Comparison plot between original proportions and transformed
ggplot(know_c_index, aes(x = percent_know, y = percent_t)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "red", alpha = 0.5) +
  labs(
    title = "Arcsine Transformation Visualization-H2",
    x = "Original Proportions",
    y = "Transformed Values"
  ) +
  theme_minimal()

# Hypothesis 3-No Arcsine Transformation needed for ANOVA

# Analysis ------------------------------------------------------------------

# Hypothesis 1: watercraft owners will be more knowledgeable about invasives than non-watercraft owners, 

# Hypothesis 1 -----------------------------------------------------------------

# Help on interpreting output from: https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/, other help from https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html#running-a-mlr-in-r

# Dependent variable = percentage of correct questions, independent variable is watercraft. Since watercraft was a yes or no question, we are running linear regression with categorical predictors. if watercraft is dependent, then it would be a binary logistic regression (commented out)

# Convert the dependent variable to a factor
know_index$watercraft <- as.factor(know_index$watercraft)

# Run t-test on transformed knowledge score
t_test_h1 <- t.test(percent_t ~ watercraft, data = know_index)

# View the results of the t-test
print(t_test_h1)

# Hypothesis 2 -----------------------------------------------------------------

# Hypothesis 2: pro-environmental behaviors (cleaning invasives) will result in higher knowledge, 
# Not clean_b is did not clean at least one of the items that they brought with them, while 1 means they either cleaned all their gear or skipped the question. then filtered out everyone who skipped the entire question, so only people who cleaned all the gear they brought with them. This means that independent variable is a continuous variable (percent knowledge) so will run an ANOVA and dependent variable is categorical (binary), so will do linear regression and ANOVA again. 

# Convert the dependent variable to a factor 
know_c_index$not_clean_b <- as.factor(know_c_index$not_clean_b)

# Run t-test on transformed knowledge score
t_test_h2 <- t.test(percent_t ~ not_clean_b, data = know_c_index)

# View the results of the t-test
print(t_test_h2)

# Hypothesis 3 -----------------------------------------------------------------

# Hypothesis 3: highly traveled visitors will have a higher knowledge of invasive species. This will be an ANOVA because the independent variable has more than two categories, while our dependent variable stays the same. Could maybe do an overall model with all three independent variables. 

# Also run a ANOVA
# Convert to factor if haven't already
know_v_index$country_group <- as.factor(know_v_index$country_group)

# Run ANOVA
aov_h3 <- aov(percent_know ~ country_group, data = know_v_index)

plot(percent_know ~ country_group, data = know_v_index)

# View the results of the ANOVA. F-statistic: Measures the ratio of the variance between the group means to the variance within the groups. A higher F-statistic indicates a greater likelihood that the group means are different.
print(aov_h3)
summary(aov_h3)

# Perform Tukey's HSD post-hoc test
tukey_h3 <- TukeyHSD(aov_h3)

# View the results of the post-hoc test
print(tukey_h3)

plot(tukey_h3)

# Table 1. ---------------------------------------------------------------------
# Code for creating Table 1 in Remer et al., 2026

# H1: watercraft owners vs. those who don't own a boat
# Count responses first
water_count <- table(know_index$watercraft)
print(water_count)

# Average Knowledge Score by Category 
know_index %>%
  group_by(watercraft) %>%
  summarise(Average_K_Score = mean(percent_know, na.rm = TRUE))

# Hypothesis 2 Count Responses (see Hypothesis 2 under analysis for understanding of how reached know_c_index from know_index) 
clean_count <- table(know_c_index$not_clean_b)
print(clean_count)

# Average Knowledge Score by Category 
know_c_index %>%
  group_by(not_clean_b) %>%
  summarise(Average_K_Score = mean(percent_know, na.rm = TRUE))

# H3: Highly-traveled visitors more knowledgeable 
# Count responses first
visit_count <- table(know_v_index$country_group)
print(visit_count)

# Average Knowledge Score by Category 
know_v_index %>%
  group_by(country_group) %>%
  summarise(Average_K_Score = mean(percent_know, na.rm = TRUE))

# Revisions --------------------------------------------------------------------

# Page 3 - Lines 21-22: Did airline passengers represent a substantial number of respondents? If so, what percentage were in that category?

# Count the number of airline passengers for both unfiltered data (298 observations) and data filtered for knowledge test (262 observations)

# For all data
air <- know_df %>% 
  summarize(air = sum(str_count(cruise, "Air"), na.rm = TRUE))

print(air) # 15 airline passengers

# For data filtered for the knowledge test
air_2 <- know_df %>% 
  summarize(air = sum(str_count(cruise, "Air"), na.rm = TRUE))

print(air_2) # 14 airline passengers

# Page 4 - Line 7: Were percentage data arcsine transformed prior to t-tests to deal with percentages that fell below 30% and/or above 70%?

# Change percentages back to proportions (between 0 and 1) for arcsine transformatino (following guide from Steven Sanderson, https://www.spsanderson.com/steveondata/posts/2024-12-30/)

# To see this transformation across all three hypotheses, please return to the arcsine transformation section


