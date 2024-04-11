#-------------Preliminary exploration of EA data set-------------

#Load the data and select relevant variables of interest
eamm_raw_df <- haven::read_sav("EAMMi2-CleanData.sav")

# Load necessary libraries
library(haven)
library(ggplot2)
library(dplyr)
library(car)

# Selecting relevant variables
selectv <- eamm_raw_df %>%
  select(starts_with('moa'),
         starts_with('idea'),
         starts_with('swb'),
         starts_with('mindful'),
         starts_with('stress'),
         starts_with('physxSx'),
         starts_with('marriage'),
         starts_with('SocMedia'),
         starts_with('support'),
         starts_with('efficacy'),
         starts_with('belong'),
         age,
         edu,
         sex,
         school,
         income,
         sibling,
         race,
         armedservices)


#View basic structure of data
glimpse(selectv)

#------- Basic data integrity check --------------

# Checking for missing values
missing_values <- colSums(is.na(selectv))
print(missing_values)

# Summarizing the data for outliers
summary_stats <- summary(selectv)
print(summary_stats)


#Check variable types 
variable_types <- sapply(selectv, class)
print(variable_types)

# ------Calculate Overall Means and Standard Deviations to cross-validate with original researchers calculations---------

# Calculating overall means and SDs for SWB
overall_means_swb <- selectv %>%
  select(starts_with('swb')) %>%
  summarise_all(list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))
print(overall_means_swb)

# ----- Calculate Aggregate Means and Standard Deviations ---------

# Aggregate means across all participants
aggregate_means <- selectv %>%
  summarise(across(where(is.numeric), ~mean(., na.rm = TRUE)))
print(aggregate_means)

# ----- Mean Scores for Specific Scales --------
# Mean scores for Marriage scale
marriage_scale_means <- selectv %>%
  select(starts_with('marriage')) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))
print(marriage_scale_means)

# ------ Calculate Aggregate Means for Multiple Variables --------

# Aggregate means for multiple variables
aggregate_means <- selectv %>%
  summarise(
    moa_mean = mean(across(starts_with("moa"), na.rm = TRUE)),
    idea_mean = mean(across(starts_with("idea"), na.rm = TRUE)),
    swb_mean = mean(across(starts_with("swb"), na.rm = TRUE)),
    mindful_mean = mean(across(starts_with("mindful"), na.rm = TRUE)),
    stress_mean = mean(across(starts_with("stress"), na.rm = TRUE)),
    marriage_mean = mean(across(starts_with("marriage"), na.rm = TRUE)),
    socmediause_mean = mean(across(starts_with("socmedia"), na.rm = TRUE)),
    support_mean = mean(across(starts_with("support"), na.rm = TRUE)),
    efficacy_mean = mean(across(starts_with("efficacy"), na.rm = TRUE)),
    belong_mean = mean(across(starts_with("belong"), na.rm = TRUE))
  )
print(aggregate_means)


# Calculate mean importance and achievement scores
mean_importance <- mean(c(selectv$MOA_RT_IMP, selectv$MOA_NC_IMP, selectv$MOA_I_IMP, selectv$MOA_RM_IMP), na.rm = TRUE)
mean_achievement <- mean(c(selectv$MOA_RT_ACH, selectv$MOA_NC_ACH, selectv$MOA_I_ACH, selectv$MOA_RM_ACH), na.rm = TRUE)


# ----- Demographic explorations ---------

# Descriptive statistics for continuous variables
continuous_vars <- c("age", "income", "USresidencyyears")
summary(eamm_raw_df[, continuous_vars])

# Frequencies for categorical variables
categorical_vars <- c("sex", "armedservices", "USresidency")
for (var in categorical_vars) {
  cat(paste("Frequencies for", var, ":\n"))
  print(table(eamm_raw_df[, var]))
}

# Step 3: Gender Differences in Well-Being

# Box plot comparing SWB distribution by gender
library(ggplot2)
ggplot(eamm_raw_df, aes(x = as.factor(sex), y = SWBscale)) +
  geom_boxplot() +
  labs(x = "Gender", y = "SWBscale") +
  ggtitle("Distribution of SWBscale by Gender")

# Step 4: Relationship Between Age and Well-Being

# Pearson correlation
cor_pearson <- cor(eamm_raw_df$age, eamm_raw_df$SWBscale, method = "pearson", use = "complete.obs")

# Spearman correlation
cor_spearman <- cor(eamm_raw_df$age, eamm_raw_df$SWBscale, method = "spearman", use = "complete.obs")

# Print correlation coefficients
print(paste("Pearson correlation:", cor_pearson))
print(paste("Spearman correlation:", cor_spearman))



#------- Linear Regression Analysis with demographics for SWB ----------

# Linear regression model to explore relationship between age (predictor) and SWB (outcome)
regression_model <- lm(SWBscale ~ age, data = eamm_raw_df)
summary(regression_model)

# --------- Analysis of Variance (ANOVA) to explore differences in neab SWB between diff categories of Education level attained  

# ANOVA for comparing means across different education levels
anova_result <- aov(SWBscale ~ factor(EducationCoded), data = eamm_raw_df)
print(summary(anova_result))

# -------- Multiple Linear Regression (MLR) to explore the combined influence of various variables on SWB ---------

# MLR model
model_MLR_Ext <- lm(SWBscale ~ BelongingScale + MindfulnessScale + EfficacyScale + MoAimportSUM + MoAachieveSUM + IDEASUM + supportSUM + NPI_sum + SocMediaSum + physsymSUM + stressSUM, data = eamm_raw_df)
summary(model_MLR_Ext)

# -------- Exploring Relationships between Variables -------- 

###########################
# GENDER DIFFERENCES      #
###########################

# Box plot comparing SWB distribution by gender
ggplot(eamm_raw_df, aes(x = as.factor(sex), y = SWBscale)) +
  geom_boxplot() +
  labs(x = "Gender", y = "SWBscale") +
  ggtitle("Distribution of SWBscale by Gender")

###########################
# AGE AND WELL-BEING      #
###########################

# Pearson correlation

# Check the data types of 'age' and 'SWBscale'
str(eamm_raw_df$age)
str(eamm_raw_df$SWBscale)

# Convert 'age' and 'SWBscale' to numeric if they are not already
eamm_raw_df$age <- as.numeric(eamm_raw_df$age)
eamm_raw_df$SWBscale <- as.numeric(eamm_raw_df$SWBscale)

# Check for missing values in 'age' and 'SWBscale'
sum(is.na(eamm_raw_df$age))
sum(is.na(eamm_raw_df$SWBscale))

# There are 1037 missing values for age and 2 for SWBscale, so multiple imputation using MICE package
install.packages("mice")
library(mice)

# Create a mice imputation model
imputation_model <- mice(eamm_raw_df[, c("age", "SWBscale")], method = "pmm")

# Perform the imputation
imputed_data <- complete(imputation_model)

# Extract the imputed dataset
imputed_age <- imputed_data$age
imputed_SWBscale <- imputed_data$SWBscale


# Calculate Pearson correlation
cor_pearson <- cor(imputed_data$age, imputed_data$SWBscale, method = "pearson")

# Calculate Spearman correlation
cor_spearman <- cor(imputed_data$age, imputed_data$SWBscale, method = "spearman")

# Print the correlation coefficients
print(paste("Pearson correlation:", cor_pearson))
print(paste("Spearman correlation:", cor_spearman))

# Linear regression
regression_model <- lm(SWBscale ~ age, data = imputed_data)
summary(regression_model)

# Scatter plot
plot(imputed_data$age, imputed_data$SWBscale, xlab = "Age", ylab = "Well-Being", main = "Scatter Plot of Age and Well-Being")
#Greater density of data observations around 20-30 years of age due to most of the data (N = 3,134) are below 25)


# Subsequent Analyses Implications:

# 1. Non-Linear Relationships: Given the lack of a clear linear trend in the scatter plot and the dense concentration of data observations within the 20-30 age range, it suggests that the relationship between age and well-being may be non-linear. This prompts exploration of non-linear modelling techniques, to capture relationships in the data more accurately.

# 2. Additional Predictor Variables: Since age alone may not fully explain the variability in well-being, consider including additional predictor variables in the regression model. Factors such as socio-economic status, and psychological traits could contribute to a better understanding of well-being outcomes within the target population.

# 3. Subgroup Analyses: Given the heterogeneity of responses within the 20-30 age range, conduct subgroup analyses to explore whether specific subgroups within this range exhibit distinct patterns of well-being. For example, you could examine differences between different educational levels, employment statuses, or cultural backgrounds to identify potential moderators of the age-well-being relationship.

# 4. Data Quality Checks: Before proceeding with further analyses, conduct thorough data quality checks to ensure the reliability and validity of the dataset. Check for outliers, missing values, and potential biases that could affect the interpretation of results.

# 5. Interpretation Caution: Given the complexity of the relationship between age and well-being observed in the scatter plot, interpret the results of subsequent analyses with caution. Consider the limitations of the data and the potential for confounding variables that may influence the observed relationships.

# 6. Sensitivity Analysis: Perform sensitivity analyses to assess the robustness of the results to different modeling assumptions and parameter specifications. This will help ensure the stability and generalizability of the findings across different analytical approaches.

# 7. Conclusion: In conclusion, the dense concentration of data observations within the 20-30 age range and the absence of a clear linear trend in the scatter plot indicate the need for a nuanced and exploratory approach to understanding the relationship between age and well-being. By incorporating alternative modeling techniques, exploring additional predictor variables, and conducting subgroup analyses, we can gain deeper insights into the factors influencing well-being outcomes in our target population.

#####################
# Subgroup Analyses:
######################
# 1. Educational Levels:
# Subset the data for individuals within the 20-30 age range
subset_age_range <- eamm_raw_df[eamm_raw_df$age >= 20 & eamm_raw_df$age <= 30, ]

# Conduct subgroup analysis by educational levels
educational_levels <- unique(subset_age_range$EducationCoded)
for (level in educational_levels) {
  subgroup_data <- subset_age_range[subset_age_range$EducationCoded == level, ]
  # Perform analysis (e.g., mean SWB by educational level)
  mean_swb <- mean(subgroup_data$SWBscale, na.rm = TRUE)
  print(paste("Mean SWB for Educational Level", level, ":", mean_swb))
}


# 2. Cultural Backgrounds:
# Conduct subgroup analysis by cultural backgrounds
cultural_backgrounds <- unique(subset_age_range$RaceCoded)  # Replace with actual column name
for (background in cultural_backgrounds) {
  subgroup_data <- subset_age_range[subset_age_range$RaceCoded == background, ]  # Replace with actual column name
  # Perform analysis (e.g., mean SWB by cultural background)
  mean_swb <- mean(subgroup_data$SWBscale, na.rm = TRUE)
  print(paste("Mean SWB for Cultural Background", background, ":", mean_swb))
}

#EXCLUDE MISSING VALUES 
# Conduct subgroup analysis by cultural backgrounds after excluding missing values
subset_age_range <- subset_age_range[!is.na(subset_age_range$RaceCoded), ]  # Exclude rows with NA values
cultural_backgrounds <- unique(subset_age_range$RaceCoded)
for (background in cultural_backgrounds) {
  subgroup_data <- subset_age_range[subset_age_range$RaceCoded == background, ]
  # Perform analysis (e.g., mean SWB by cultural background)
  mean_swb <- mean(subgroup_data$SWBscale, na.rm = TRUE)
  print(paste("Mean SWB for Cultural Background", background, ":", mean_swb))
}

###### are the mean SWB scores per diff category of race sig diff ? ANOVA

# First, ensure 'RaceCoded' is treated as a factor variable
subset_age_range$RaceCoded <- factor(subset_age_range$RaceCoded)

# Perform ANOVA
anova_result <- aov(SWBscale ~ RaceCoded, data = subset_age_range)

# Print ANOVA table
print(summary(anova_result))

#The ANOVA test indicates a marginal relationship between cultural background and subjective well-being, with a p-value of 0.069 suggesting weak evidence to reject the null hypothesis.



# Data Quality Checks:

# 1. Outliers:
# Identify and handle outliers using box plots, scatter plots, or statistical methods such as z-scores or IQR.
# For example, you can use the boxplot function to visualize the distribution of SWBscale and age.
boxplot(eamm_raw_df$SWBscale, main = "Boxplot of SWBscale")
boxplot(eamm_raw_df$age, main = "Boxplot of Age") #lots of outliers... 

# 2. Missing Values:
# Check for missing values in relevant variables (e.g., SWBscale and age)
missing_values <- sum(is.na(eamm_raw_df$SWBscale) | is.na(eamm_raw_df$age))
print(paste("Number of Missing Values:", missing_values))


###########################
# EDUCATION AND WELL-BEING#
###########################

# ANOVA
anova_result <- aov(SWBscale ~ factor(EducationCoded), data = eamm_raw_df)
print(summary(anova_result))

#Results indicate a statistically significant relationship between educational level and subjective well-being, as evidenced by a p-value of 0.000519, suggesting strong evidence to reject the null hypothesis.
#Run posthocs

#-----Post hoc for significance test

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Print the results
print(tukey_result)

### Interpeation of ANOVA: The potential nuance lies in the interpretation of the Tukey's HSD results, suggesting that while there are significant differences in mean well-being scores across different educational levels, the magnitude of these differences may vary. Additionally, the nonsignificant differences observed between certain educational levels may indicate similarity in well-being scores among those groups, warranting further investigation into potential factors contributing to this similarity.


#Plot anova results using ggplot2
# Create boxplot
ggplot(eamm_raw_df, aes(x = factor(EducationCoded), y = SWBscale)) +
  geom_boxplot() +
  xlab("Educational Level") +
  ylab("Subjective Well-Being") +
  ggtitle("Boxplot of Subjective Well-Being by Educational Level")


###########################
# SOCIAL SUPPORT & WELL-BEING #
###########################

# Linear regression
model_lin1 <- lm(SWBscale ~ supportSUM, data = eamm_raw_df)
summary(model_lin1)

#The linear regression shows a significant positive relationship between well-being (SWBscale) and support level (supportSUM) (β = 0.27, p < 0.001), indicating that higher levels of support are associated with higher well-being scores.

###########################
# STRESS AND WELL-BEING   #
###########################

# Scatter plot
ggplot(eamm_raw_df, aes(x = stressSUM, y = SWBscale)) +
  geom_point() +
  labs(title = "Relationship Between Stress and SWB", x = "Stress Score", y = "SWB Score")


#interpretation: The scatterplot shows a wide variability in Subjective Well-Being (SWB) scores across a moderate range of stress scores with no obvious linear relationship.

#Pearsons correlation between stress and SWB, after imputation

library(dplyr)

# Impute missing values with the mean of the column
eamm_raw_df <- eamm_raw_df %>%
  mutate(stressSUM = ifelse(is.na(stressSUM), mean(stressSUM, na.rm = TRUE), stressSUM),
         SWBscale = ifelse(is.na(SWBscale), mean(SWBscale, na.rm = TRUE), SWBscale))

# Calculate the Pearson correlation coefficient after imputation
correlation <- cor(eamm_raw_df$stressSUM, eamm_raw_df$SWBscale, method = "pearson")

# Print the result
print(correlation)

#A very weak negative relationship between stressSUM and SWBscale (non linear relationships, high variability)

###########################
# OTHER GENDER ANALYSIS   #
###########################

# Subset for 'other' gender
other_gender_df <- eamm_raw_df[eamm_raw_df$sex == 3, ]

# Correlation analysis
correlation_matrix <- cor(other_gender_df[c("belongSUM")])
cor_test_result <- cor.test(other_gender_df$belongSUM, other_gender_df$SWBscale)

# T-test
belong_t_test <- t.test(other_gender_df$belongSUM)
stress_t_test <- t.test(other_gender_df$stressSUM)

# Density plot
ggplot(other_gender_df, aes(x = belongSUM)) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Density Plot of Belonging for 'Other' Gender Group",
       x = "Belonging Score", y = "Density")

#This kind of (roughly normal, most common scores higher) distribution could indicate that the majority of individuals in the 'Other' gender group feel a moderate to high sense of belonging, with fewer individuals feeling a low sense of belonging.


###########################
# SEM (STRUCTURAL EQUATION MODELING) #
###########################

# Define the SEM model
modelSEM <- '
  # Define the latent variables and their indicators
  SWB =~ SWBscale
  EA =~ MoAachieveSUM
  MF =~ mindfulSUM

  # Define the regression paths
  EA ~ SWB
  EA ~ MF

  # Define exogenous variables
  MF ~ age + sex

  # Define residual variances
  SWB ~~ SWB
  EA ~~ EA
  MF ~~ MF
'

# Fit the model to the data
fit <- sem(modelSEM, data = eamm_raw_df)

# Summarize the results
summary(fit)

# Assuming 'fit' is your SEM model object from lavaan

# Extract AIC and BIC values
aic_value <- fitMeasures(fit, "aic")
bic_value <- fitMeasures(fit, "bic")

# Print the AIC and BIC values
print(paste("AIC:", aic_value))
print(paste("BIC:", bic_value))

#Poor fit... aic/bic 45,000+

# Various analyses and visualizations exploring relationships between variables

# Multinomial Logistic Regression Model with Interaction
model2 <- multinom(EfficacyScale ~ RaceCoded * Stress, data = eamm_raw_df)
summary(model2)
coefficients2 <- coef(model2)
odds_ratios2 <- exp(coefficients2)
print(round(odds_ratios2, 3))

# Visualize interaction effect of RaceCoded and Stress
interaction_plot(x.factor = dfEA$RaceCoded, trace.factor = dfEA$Stress, response = dfEA$EfficacyScale)

# Ordinal Logistic Regression Model

#Check the class of SWBscale
class(eamm_raw_df$SWBscale)

# Convert SWBscale to a factor if it's not already
eamm_raw_df$SWBscale <- factor(eamm_raw_df$SWBscale)
model <- polr(SWBscale ~ IDEA8_exp + IDEA8_identity + IDEA8_negativity + IDEA8_between, data = eamm_raw_df, Hess=TRUE)
summary(model)

# Visualize relationship between IDEA8_exp and SWBscale
ggplot(eamm_raw_df, aes(x = SWBscale, y = IDEA8_exp)) +
  geom_boxplot()


