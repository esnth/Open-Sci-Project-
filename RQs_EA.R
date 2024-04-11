#Load Data 
eamm_raw_df <- haven::read_sav("EAMMi2-CleanData.sav")

glimpse(eamm_raw_df)




#3 RQs to explore To Examine the Influence of Socio-Economic Context

#RQ1:How does socio-economic status influence the attainment of markers of adulthood?


# Create the overall variable
eamm_raw_df$Markers_of_Adulthood <- eamm_raw_df$MoAimportSUM + eamm_raw_df$MoAachieveSUM

# Perform multiple linear regression
model <- lm(Markers_of_Adulthood ~ income + edu, data = eamm_raw_df)

# Summarize the regression results
summary(model)

#The results suggest that while education level significantly influences the "Markers_of_Adulthood" variable, income level does not have a statistically significant effect. This implies that factors related to educational attainment may play a more crucial role in shaping markers of adulthood among emerging adults compared to income level. Therefore, in subsequent analyses, it may be beneficial to further explore the specific mechanisms through which education impacts markers of adulthood. Additionally, since the overall model had a low explanatory power, it indicates that other unexamined variables or non-linear relationships may also contribute to the variability in markers of adulthood. Hence, it would be essential to consider additional factors and potentially employ more sophisticated analytical techniques to better understand the complexities involved in this relationship.
#Overall, these findings imply that while higher education levels are associated with increased markers of adulthood among emerging adults, the relationship between income level and markers of adulthood may be more nuanced and potentially non-linear. Further investigation into the nature of this relationship, potentially employing non-linear modeling techniques, would be beneficial to fully understand the impact of socio-economic status on markers of adulthood


#RQ2: Are there differences in subjective well-being outcomes among emerging adults from varying socio-economic backgrounds?

# Load required libraries
library(stats)

# Perform regression analysis
model1 <- lm(SWBscale ~ income + edu, data = eamm_raw_df)

summary(model1)

#interpet: The regression analysis reveals that both income and education significantly impact subjective well-being among emerging adults. Higher income and education levels are associated with greater subjective well-being. Specifically, each unit increase in income is linked to a 0.08199 increase in subjective well-being, and each unit increase in education leads to a 0.03983 increase. These findings suggest that socio-economic factors play a crucial role in shaping subjective well-being during emerging adulthood.
The multiple R-squared value is 0.02214, indicating that approximately 2.21% of the variance in subjective well-being scores can be explained by the socio-economic factors included in the regression model.
This suggests that socio-economic factors, specifically income and education, account for a small but statistically significant portion of the variation in subjective well-being scores among emerging adults.This finding suggests that further analysis should be conducted to explore additional factors that may contribute to subjective well-being outcomes among emerging adults.

# Load the mediation package
library(mediation)


#RQ3: Do socio-economic factors predict differences in psychosocial dimensions during the transition to adulthood?

# Perform regression analysis for self-efficacy
model_self_efficacy <- lm(efficacySUM ~ income + edu, data = eamm_raw_df)
summary(model_self_efficacy)

#Overall, these results suggest that both income and education significantly predict differences in self-efficacy among emerging adults. However, the overall explanatory power of the model is relatively low, indicating that other factors not included in the analysis may also influence self-efficacy.


#3 RQs to Investigate the Role of Social Support Networks: 

#RQ1: What is the relationship between perceived social support and subjective well-being among emerging adults?

# Calculate correlation coefficients
correlation_matrix <- cor(eamm_raw_df[c("supportSUM", "belongSUM", "SWBscale", "mindfulSUM")])

# Calculate correlation coefficient between supportSUM and SWBscale

correlation <- cor(eamm_raw_df$supportSUM, eamm_raw_df$SWBscale, use = "complete.obs")

print(correlation)
#A correlation coefficient of 0.5 indicates a moderate positive relationship between the perceived social support (supportSUM) and subjective well-being (SWBscale) among emerging adults.

This suggests that as perceived social support increases, subjective well-being tends to increase as well, and vice versa, although the strength of the relationship is moderate.


#See if relaiotnhip covaries by sex 
eamm_raw_df$interaction <- eamm_raw_df$supportSUM * eamm_raw_df$sex


model_interaction <- lm(SWBscale ~ supportSUM + sex + interaction, data = eamm_raw_df)

summary(model_interaction)

#The regression analysis revealed that higher levels of perceived social support are associated with greater subjective well-being among emerging adults. While gender alone did not significantly predict subjective well-being, an interaction effect between perceived social support and gender was observed. This interaction suggests that the relationship between social support and well-being varies between genders, indicating a nuanced dynamic that influences subjective well-being outcomes.


library(ggplot2)

# Create the plot
ggplot(eamm_raw_df, aes(x = supportSUM, y = SWBscale)) +
  geom_point() +  # Add points
  facet_wrap(~ sex) +  # Facet by sex
  labs(x = "Perceived Support", y = "Subjective Well-being") +  # Labels
  theme_minimal()  # Optional: Customize theme

#LETS LOOK FURTHERI NTO THIS INTERACTION

# Check the frequency count of the 'sex' variable
table(eamm_raw_df$sex)

# Get a summary of the 'sex' variable
summary(eamm_raw_df$sex)


male_data <- eamm_raw_df[eamm_raw_df$sex == 1, ]
female_data <- eamm_raw_df[eamm_raw_df$sex == 2, ]

# For males
model_male <- lm(SWBscale ~ supportSUM, data = male_data)
summary(model_male)

# For females
model_female <- lm(SWBscale ~ supportSUM, data = female_data)
summary(model_female)

#Both models exhibited strong statistical significance (p < 0.001) and explained a substantial amount of variance in subjective well-being (males: R² = 0.270, females: R² = 0.202), indicating that perceived social support is a significant predictor of subjective well-being for both genders.
#The findings suggest that perceived social support significantly contributes to subjective well-being among both males and females in the emerging adult population. This underscores the importance of social support networks in promoting well-being during this transitional phase of life. Interventions aimed at enhancing social support systems could potentially improve subjective well-being outcomes for emerging adults, regardless of gender. Additionally, the results highlight the need for tailored support mechanisms that address the unique social and emotional needs of individuals transitioning into adulthood.


#RQ2: •	How does social support mediate the relationship between socio-economic context and markers of adulthood?

# Select the desired variables using subset function
selected_data_1 <- subset(eamm_raw_df, select = c(supportSUM, income, edu, MoAimportSUM, MoAachieveSUM))

# Alternatively, you can directly subset the dataframe
selected_data <- eamm_raw_df[, c("supportSUM", "income", "edu", "MoAachieveSUM", "MoAimportSUM")]


# Load the mice package
library(mice)

# Convert 'income' variable to numeric
selected_data$income <- as.numeric(selected_data$income)


# Convert 'edu' variable to numeric
selected_data$edu <- as.numeric(selected_data$edu)

# Perform imputation
imputed_data_11 <- mice(selected_data, method = "pmm", m = 100)


# Complete the imputation process
completed_data <- complete(imputed_data_11)


#MEDIATION MODEL SETUP 

outcome_model <- lm(MoAachieveSUM ~ income + edu, data = completed_data)

summary(outcome_model)
        
# Define the mediator model (social support)
mediator_model <- lm(supportSUM ~ income + edu, data = completed_data)
summary(mediator_model)

# Run the mediation analysis
# Run the mediation analysis
mediation_model <- mediate(outcome_model, mediator_model, data = completed_data)

# Run the mediation analysis
mediation_results <- summary(mediation_model)
        
        # View the results
        print(mediation_results)
        
        # Step 1: Load the lavaan package
        library(lavaan)
        
        # Step 2: Specify the Model
        model <- "
  # Direct effects
  MoAachieveSUM ~ b1*income + b2*edu
  supportSUM ~ c1*income + c2*edu
  MoAachieveSUM ~ d*supportSUM

  # Indirect effect (mediation)
  indirect := b1*c1 + b1*c2
"
        
        # Step 3: Fit the Model
        mediation_model <- sem(model, data = completed_data)
        
        # Step 4: Obtain Mediation Results
        summary(mediation_model)
        

        
        #diff model modified
lavaanPlot(mediation_model, coef = TRUE)

#problems doing mediation due to missing values? solution: IMPUTE!

install.packages("mice")
library(mice)

# Select only the relevant variables for imputation
impute_data <- eamm_raw_df[c("MoAimportSUM", "MoAachieveSUM", "supportSUM", "income", "edu")]


# Convert 'income' and 'edu' variables to numeric
impute_data$income <- as.numeric(impute_data$income)
impute_data$edu <- as.numeric(impute_data$edu)

# Check the data types again
sapply(impute_data, class) #good, all in numeric format
# Assuming your dataset is named 'data' and the race variable is named 'race'

# Using the table() function to count the frequencies of each category
race_counts <- table(eamm_raw_df$RaceCoded)

# Print the counts
print(race_counts)


# Perform multiple imputation
imputed_data <- mice(impute_data, m = 5, method = "pmm", seed = 123) # SUCCESSFUL
View(impute_data)
glimpse(impute_data)

# Check for missing values in the imputed data
sapply(impute_data, function(x) sum(is.na(x)))


hist(impute_data$income, main = "Histogram of income")

table(eamm_raw_df$income)


# Convert income variable to factor or character
impute_data$income <- as.factor(impute_data$income)

# Define regression model for outcome variable (Markers_of_Adulthood)
modelMED <- lm(MoAachieveSUM ~ supportSUM + income + edu, data = imputed_data)

# Define regression model for mediator variable (supportSUM)
mediatorMED <- lm(supportSUM ~ income + edu, data = imputed_data)

# Define regression model for outcome variable with mediator as predictor
outcomeMED <- lm(MoAachieveSUM ~ supportSUM + income + edu, data = imputed_data)


# Step 3: Perform mediation analysis
mediation_result <- mediate(modelMED, mediatorMED, outcomeMED, treat = "supportSUM", control = c("income", "edu"), boot = TRUE, sims = 1000)


## Step 3: Perform mediation analysis
mediation_result <- mediate(modelMED, mediatorMED, outcomeMED, treat = "supportSUM", boot = TRUE, sims = 1000, boot.ci.type = "bca")

# Step 4: View the results
summary(mediation_result)





# Check the data types of variables
sapply(impute_data, class) #supportSUM = numeric, income = dbl, edu = double


# Convert 'income' and 'edu' variables to numeric
impute_data$income <- as.numeric(impute_data$income)
impute_data$edu <- as.numeric(impute_data$edu)

# Perform multiple imputation
imputed_data <- mice(impute_data, m = 5, method = "pmm", seed = 123)

#NOW DO IT USING IMPUTED DATA

#first 

# Create the 'Markers_of_Adulthood' variable using imputed values
imputed_data$Markers_of_Adulthood <- rowMeans(imputed_data[c("MoAimportSUM", "MoAachieveSUM")], na.rm = TRUE)

# Now define your regression model using the newly created variable
modelMED <- lm(Markers_of_Adulthood ~ income + edu, data = imputed_data)

# Step 2: Define your regression models using the imputed dataset
modelMED <- with(imputed_data, lm(Markers_of_Adulthood ~ income + edu))
mediatorMED <- with(imputed_data, lm(supportSUM ~ income + edu))
outcomeMED <- with(imputed_data, lm(Markers_of_Adulthood ~ supportSUM + income + edu))

# Step 3: Perform mediation analysis
mediation_result <- mediate(modelMED, mediatorMED, outcomeMED, treat = "supportSUM", control = c("income", "edu"), boot.ci.type = "perc", sims = 1000)

# Step 4: View the results
summary(mediation_result)

#TRY NEW METHOD


#RQ3:•	Are there differences in the utilization of social support networks based on individual characteristics, such as gender and cultural background?


#exlpore data distirbution 

# Load the mice package
library(mice)

# Assuming your data frame is called eamm_raw_df
# Select only the relevant variables for imputation
impute_data_1 <- eamm_raw_df[c("sex", "supportSUM")]

# Convert sex variable to factor or character
impute_data_1$sex <- as.factor(impute_data_1$sex)  # or as.character(impute_data_1$sex)

# Perform multiple imputation
imputed_data <- mice(impute_data_1, m = 5, method = "pmm", seed = 123)


# Summary statistics
summary(impute_data$supportSUM)

glimpse(eamm_raw_df)



# Boxplot to visualize distribution by gender
ggplot(impute_data_1, aes(x = sex, y = supportSUM)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Social Support Variable") +
  ggtitle("Distribution of Social Support by Gender")

# Boxplot to visualize distribution by cultural background
ggplot(eamm_raw_df, aes(x = race, y = supportSUM)) +
  geom_boxplot() +
  labs(x = "Cultural Background", y = "Social Support Variable") +
  ggtitle("Distribution of Social Support by Cultural Background")

# T-test for comparing social support between genders
t_test_result <- t.test(supportSUM ~ sex, data = eamm_raw_df)
print(t_test_result)

# Assuming "sex" variable has values 1 and 2 where 1 represents male and 2 represents female

# Create a new variable "gender" indicating male or female
eamm_raw_df$sex <- ifelse(eamm_raw_df$sex == 1, "Male", "Female")

# Check the new variable
table(eamm_raw_df$sex)

# Filter the dataset to include only males
males <- subset(eamm_raw_df, sex == "Male")

# Filter the dataset to include only females
females <- subset(eamm_raw_df, sex == "Female")

# Perform t-test comparing social support between males and females
t_test_result <- t.test(supportSUM ~ sex, data = eamm_raw_df)

summary(t_test_result)

# Create a binary variable indicating male or female
eamm_raw_df$gender <- ifelse(eamm_raw_df$sex == 1, "Male", "Female")

# Perform t-test comparing social support between males and females
t_test_result <- t.test(supportSUM ~ sex, data = eamm_raw_df)

# Print the summary of the t-test result
summary(t_test_result)

t_test_result

# Print the t-test result
print(t_test_result)
# ANOVA for comparing social support among cultural backgrounds
anova_result <- aov(supportSUM ~ race, data = eamm_raw_df)
print(anova_result) #Overall, the ANOVA results indicate whether there are statistically significant differences in social support scores across different cultural backgrounds (race).

#results: look at notes 



# Data Exploration
# Summary statistics
summary(eamm_raw_df)

# Distribution of social support by gender
table(eamm_raw_df$sex)
summary(eamm_raw_df$supportSUM)

# Distribution of social support by cultural background
table(eamm_raw_df$race)

# Statistical Analysis
# Perform ANOVA
anova_result <- aov(supportSUM ~ as.factor(RaceCoded), data = eamm_raw_df)

# Print the ANOVA summary
print(summary(anova_result))

#The ANOVA summary indicates that there is a statistically significant difference in the means of supportSUM across the different levels of RaceCoded (p < 0.001). This suggests that there are differences in the utilization of social support networks based on cultural background.

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Print the results
print(tukey_result)


# Visualization
# Box plot of social support by gender
ggplot(eamm_raw_df, aes(x = sex, y = supportSUM)) +
  geom_boxplot() +
  labs(title = "Distribution of Social Support by Gender")


# Visualize significant differences from post-hoc test
plot(tukey_result)



#CORRELAITON ANALYISs

# Create a subset of the data frame with only the variables of interest
subset_df <- subset(eamm_raw_df, select = c("MoAachieveSUM", "MoAimportSUM"))

glimpse(subset_df)

# Perform multiple imputation
imputed_data_4correlation <- mice(subset_df, m = 5, method = "pmm", seed = 123)

correlation(imputed_data_4correlation)

correlation_matrix <- cor(imputed_data_4correlation)

str(imputed_data_4correlation)

# Extract imputed data for MoAachieveSUM and MoAimportSUM
imputed_MoAachieve <- complete(imputed_data_4correlation, "long", include = FALSE)$MoAachieveSUM
imputed_MoAimport <- complete(imputed_data_4correlation, "long", include = FALSE)$MoAimportSUM

# Combine imputed data into a dataframe
imputed_df <- data.frame(MoAachieveSUM = imputed_MoAachieve, MoAimportSUM = imputed_MoAimport)

# Check for NA values
anyNA(imputed_df)

# Compute correlation
correlation <- cor(imputed_df)
print(correlation)


# Assuming imputed_data_4correlation is your dataframe containing the variables MoAachieveSUM and MoAimportSUM
correlation <- cor(imputed_data_4correlation$MoAachieveSUM, imputed_data_4correlation$MoAimportSUM)

print(correlation)

#american dream exploration

# Rename variables
colnames(eamm_raw_df)[which(names(eamm_raw_df) == "usdream_1")] <- "Belief_Importance"
colnames(eamm_raw_df)[which(names(eamm_raw_df) == "usdream_2")] <- "Belief_Achievability"


# Check for missing values
summary(eamm_raw_df$Belief_Importance) #10 missing
summary(eamm_raw_df$Belief_Achievability) #10 missing 

# Summary statistics
summary(eamm_raw_df$Belief_Importance) #mwan 3.42
summary(eamm_raw_df$Belief_Achievability) #mean 3.59

# Visualize distributions (e.g., histograms)
hist(eamm_raw_df$Belief_Importance) #mode = 4
hist(eamm_raw_df$Belief_Achievability) # mode = 4

# Impute missing values with mean
eamm_raw_df$Belief_Importance[is.na(eamm_raw_df$Belief_Importance)] <- mean(eamm_raw_df$Belief_Importance, na.rm = TRUE)
eamm_raw_df$Belief_Achievability[is.na(eamm_raw_df$Belief_Achievability)] <- mean(eamm_raw_df$Belief_Achievability, na.rm = TRUE)


# Calculate correlation
correlation <- cor(eamm_raw_df$Belief_Importance, eamm_raw_df$Belief_Achievability)

# Print correlation coefficient
print(correlation)

#results: This positive correlation (0.619) suggests that there is a moderately strong positive relationship between the perceived importance of achieving the American Dream and the perceived achievability of achieving it. In other words, individuals who believe that achieving the American Dream is important also tend to believe that it is achievable, and vice versa.

# Scatter plot: Subjective Well-being vs. Belief Importance
plot(eamm_raw_df$Belief_Importance, eamm_raw_df$Subjective_Wellbeing,
     xlab = "Belief Importance", ylab = "Subjective Well-being",
     main = "Scatter Plot: Subjective Well-being vs. Belief Importance")

# Scatter plot: Subjective Well-being vs. Belief Achievability
plot(eamm_raw_df$Belief_Achievability, eamm_raw_df$Subjective_Wellbeing,
     xlab = "Belief Achievability", ylab = "Subjective Well-being",
     main = "Scatter Plot: Subjective Well-being vs. Belief Achievability")

#Ordinsal logistci regression

#get outcome in factor mode

# Convert SWBscale to a factor
eamm_raw_df$SWBscale <- factor(eamm_raw_df$SWBscale)

# Fit ordinal logistic regression model
mod_ordinal <- polr(SWBscale ~ Belief_Importance + Belief_Achievability, data = eamm_raw_df, Hess = TRUE)

# Summary of the model
summary(mod_ordinal)

# Load the MASS package
library(MASS)

# Fit ordinal logistic regression model
model <- polr(SWBscale ~ Belief_Importance + Belief_Achievability, data = eamm_raw_df, Hess = TRUE)

# Summary of the model
summary(model) #Coefficients:
  Belief_Importance: The coefficient estimate is -0.08224 with a standard error of 0.03338 and a t-value of -2.464.
Belief_Achievability: The coefficient estimate is 0.60910 with a standard error of 0.03919 and a t-value of 15.542.
These coefficients represent the log odds of moving from one level of the outcome variable to the next, for a one-unit increase in the predictor variable, holding other variables constant.
Overall, the results suggest that beliefs about the achievability of the American Dream have a stronger association with subjective well-being compared to beliefs about its importance. 
First, assess the correlations between beliefs about the American Dream (e.g., Belief_Importance, Belief_Achievability), self-efficacy, and subjective well-being (SWB).


# install.packages("psych")
library(psych)

# Select the variables of interest and convert them to numeric if needed
variables_of_interest <- eamm_raw_df[, c("Belief_Importance", "Belief_Achievability", "efficacySUM", "SWBscale")]

# Convert selected columns to numeric if they are not already
variables_of_interest <- sapply(variables_of_interest, as.numeric)

# Assess correlations
correlation_matrix <- cor(variables_of_interest, use="pairwise.complete.obs")

# Print correlation matrix
print(correlation_matrix) #* Overall, the correlations suggest that beliefs about the American Dream, self-efficacy, and subjective well-being are related constructs, with individuals who hold stronger beliefs in the American Dream and higher levels of self-efficacy also reporting higher levels of subjective well-being. However, the relationships are not extremely strong, suggesting that other factors may also play a role in influencing these constructs.



# after assessing correlations, do mediation

# Step 2: Conduct Mediation Analysis
library(mediation)

# Check data types
str(eamm_raw_df)

# Convert SWBscale to numeric
eamm_raw_df$SWBscale <- as.numeric(as.character(eamm_raw_df$SWBscale))

# Re-fit the mediation model
med_model <- lm(SWBscale ~ Belief_Importance + Belief_Achievability + efficacySUM, data = eamm_raw_df)

summary(med_model)
----------------------------------------------
  
  # Load the mediation package
  library(mediation)

# Load libraries
library(tidyverse)
library(psyntur)
library(lavaan)
install.packages("lavaanPlot")
library(lavaanPlot)

# Specify the mediation model and mediator model as strings
mediation_model <- "
  SWBscale ~ c_prime * Belief_Importance + b * Belief_Achievability
  Belief_Achievability ~ a * Belief_Importance
"

# Fit the mediation model using the sem() function
med_model <- sem(mediation_model, data = eamm_raw_df)

# Check the results
summary(med_model)

# Visualize the mediation paths using semPaths()
semPaths(med_model)

model33333 <- lm(belongSUM ~ stressSUM*efficacySUM*supportSUM*efficacySUM*mindfulSUM*efficacySUM*stressSUM, eamm_raw_df)
summary(model33333)




# Select the variables of interest
variables_of_interest <- eamm_raw_df[, c("Belief_Importance", "Belief_Achievability", "efficacySUM", "SWBscale")]

# Assess correlations
correlation_matrix <- cor(variables_of_interest)

# Print correlation matrix
print(correlation_matrix)





# Define your variables
socio_economic_context <- data$socio_economic_context_variable
perceived_social_support <- data$perceived_social_support_variable
beliefs_marriage_career <- data$beliefs_marriage_career_variable
developmental_outcomes <- data$developmental_outcomes_variable

# Conduct mediation analysis
med_results <- mediate(model.m = lm(developmental_outcomes ~ socio_economic_context),
                       model.z = lm(perceived_social_support ~ socio_economic_context + beliefs_marriage_career),
                       model.y = lm(developmental_outcomes ~ socio_economic_context + beliefs_marriage_career + perceived_social_support))

# Summarize results
summary(med_results)

