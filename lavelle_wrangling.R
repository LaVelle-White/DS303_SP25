####
# script to process Fed Small business data from 
#https://www.fedsmallbusiness.org/reports/survey/2024/2024-report-on-employer-firms
# Amber Camp, LaVelle White, April 2025
## My Objective: To use small business survey data to investigate how revenue levels predict different business success indicators, such as:
### 1.) Taking loans or grants
### 2.) Applying for Financing
### 3.) Getting funding requested
### 4.) Being profitable 
### 5.) Reporting strong financial condition
### 6.) Expecting employment growth
### 7.) A final combined model predicting business success

## My Approach: To use logistic Regression modeling to predict
## business success. From the Revenue sheet I several question and response
## along with sample size to predict business success, get the AIC and ANOVA.


# load packages
library(tidyverse)
library(janitor)
library(readxl)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

# Read Excel
revenue_data <- read_excel("C:/Users/12103/OneDrive - Chaminade University of Honolulu/Documents/DS303_SP25/sbcs-employer-firms-appendix-2024.xlsx")
excel_sheets("C:/Users/12103/OneDrive - Chaminade University of Honolulu/Documents/DS303_SP25/sbcs-employer-firms-appendix-2024.xlsx")
revenue_data <- read_excel("C:/Users/12103/OneDrive - Chaminade University of Honolulu/Documents/DS303_SP25/sbcs-employer-firms-appendix-2024.xlsx",
  sheet = "Revenue"
)




# Write as CSV
write_csv(revenue_data, "C:/Users/12103/Documents/revenue-tab.csv")


# Check working Directory
head(revenue_data)
getwd()
names(revenue_data)

# Read with no skipping to inspect the top of the file
#revenue_data <- read.csv("C:/Users/12103/OneDrive - Chaminade University of Honolulu/Documents/DS303_SP25/sbcs-employer-firms-appendix-2024.xlsx", col_names = FALSE)

# View the first 40 rows to find where real headers appear
print(head(revenue_data, 40))
revenue_data$percent


#  Clean and prep data
clean_names <- revenue_data %>% clean_names()

# Make percent numeric
#clean_names$percent <- parse_number(as.character(revenue_data$percent))
#clean_names$percent <- parse_number(as.character(revenue_data$percent)) / 100
clean_names <- revenue_data %>%
  mutate(percent = parse_number(as.character(percent)) / 100)


# Filter and mark funding success
## Actions taken during Financial challenges 
### Significant finding: Higher-revenue business were less likely to reply on loans or grants.($5M and above)
### AIC: 697.69, p < 2.2e-16 (The model statistically significant)
## This suggest small business are more dependent on external funding during hardship.

actions_success <- clean_names %>%
  filter(str_detect(survey_question, "(?i)actions taken in response to financial challenges")) %>%
  mutate(
    success = case_when(
      str_detect(response_option, "must be repaid") ~ 1,
      str_detect(response_option, "do not have to be repaid") ~ 1,
      TRUE ~ 0
    ),
    yes = round(percent * question_sample_size),
    no = question_sample_size - yes,
    revenue = factor(revenue)
  ) %>%
  group_by(revenue, success) %>%
  summarise(
    yes = sum(yes),
    no = sum(no),
    .groups = "drop"
  )

# Logistic regression model
model_actions_success <- glm(cbind(yes, no) ~ revenue, 
                             data = actions_success, 
                             family = binomial())

summary(model_actions_success)
# AIC
AIC(model_actions_success)

# ANOVA
anova(model_actions_success)


# Generate predicted probabilities by revenue group
actions_success$predicted <- predict(model_actions_success, type = "response")

# Plot predicted success by revenue group
ggplot(actions_success, aes(x = revenue, y = predicted)) +
  geom_col(fill = "#377eb8") +
  coord_flip() +
  labs(
    title = "Predicted Probability of Receiving Loan or Grant by Revenue",
    x = "Revenue Group",
    y = "Predicted Probability of Success"
  ) +
  theme_minimal()




# Filter and mark application success
## Applied for financing
### Strong Model: AIC 79.96, p < 0.001
### Significant Predictors: mid-revenue businesses ($25k-$500k) were less likely to apply
## Interpretation: Somewhat unexpected—could indicate a lack of access, awareness, or confidence in obtaining financing.

application_success <- clean_names %>%
  filter(str_detect(survey_question, "(?i)applications for financing")) %>%
  mutate(
    success = if_else(response_option == "Applied for a loan, line of credit, or merchant cash advance", 1, 0),
    yes = round(percent * question_sample_size),
    no = question_sample_size - yes,
    revenue = factor(revenue)
  ) %>%
  group_by(revenue, success) %>%
  summarise(
    yes = sum(yes),
    no = sum(no),
    .groups = "drop"
  )

# Logistic regression model
model_applications <- glm(cbind(yes, no) ~ revenue,
                          data = application_success,
                          family = binomial())

summary(model_applications)

# AIC
AIC(model_applications)

#ANOVA
anova(model_applications)

# Generate predicted probabilities
application_success$predicted <- predict(model_applications, type = "response")

# Plot
ggplot(application_success, aes(x = revenue, y = predicted)) +
  geom_col(fill = "#4daf4a") +
  coord_flip() +
  labs(
    title = "Predicted Probability of Applying for Financing by Revenue",
    x = "Revenue Group",
    y = "Predicted Probability"
  ) +
  theme_minimal()




# Filter and define success as receiving most/all of requested funding
## Received Most/All Funding
### Weak model: AIC: 695.44, not statistically significant
### Revenue did not significantly predict success in receiving requested funding.
## Suggests approval outcomes may depend more on factors like credit score or lender type than revenue.

outcome_success <- clean_names %>%
  filter(str_detect(survey_question, "(?i)best outcome across loan")) %>%
  mutate(
    success = case_when(
      response_option %in% c("Most (51-99%)", "All (100%)") ~ 1,
      TRUE ~ 0
    ),
    yes = round(percent * question_sample_size),
    no = question_sample_size - yes,
    revenue = factor(revenue)
  ) %>%
  group_by(revenue, success) %>%
  summarise(
    yes = sum(yes),
    no = sum(no),
    .groups = "drop"
  )

# Logistic regression model
model_outcome <- glm(cbind(yes, no) ~ revenue,
                     data = outcome_success,
                     family = binomial())

summary(model_outcome)

#AIC
AIC(model_outcome)

#ANOVA
anova(model_outcome)


# Generate predictions from the model
outcome_success$predicted <- predict(model_outcome, type = "response")

# Plot
ggplot(outcome_success, aes(x = revenue, y = predicted)) +
  geom_col(fill = "#984ea3") +
  coord_flip() +
  labs(
    title = "Predicted Probability of Receiving Most/All Requested Funding",
    x = "Revenue Group",
    y = "Predicted Probability"
  ) +
  theme_minimal()

# Filter and mark profitable businesses
## Profitable in 2023
### Very weak model: AIC: 2359, p-values all non-significant
### Revenue did not predict profitability
## Interpretation: Revenue ≠ profit — some high-revenue businesses were still not profitable.

profit_success <- clean_names %>%
  filter(str_detect(survey_question, "(?i)profitability, end of 2023")) %>%
  mutate(
    success = if_else(response_option == "Operating at a profit", 1, 0),
    yes = round(percent * question_sample_size),
    no = question_sample_size - yes,
    revenue = factor(revenue)
  ) %>%
  group_by(revenue, success) %>%
  summarise(
    yes = sum(yes),
    no = sum(no),
    .groups = "drop"
  )

# Logistic regression model
model_profit <- glm(cbind(yes, no) ~ revenue,
                    data = profit_success,
                    family = binomial())

summary(model_profit)

#AIC
AIC(model_profit)

#ANOVA
anova(model_profit)


# Generate predicted values
profit_success$predicted <- predict(model_profit, type = "response")

# Plot
ggplot(profit_success, aes(x = revenue, y = predicted)) +
  geom_col(fill = "#e41a1c") +
  coord_flip() +
  labs(
    title = "Predicted Probability of Being Profitable (End of 2023)",
    x = "Revenue Group",
    y = "Predicted Probability"
  ) +
  theme_minimal()



# Filter and mark positive financial condition
## Strong Financial Condition
### Again, not significant: AIC: 1914, flat estimates
## Interpretation: Subjective condition is not strongly tied to revenue group.

condition_success <- clean_names %>%
  filter(str_detect(survey_question, "(?i)current financial condition")) %>%
  mutate(
    success = case_when(
      response_option %in% c("Good", "Very good", "Excellent") ~ 1,
      TRUE ~ 0
    ),
    yes = round(percent * question_sample_size),
    no = question_sample_size - yes,
    revenue = factor(revenue)
  ) %>%
  group_by(revenue, success) %>%
  summarise(
    yes = sum(yes),
    no = sum(no),
    .groups = "drop"
  )

# Logistic regression model
model_condition <- glm(cbind(yes, no) ~ revenue,
                       data = condition_success,
                       family = binomial())

summary(model_condition)

#AIC
AIC(model_condition)

#ANOVA
anova(model_condition)


# Predict success probability
condition_success$predicted <- predict(model_condition, type = "response")

# Plot
ggplot(condition_success, aes(x = revenue, y = predicted)) +
  geom_col(fill = "#ff7f00") +
  coord_flip() +
  labs(
    title = "Predicted Probability of Positive Financial Condition",
    x = "Revenue Group",
    y = "Predicted Probability"
  ) +
  theme_minimal()



# Filter and mark employment growth expectations
# Predict
## Employment Growth Expectations
### Flat model: AIC: 440, no significant predictors
## Suggests that hiring plans are not driven by current revenue alone.

employment_success <- clean_names %>%
  filter(str_detect(survey_question, "(?i)employment expectations")) %>%
  mutate(
    success = if_else(response_option == "Will increase", 1, 0),
    yes = round(percent * question_sample_size),
    no = question_sample_size - yes,
    revenue = factor(revenue)
  ) %>%
  group_by(revenue, success) %>%
  summarise(
    yes = sum(yes),
    no = sum(no),
    .groups = "drop"
  )

# Logistic regression model
model_employment <- glm(cbind(yes, no) ~ revenue,
                        data = employment_success,
                        family = binomial())

summary(model_employment)

#AIC
AIC(model_employment)

#ANOVA
anova(model_employment)


employment_success$predicted <- predict(model_employment, type = "response")

# Plot
ggplot(employment_success, aes(x = revenue, y = predicted)) +
  geom_col(fill = "#a65628") +
  coord_flip() +
  labs(
    title = "Predicted Probability of Increasing Employment",
    x = "Revenue Group",
    y = "Predicted Probability"
  ) +
  theme_minimal()



# Combining Best Models
### Whether a business applied for funding
### Whether it received most/all of the requested funds
## Results
### Highly significant improvement over null: p < 2.2e-16
### Best AIC among all models: 474925.9 (lower is better)
## Businesses that both seek financing and successfully secure it are the most likely to exhibit success behaviors overall. This is a strong predictor of health and resilience.

clean_data <- clean_names %>%
  rename(
    question = survey_question,
    response = response_option,
    sample_size = question_sample_size
  )



business_data <- clean_data %>%
  mutate(
    applications = if_else(response == "Applied for a loan...", 1, 0),
    outcome = if_else(response %in% c("Most (51-99%)", "All (100%)"), 1, 0),
    success = applications + outcome,  # optional: create score
    yes = round(percent * sample_size),
    no = sample_size - yes
  )

model_null <- glm(cbind(yes, no) ~ 1, data = business_data, family = binomial())
model_applications <- glm(cbind(yes, no) ~ applications, data = business_data, family = binomial())
model_combined <- glm(cbind(yes, no) ~ applications + outcome, data = business_data, family = binomial())

anova(model_null, model_applications, model_combined, test = "Chisq")
AIC(model_null, model_applications, model_combined)

#Summary
## I analyzed survey data from small businesses to evaluate how revenue level predicts various indicators of business success.
## I built logistic regression models for six key indicators, including financing actions, profitability, and expected employment growth.
## While some models—like applying for financing or taking action in response to challenges—showed meaningful patterns, others like profitability or financial condition were not strongly predicted by revenue alone.
## The best-performing model combined two behaviors: applying for funding and receiving most/all of it, suggesting that access to capital and capital success are strong indicators of overall business health.
