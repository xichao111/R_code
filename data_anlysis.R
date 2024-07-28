#Baseline Data Analysis####
# Load necessary libraries
library(gtsummary)
library(tidyverse)
library(flextable)

# Read the data
data <- read.csv('shr_data.csv')

# Create summary table
t1 <- data %>% 
  tbl_summary(
    by = shr_t, # Group by 'shr_t'
    statistic = all_categorical() ~ "{n} ({p}%)", # Statistics for categorical variables
    digits = list(all_continuous() ~ 1, all_categorical() ~ 1) # Set digits for continuous and categorical variables
  ) %>% 
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% # Add p-values with 3 decimal places
  add_overall() %>% # Add overall column
  add_stat_label() # Add statistical labels

# Convert the table to a flextable and save as a .docx file
t1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = 'table_1.docx') # Save the table as 'table_1.docx'




#Cox Regression Analysis####
# Load necessary libraries
library(survival)
library(survminer)

# Cox Regression Analysis
mul_cox <- coxph(Surv(time, status) ~ age + gender + hypertension + heart_failure + myocardial_infarction + renal_disease + antiplatelet + anticoagulation + MBP + SPO2 + GCS + SOFA + hemoglobin + WBC + BUN + creatinine + INR + PT+shr_t, data = shr_data)
summary(mul_cox)





#Multi logstic anlysis####
# Load necessary libraries for logistic regression
library(MASS)

# Logistic Regression Analysis
mul_logistic <- glm(status ~ age + gender + hypertension + heart_failure + myocardial_infarction + renal_disease + antiplatelet + anticoagulation + MBP + SPO2 + GCS + SOFA + hemoglobin + WBC + BUN + creatinine + INR + PT+shr_t, data = shr_data, family = binomial)
summary(mul_logistic)

# Print the summary of the logistic regression model
summary(mul_logistic)




#Survival curve####
# Load necessary libraries
library(survival)
library(survminer)

# Read the data
pni <- read.csv("shr_data.csv")

# Create a survival analysis model
fit1 <- survfit(Surv(d90_time, d90_status) ~ shr_t, data = pni)

# Plot the survival analysis graph
g <- ggsurvplot(fit1, data = pni, pval = TRUE,
                conf.int = FALSE,
                title = " ",
                linetype = 1, risk.table = TRUE,
                legend.title = "SHR",
                legend.labs = c("T1", "T2","T3"),
                risk.table.col = 1,
                palette = c("green", "blue","red"),
                break.x.by = 30,
                xlim = c(0, 90),
                xlab = "Time (days)",
                ggtheme = theme_bw())

# Display the plot
print(g)



#RCS curve####
# Load necessary libraries
library(rms)   # For advanced statistical models
library(ggplot2)  # For data visualization

# Read data
shr_data <- read.csv("shr_data.csv")  # Load data from a CSV file into a DataFrame

# Setup data distribution, which is a requirement from the rms package for model fitting
data <- datadist(shr_data)
options(datadist='data')

# Fit a Cox Proportional Hazards model using the cph function from the rms package, modeling the 'shr' variable with restricted cubic splines
fit <- cph(Surv(d30_time, d30_status) ~ rcs(shr, 4), data=shr_data)

# Generate predicted data to calculate Hazard Ratios (HR) for each 'shr' value
HR <- Predict(fit, shr, fun=exp, ref.zero=TRUE);

# Update the data object to change the default reference value to 1.02 for 'shr'
data$limits$shr[2] <- 1.02

# Update the model to reflect the new reference value
fit <- update(fit)

# Use ggplot2 to plot the Hazard Ratio curve
ggplot() +
  geom_line(data=HR, aes(shr, yhat), linetype=1, linewidth=1, alpha = 0.9, colour="red") +  # Plot the HR curve
  geom_ribbon(data=HR, aes(shr, ymin = lower, ymax = upper), alpha = 0.3, fill="red") +   # Add confidence interval shading
  geom_hline(yintercept=1, linetype=2, linewidth=1) +  # Add a horizontal reference line (HR=1)
  geom_vline(xintercept=1.02, linetype=2, linewidth=1) +  # Add a vertical reference line (at the reference value)
  theme_classic() +  # Use classic theme for the plot
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5), breaks = seq(0, 8, 1)) +  # Set y-axis range and ticks
  labs(title = " ", x="SHR", y="HR(95% CI)")  # Set chart title and axis labels
