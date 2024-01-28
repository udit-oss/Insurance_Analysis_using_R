# Set working directory to the folder the files to be imported are stored
getwd()

# Import the .csv file to the global evnironment
insrn = read.csv("insurance.csv", header = TRUE, sep=',')
insrn

# 1. Checking the type of each variable and printing the first few observations
str(insrn)
head(insrn)

# 2. Check if there are any missing values in the dataset
missing_values = colSums(is.na(insrn))
print(missing_values[missing_values > 0]) # There are no missing values in the dataset

# 3. Fit a multiple linear regression
# First we convert the dataset headers to variables
age_variable <- insrn$age
gender_variable <- insrn$Gender
bmi_variable <- insrn$bmi
children_variable <- insrn$children
smoker_variable <- insrn$smoker
region_variable <- insrn$region
charges_variable <- insrn$charges
# Now we fit a multiple linear regression
model = lm(charges_variable ~ age_variable + gender_variable 
           + bmi_variable + children_variable + smoker_variable 
           + region_variable, data = insrn)

summary(model)

# 4. Check for multicollinearity and remove the variables 
#    with VIF(Variance Inflation Factor) >= 10
# First we install the car package to remove the variables with VIF >= 10
install.packages("car")
library(car)

# Check for multicollinearity using VIF
vif_values = car::vif(model)

# Identify variables with VIF >= 10
high_vif_variables = names(vif_values[vif_values >= 10])

print(high_vif_variables)

# Remove variables with VIF >= 10 from the model
model_no_collinearity = update(model, . ~ . - high_vif_variables)


# 5. List the significant variables, interpret the adjusted R square plot the residuals

# Significant Variables
summary(model_no_collinearity)

# Interpret Adjusted R-squared plots
summary(model_no_collinearity)$adj.r.squared

# Plot the residuals
residuals = resid(model_no_collinearity)
plot(model_no_collinearity$fitted.values, residuals, xlab = "Fitted Values",
     ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)

# 6. Check if there is any pattern or is it randomly distributed?

# Residuals vs Fitted Values Plot
plot(model_no_collinearity$fitted.values, residuals, xlab = "Fitted Values",
     ylab = "Residuals", main = "Residuals vs Fitted Values Plot")
abline(h = 0, col = "red", lty = 2)

# Histogram of residuals
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals") 

# Q-Q Plot to compare the  distribution of residuals to a theoretical normal distribution
qqnorm(residuals)
qqline(residuals)


