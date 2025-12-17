#Loading Packages
library(arules)
library(arulesViz)
library(dplyr)
library(DescTools)
library(dplyr)
library(ggplot2)
library(pROC)
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(ROCR)
library(car)

#Importing dataset
Telco.data <- read.csv("D:/Fall 2025/Business Analytics - CMIS 566/Project/TelcoCustomerChurn.csv")
View(Telco.data)

#Checking for missing values in each column
missing_values <- sapply(Telco.data, function(x) which(is.na (x)))
missing_values

# viewing distribution
summary(Telco.data$TotalCharges)
hist(Telco.data$TotalCharges, main = "Histogram of TotalCharges", xlab = "TotalCharges")

summary(Telco.data$MonthlyCharges)
hist(Telco.data$TotalCharges, main = "Histogram of MonthlyCharges", xlab = "MonthlyCharges")

summary(Telco.data$tenure)
hist(Telco.data$tenure, main = "Histogram of Tenure", xlab = "tenure")


# Extract column
x <- Telco.data$TotalCharges

# Calculate Q1, Q3 and IQR
Q1 <- quantile(x, 0.25, na.rm = TRUE)
Q3 <- quantile(x, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define bounds
lower.bound <- Q1 - 1.5 * IQR
upper.bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- x[x < lower.bound | x > upper.bound]
outliers

sum(x < lower.bound | x > upper.bound, na.rm = TRUE)

boxplot(Telco.data$TotalCharges,
        main = "Boxplot of TotalCharges (No Outliers Detected)",
        ylab = "TotalCharges",
        col = "lightblue",
        border = "black")

# Add text annotation
mtext("No outliers detected using IQR method", side = 3, line = 0.5)

# we got 0 outliers, so the data seemed to be refined which 
# made us think if the missing values are for the new customer. So having
# total charge NA makes sense, as they haven't been billed yet.

missing.check <- Telco.data[is.na(Telco.data$TotalCharges), c("customerID", "tenure", "MonthlyCharges")]
missing.check

# Here all the missing values are for new customers as tenure is 0.
# hence, the correct way to replace the missing values would be with 0 instead of mean or median.

#replace missing values with 0
Telco.data$TotalCharges[is.na(Telco.data$TotalCharges)] <- 0

#double check
sum(is.na(Telco.data$TotalCharges))
summary(Telco.data$TotalCharges)

#Checking for duplicate values
sum(duplicated(Telco.data$customerID))

#Check for duplicate row
duplicated(Telco.data)

# No duplicate data
sum(duplicated(Telco.data) == TRUE)

# Handling Noisy Data if any

# Check unique values for all columns
lapply(Telco.data, unique)

# There is no noisy data, while checking the unique value for all columns
# Double check
table(Telco.data$Contract)
table(Telco.data$PaymentMethod)
table(Telco.data$InternetService)

# checking for outliers in MonthlyCharges
num_col <- Telco.data$MonthlyCharges

Q1 <- quantile(num_col, 0.25, na.rm = TRUE)
Q3 <- quantile(num_col, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# View how many outliers exist (if any)
sum(num_col < lower_bound | num_col > upper_bound, na.rm = TRUE)

# Boxplot to visually confirm 0 outliers
boxplot(Telco.data$MonthlyCharges,
        main = "Boxplot of MonthlyCharges (No Outliers Detected)",
        ylab = "MonthlyCharges",
        col = "lightblue",
        border = "black")

# Add annotation
mtext("No outliers detected using IQR method", side = 3, line = 0.5)

# No outliers in MonthlyCharge Column


# listing each column with its data type and factor status
factor_summary <- data.frame(
  Column = names(Telco.data),
  Class = sapply(Telco.data, class),
  IsFactor = sapply(Telco.data, is.factor)
)

# View the summary
factor_summary


# Converting Categorical columns to factor for analysis
# These will often be used as categorical predictors
# Replacing Categorical values with numerical values for SeniorCitizen column
Telco.data$SeniorCitizen <- factor(Telco.data$SeniorCitizen,
                                   levels = c(0, 1),
                                   labels = c("No", "Yes"))

# Convert InternetService into a proper factor with defined levels
Telco.data$InternetService <- factor(Telco.data$InternetService,
                               levels = c("DSL", "Fiber optic", "No"))

# Convert Partner column (No = 0, Yes = 1)
Telco.data$Partner <- factor(Telco.data$Partner,
                       levels = c("No", "Yes"),
                       labels = c(0, 1))

# Convert Dependents column (No = 0, Yes = 1)
Telco.data$Dependents <- factor(Telco.data$Dependents,
                          levels = c("No", "Yes"),
                          labels = c(0, 1))


# Column Churn
Telco.data$Churn <- factor(Telco.data$Churn, levels = c("No", "Yes"), labels = c(0, 1))

# There are 3 categories on columns: OnlineSecurity, OnlineBackup, DeviceProtection,	
# TechSupport, StreamingTV,	StreamingMovies

# so we thought about combining No internet service and No into single category

# Churn rate comparison for OnlineSecurity
Telco.data %>%
  group_by(OnlineSecurity) %>%
  summarise(
    Total_Customers = n(),
    Churn_Count = sum(as.numeric(as.character(Churn))),   
    Churn_Rate = mean(as.numeric(as.character(Churn)))  
  )

# Churn rate comparison for OnlineBackup
Telco.data %>%
  group_by(OnlineBackup) %>%
  summarise(
    Total_Customers = n(),
    Churn_Count = sum(as.numeric(as.character(Churn))),   
    Churn_Rate = mean(as.numeric(as.character(Churn)))  
  )

# Churn rate comparison for DeviceProtection
Telco.data %>%
  group_by(DeviceProtection) %>%
  summarise(
    Total_Customers = n(),
    Churn_Count = sum(as.numeric(as.character(Churn))),   
    Churn_Rate = mean(as.numeric(as.character(Churn)))  
  )


# After analysis, we  came to conclusion that, we should NOT combine "No" 
# and "No internet service" into a single "No" category, 
# because their churn behavior is very different.

# so we are changing them to 3 level factors
cols_three_level <- c("OnlineSecurity", "OnlineBackup", "DeviceProtection",
                      "TechSupport", "StreamingTV", "StreamingMovies")

for (col in cols_three_level) {
  Telco.data[[col]] <- factor(Telco.data[[col]])
}

summary(Telco.data[cols_three_level])
str(Telco.data[cols_three_level])

# for contract
Telco.data$Contract <- factor(Telco.data$Contract,
                        levels = c("Month-to-month", "One year", "Two year"))

# for Paperless billing
Telco.data$PaperlessBilling <- factor(Telco.data$PaperlessBilling,
                                levels = c("No", "Yes"))
# for Payment Method
Telco.data$PaymentMethod <- factor(Telco.data$PaymentMethod,
                             levels = c("Electronic check",
                                        "Mailed check",
                                        "Bank transfer (automatic)",
                                        "Credit card (automatic)"))


# MultipleLines into factor
Telco.data$MultipleLines <- factor(Telco.data$MultipleLines,
                                   levels = c("No", "Yes", "No phone service"))

# we plan to remove columns PhoneService as it can be regenerated from MultipleLines
Telco.data$PhoneService <- NULL

# we also removed customerID as it is just an identifier
Telco.data$customerID <- NULL

# listing each column with its data type and factor status
factor_summary <- data.frame(
  Column = names(Telco.data),
  Class = sapply(Telco.data, class),
  IsFactor = sapply(Telco.data, is.factor)
)

# View the summary
factor_summary

# descriptive statistics

# Numeric features descriptive stats
summary(Telco.data[, c("tenure", "MonthlyCharges", "TotalCharges")])

# more details
Telco.data %>%
  summarise(
    tenure_mean = mean(tenure, na.rm = TRUE),
    tenure_sd   = sd(tenure, na.rm = TRUE),
    mc_mean     = mean(MonthlyCharges, na.rm = TRUE),
    mc_sd       = sd(MonthlyCharges, na.rm = TRUE),
    tc_mean     = mean(TotalCharges, na.rm = TRUE),
    tc_sd       = sd(TotalCharges, na.rm = TRUE)
  )

# Churn distribution overall
table(Telco.data$Churn)
prop.table(table(Telco.data$Churn))

# Descriptive churn grouped by key predictors (RQ1)
Telco.data %>%
  group_by(Contract) %>%
  summarise(Churn_Rate = mean(Churn == "1"))

Telco.data %>%
  group_by(InternetService) %>%
  summarise(Churn_Rate = mean(Churn == "1"))

Telco.data %>%
  group_by(PaymentMethod) %>%
  summarise(Churn_Rate = mean(Churn == "1"))

# Tenure grouped to support RQ2
Telco.data <- Telco.data %>%
  mutate(tenure_group = cut(tenure,
                            breaks = c(0, 12, 24, 60, Inf),
                            labels = c("0-12", "13-24", "25-60", "60+"),
                            right = TRUE))

# Churn rate comparison for OnlineSecurity
churn_summary_OS <- Telco.data %>%
  group_by(OnlineSecurity) %>%
  summarise(
    Total_Customers = n(),
    Churn_Count = sum(as.numeric(as.character(Churn))),   
    Churn_Rate = mean(as.numeric(as.character(Churn)))  
  )
churn_summary_OS
# Bar plot for Churn Rate with steelblue color
ggplot(churn_summary_OS, aes(x = OnlineSecurity, y = Churn_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Churn Rate by Online Security",
       x = "Online Security",
       y = "Churn Rate") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  

# Churn rate comparison for OnlineBackup
churn_summary_OB <- Telco.data %>%
  group_by(OnlineBackup) %>%
  summarise(
    Total_Customers = n(),
    Churn_Count = sum(as.numeric(as.character(Churn))),   
    Churn_Rate = mean(as.numeric(as.character(Churn)))  
  )
churn_summary_OB
# Bar plot for Churn Rate with steelblue color
  
ggplot(churn_summary_OB, aes(x = OnlineBackup, y = Churn_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Churn Rate by Online Backup",
         x = "Online Backup",
         y = "Churn Rate") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))


# Churn rate comparison for DeviceProtection
Telco.data %>%
  group_by(DeviceProtection) %>%
  summarise(
    Total_Customers = n(),
    Churn_Count = sum(as.numeric(as.character(Churn))),   
    Churn_Rate = mean(as.numeric(as.character(Churn)))  
  )

# Churn rate comparison for TechSupport
churn_summary_TS <- Telco.data %>%
  group_by(TechSupport) %>%
  summarise(
    Total_Customers = n(),
    Churn_Count = sum(as.numeric(as.character(Churn))),
    Churn_Rate = mean(as.numeric(as.character(Churn)))
  )

churn_summary_TS

ggplot(churn_summary_TS, aes(x = TechSupport, y = Churn_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Churn Rate by Tech Support",
       x = "Tech Support",
       y = "Churn Rate") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


# Churn rate comparison for StreamingTv
Telco.data %>%
  group_by(StreamingTV) %>%
  summarise(
    Total_Customers = n(),
    Churn_Count = sum(as.numeric(as.character(Churn))),   
    Churn_Rate = mean(as.numeric(as.character(Churn)))  
  )

# Churn rate comparison for StreamingMovies
Telco.data %>%
  group_by(StreamingMovies) %>%
  summarise(
    Total_Customers = n(),
    Churn_Count = sum(as.numeric(as.character(Churn))),   
    Churn_Rate = mean(as.numeric(as.character(Churn)))  
  )

# Here, based on numbers we are choosing OnlineSecurity and TechSupport
# for RQ2 because these two show high churn variation and strong business meaning


#RQ3
# Step 1: Create churn-risk categories based on tenure
# Logic:
# - Customers in their first year (≤12 months) are most likely to churn → High Risk
# - Customers between 13–24 months are moderately stable → Medium Risk
# - Customers staying longer than 24 months are loyal → Low Risk

Telco.data <- Telco.data %>%
  mutate(
    churn_risk = case_when(
      tenure <= 12 ~ "High Risk",      
      tenure > 12 & tenure <= 24 ~ "Medium Risk", 
      tenure > 24 ~ "Low Risk"       
    )
  )

# Step 2: Summarize churn rate by churn-risk group

Telco.data %>%
  group_by(churn_risk) %>%
  summarise(
    Total_Customers = n(),            
    Churn_Count = sum(Churn == "1"),      
    Churn_Rate = round(mean(Churn == "1"), 3)  
  )

# Step 3: Visualize churn distribution by risk level

ggplot(Telco.data, aes(x = churn_risk, fill = Churn)) +
  geom_bar() +
  labs(title = "Churn by Risk Category",
       x = "Churn Risk Group",
       y = "Number of Customers")


# PHASE 3 


# creating training and testing sets from Telco.data.
# Target variable is Churn (0 = No, 1 = Yes).

set.seed(123)  # for reproducibility

split.flag <- sample.split(Telco.data$Churn, SplitRatio = 0.7)

data.train <- subset(Telco.data, split.flag == TRUE)
data.test  <- subset(Telco.data, split.flag == FALSE)

nrow(data.train)  # number of training rows
nrow(data.test)   # number of testing rows


# Logistic Regression Model

model.logistic1 <- glm(
  Churn ~ tenure +
    Contract +
    InternetService +
    OnlineSecurity +
    TechSupport +
    MonthlyCharges +
    PaymentMethod +
    SeniorCitizen +
    Partner +
    Dependents +
    MultipleLines +
    PaperlessBilling,
  data = data.train,
  family = binomial(link = "logit")
)

# print model structure and detailed summary
print(model.logistic1)
print(summary(model.logistic1))

#vif_values <- vif(model.logistic1)
#vif_values

# while checking the Multicollinearity with vif, we got an issue of aliased coefficient
alias(model.logistic1)$Complete

# we got to know that it happened because:
# Customers who have No internet service automatically cannot have OnlineSecurity or TechSupport
# This creates perfect separation, making those dummy variables redundant.

# Although, in phase 2 we concluded we can not merge No internet service and No into single category 
# because of their different churn behaviour
# so we tried removing InternetService from the model but that also did not solve the aliasing issue
# so just for the modeling we need to merge these two categories into 
# single No Category
Telco.model <- Telco.data

# cleaning OnlineSecurity and TechSupport for modeling:
Telco.model$OnlineSecurity <- as.character(Telco.model$OnlineSecurity)
Telco.model$TechSupport    <- as.character(Telco.model$TechSupport)

# Any value that is exactly "Yes" stays "Yes".
# everything else (No, No internet service) is treated as "No" for modeling.
Telco.model$OnlineSecurity <- ifelse(Telco.model$OnlineSecurity == "Yes", "Yes", "No")
Telco.model$TechSupport    <- ifelse(Telco.model$TechSupport == "Yes", "Yes", "No")

# Convert back to clean 2-level factors
Telco.model$OnlineSecurity <- factor(Telco.model$OnlineSecurity, levels = c("No", "Yes"))
Telco.model$TechSupport    <- factor(Telco.model$TechSupport, levels = c("No", "Yes"))

# Double-check levels
levels(Telco.model$OnlineSecurity)
levels(Telco.model$TechSupport)

set.seed(123)
split.flag <- sample.split(Telco.model$Churn, SplitRatio = 0.7)

model.train <- subset(Telco.model, split.flag == TRUE)
model.test  <- subset(Telco.model, split.flag == FALSE)

# Now refitting the logistic model and checking Multicollinearity
model.logistic <- glm(
  Churn ~ tenure +
    Contract +
    InternetService +
    OnlineSecurity +
    TechSupport +
    MonthlyCharges +
    PaymentMethod +
    SeniorCitizen +
    Partner +
    Dependents +
    MultipleLines +
    PaperlessBilling,
  data = model.train,
  family = binomial(link = "logit")
)

# print model structure and detailed summary
print(model.logistic)
print(summary(model.logistic))

# checking multicollinearity
vif_values <- vif(model.logistic)
vif_values

alias(model.logistic)$Complete

# predictions on test data
# predicting churn probability (values between 0 and 1)
predicted.results <- predict(
  model.logistic,
  newdata = model.test,
  type = "response"
)

# creating a small data frame to compare predicted vs actual
new.data <- data_frame(
  predicted.results = predicted.results,
  # Convert factor "0"/"1" to numeric 0/1 for easier handling
  original = as.numeric(as.character(model.test$Churn))
)

View(new.data)

# Converting probabilities to classes (0 or 1)
# If predicted probability >= 0.5 --> predict churn (1), else no churn (0)

new.data <- new.data %>%
  mutate(
    roundedpred = ifelse(predicted.results >= 0.5, 1, 0)
  )

# Confusion Matrix

confusion.matrix <- confusionMatrix(
  as.factor(new.data$roundedpred),  # predicted class (0/1)
  as.factor(new.data$original),     # actual class (0/1)
  positive = "1"                    # treat "1" (churn) as positive class
)

print(confusion.matrix)

# ROC Curve and AUC (Model quality)

roc_input <- prediction(
  predicted.results,
  new.data$original 
)

performance.model <- performance(
  roc_input,
  measure = "tpr",   # true positive rate (sensitivity)
  x.measure = "fpr"  # false positive rate (1 - specificity)
)

# ploting ROC curve
plot(performance.model,
     main = "ROC Curve - Logistic Regression Churn Model")
abline(0, 1, lty = 2, col = "gray")  # diagonal reference line

# computing AUC 
area.under.curve <- performance(roc_input, measure = "auc")
auc.value <- area.under.curve@y.values[[1]]
auc.value  


# Decision Tree Model
# Using the same predictors as logistic regression
# Decision tree is immune to aliasing issue, hence we used the original data set
# with 3 categories --> Telco.data, line 419 and 420

model.tree <- rpart(
  Churn ~ tenure +
    Contract +
    InternetService +
    OnlineSecurity +
    TechSupport +
    MonthlyCharges +
    PaymentMethod +
    SeniorCitizen +
    Partner +
    Dependents +
    MultipleLines +
    PaperlessBilling,
  data = data.train,
  method = "class",
  control = rpart.control(cp= 0.01)
)

# viewing model structure
print(model.tree)
rpart.plot(model.tree, main = "Decision Tree for Customer Churn")


# predictions on test data
# predicting probabilistic output (probability of Churn = 1)
tree.prob <- predict(model.tree, newdata = data.test, type = "prob")[,2]

# converting to class labels using 0.5 cutoff
tree.pred.class <- ifelse(tree.prob > 0.5, 1, 0)

# preparing data frame
tree.eval.data <- data.frame(
  predicted = tree.pred.class,
  actual = as.numeric(as.character(data.test$Churn))
)


# Confusion Matrix (Decision Tree)

tree.confusion <- confusionMatrix(
  as.factor(tree.eval.data$predicted),
  as.factor(tree.eval.data$actual),
  positive = "1"
)

print(tree.confusion)

# ROC Curve & AUC (Decision Tree)

tree.prediction.obj <- prediction(tree.prob, tree.eval.data$actual)

tree.performance <- performance(tree.prediction.obj,
                                measure = "tpr",
                                x.measure = "fpr")

# ploting ROC curve
plot(tree.performance,
     main = "ROC Curve - Decision Tree Model",
     col = "blue")
abline(0, 1, lty = 2, col = "gray")

# computing AUC
tree.auc.obj <- performance(tree.prediction.obj, measure = "auc")
tree.auc <- tree.auc.obj@y.values[[1]]
tree.auc


# FINAL MODEL COMPARISON – LOGISTIC VS DECISION TREE

logit.acc <- confusion.matrix$overall["Accuracy"]
logit.sens <- confusion.matrix$byClass["Sensitivity"]
logit.spec <- confusion.matrix$byClass["Specificity"]
logit.auc <- auc.value

tree.acc <- tree.confusion$overall["Accuracy"]
tree.sens <- tree.confusion$byClass["Sensitivity"]
tree.spec <- tree.confusion$byClass["Specificity"]
tree.auc  <- tree.auc

cat("\nLogistic Regression:\n")
cat("  Accuracy   :", round(logit.acc, 3), "\n")
cat("  Sensitivity:", round(logit.sens, 3), "\n")
cat("  Specificity:", round(logit.spec, 3), "\n")
cat("  AUC        :", round(logit.auc, 3), "\n")

cat("\nDecision Tree:\n")
cat("  Accuracy   :", round(tree.acc, 3), "\n")
cat("  Sensitivity:", round(tree.sens, 3), "\n")
cat("  Specificity:", round(tree.spec, 3), "\n")
cat("  AUC        :", round(tree.auc, 3), "\n")


# FINAL CONCLUSION
if (logit.auc > tree.auc) {
  cat("\nConclusion:\n")
  cat("- Logistic Regression performs better overall (higher AUC).\n")
  cat("- It is more consistent and interpretable for identifying churn drivers.\n")
} else if (tree.auc > logit.auc) {
  cat("\nConclusion:\n")
  cat("- Decision Tree performs better (higher AUC).\n")
  cat("- It captures interactions and is easier to explain visually.\n")
} else {
  cat("\nConclusion:\n")
  cat("- Both models perform similarly.\n")
}



