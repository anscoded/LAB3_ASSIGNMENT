getwd()
churn <- read.csv("C:/Users/Alifah/OneDrive/Documents/DS SEM IV/Churn_Train.csv")

#check for missing values 
churn$Monthly_Charges
is.na(churn$Monthly_Charges)

#replace for missing values
churn <- churn %>%
  mutate(Total.Charges=replace(Total.Charges, is.na(Total.Charges), median(Total.Charges, na.rm=T)))
is.na(churn$Total.Charges)

#data preparation task
library(titanic)
summary(Titanic)
titanic_train$Age


#view age distribution using histogram
library(ggplot2)
library(dplyr)
library(cowplot)
ggplot(titanic_train, aes(Age)) +
  geom_histogram(color = "#000000", fill = "#C3B1E1") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Perform simple value imputation and view the data.
value_imputed <- data.frame(original = titanic_train$Age,
                            imputed_zero = replace(titanic_train$Age, is.na(titanic_train$Age), 0),
                            imputed_mean = replace(titanic_train$Age, is.na(titanic_train$Age), mean(titanic_train$Age, na.rm = TRUE)),
                            imputed_median = replace(titanic_train$Age, is.na(titanic_train$Age), median(titanic_train$Age, na.rm = TRUE)))
value_imputed

#Create histograms after imputation.
h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#FFDAB9", color = "#000000", position = "identity") + 
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#DAF7A6", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#B0C4DE", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#EEE8AA", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") + theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#Activity 2 – Impute Missing Values in R with MICE
#Install the mice package and test with md.pattern() function. Observe the visual representation of missing values.
library(mice)
titanic_numeric <- titanic_train %>%
  select(Survived, Pclass, SibSp, Parch, Age)
md.pattern(titanic_numeric)

#Perform MICE imputation methods
mice_imputed <- data.frame(original = titanic_train$Age,
                           imputed_pmm = complete(mice(titanic_numeric, method = "pmm"))$Age, #predictive mean matching
                           imputed_cart = complete(mice(titanic_numeric, method = "cart"))$Age, #Classification and regression trees
                           imputed_lasso = complete(mice(titanic_numeric, method = "lasso.norm"))$Age) #Lasso linear regression
mice_imputed

#Create histograms after imputation.
h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#FFDAB9", color = "#000000", position = "identity") + 
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#DAF7A6", color = "#000000", position = "identity") +
  ggtitle("Predictive Mean Matching distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#B0C4DE", color = "#000000", position = "identity") +
  ggtitle("Classification and Regression trees distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#EEE8AA", color = "#000000", position = "identity") +
  ggtitle("Lasso linear regression distribution") + theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

value_imputed

#Activity 3 – Imputation with R missForest Package
library(missForest)

missForest_imputed<-data.frame(
  original=titanic_numeric$Age,
  imputed_missForest=missForest(titanic_numeric)$ximp$Age)
missForest_imputed

#Create histograms after imputation.
h1 <- ggplot(missForest_imputed, aes(x = original)) +
  geom_histogram(fill = "#FFDAB9", color = "#000000", position = "identity") + 
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#DAF7A6", color = "#000000", position = "identity") +
  ggtitle("Estimated distribution") +
  theme_classic()
plot_grid(h1, h2, nrow = 2, ncol = 2)

#Activity 4 - Normalize data with scaling methods
#Normalize data using Log Transformation
titanic_train$Fare
titanic <- data.frame(Fare = c(7.25, 447, NA, 374, 831))
summary(titanic)
log_scale = log(as.data.frame(titanic$Fare))
print(log_scale)

#Normalize data using Min-Max Scaling
library(caret)
process <- preProcess(as.data.frame(titanic$Fare), method=c("range"))
norm_scale <- predict(process, as.data.frame(titanic$Fare))

#Normalize data using standard scaling in R
scale_data <- as.data.frame(scale(titanic$Fare))

#Activity 5 - Feature Encoding
#Label Encoding
gender_encode <- ifelse(titanic_train$Sex == "male",1,0)
table(gender_encode)

#One-Hot Encoding
new_dat = data.frame(titanic_train$Fare,titanic_train$Sex,titanic_train$Embarked)
summary(new_dat)

#Use the dummyVars() function to create a full set of dummy variables
library(caret)
dmy <- dummyVars(" ~ .", data = new_dat, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = new_dat))
glimpse(dat_transformed)

#Encoding Continuous (or Numeric) Variables
summary(new_dat$titanic_train.Fare)

#The first step is to create a vector of cut-off points based on 1st Quarter value and 3rd Quarter values
bins <- c(-Inf, 7.91, 31.00, Inf)

#The second step gives the respective names to these cut-off points.
bin_names <- c("Low", "Mid50", "High")

#The third step uses the cut() function to break the vector using the cut-off points. 
new_dat$new_Fare <- cut(new_dat$titanic_train.Fare, breaks = bins, labels = bin_names)

#Finally, we compare the original Fare variable with the binned new_Fare variable using the summary() function.
summary(new_dat$titanic_train.Fare)
summary(new_dat$new_Fare)

#Create a EDA report using eda_paged_report()for static report for object inherited from data.frame(tbl_df, tbl, etc) or data.frame.
churn %>%
  eda_paged_report(subtitle="churn",output_dir = "./", output_file = "EDA.pdf", theme = "blue")