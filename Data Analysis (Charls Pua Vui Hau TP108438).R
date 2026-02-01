library(tidyverse)
library(ggplot2)
library(forcats)
library(VIM)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)

setwd("") #here you put your file directory eg /yourName/name/download/datasets....
getwd()

df <- read.csv("cleaned_data.csv", header = TRUE, sep = ",")
df

#quick overview 
head(df)
tail(df)
str(df)
dim(df)
summary(df)
colnames(df)
view(df)
sum(is.na(df)) #Should return 0
any(duplicated(df)) #Should return FALSE

#to investigate the distribution of protocol types and source bytes through descriptive analysis.
#univariate analysis for protocol and SrcBytes (what)
summary(df$Protocol)
top5_protocols <- names(sort(table(df$Protocol), decreasing = TRUE)[1:5])
top5_protocols

#create a new variable with top 5 + "Other" (feature engineer)
df$Protocol_Top5 <- ifelse(df$Protocol %in% top5_protocols, 
                           as.character(df$Protocol), 
                           "Other")
df$Label <- factor(df$Label)

df$Protocol_Top6 <- factor(df$Protocol_Top5)
#Frequency table
table(df$Protocol_Top5)
#Bar chart for Top 6 protocols
ggplot(df, aes(x = Protocol_Top6, fill = Protocol_Top6)) +
  geom_bar() +
  labs(title = "Distribution of Top 6 Protocols",
       x = "Protocol", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#summary statistic for srcbytes
summary(df$SrcBytes_log)
table(df$SrcBytes_log)
#make Top 10 of the dataset srbytes instead of output every all of them
top_data <- head(sort(table(df$SrcBytes_log), decreasing = TRUE), 10)
top_df <- data.frame(SrcBytes_log = names(top_data), Frequency = as.numeric(top_data))

ggplot(top_df, aes(x = reorder(SrcBytes_log, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.9) + 
  labs(title = "Top 10 most common source bytes values", x = "Source Bytes", y = "Frequency") + 
  theme_minimal() + theme(axis.text = element_text(angle = 45, hjust = 0.8))

#to examine how the network protocol and the size of data sent from the source influence the likelihood 
#of a connection being an attack label = 1 or normal label = 0 using logistic regression (Diagnostic analysis, why)
set.seed(123)
train_index <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_index, ]
testing_data <- df[-train_index, ]

#identify top protocols (using training data only to avoid data leakage)
top5_protocols <- names(sort(table(train_data$Protocol), decreasing = TRUE)[1:5])

#create simplified protocol variable
train_data$Protocol_Top5 <- ifelse(train_data$Protocol %in% top5_protocols, 
                                   as.character(train_data$Protocol), "Other")
testing_data$Protocol_Top5 <- ifelse(testing_data$Protocol %in% top5_protocols, 
                                     as.character(testing_data$Protocol), "Other")

#convert to factor with consistent levels
common_levels <- c(top5_protocols, "Other")
train_data$Protocol_Top5 <- factor(train_data$Protocol_Top5, levels = common_levels)
testing_data$Protocol_Top5 <- factor(testing_data$Protocol_Top5, levels = common_levels)

#use the simplified protocol variable in the model
models <- glm(Label ~ Protocol_Top5 + SrcBytes_log, data = train_data, family = binomial)
summary(models)

#predictions
prediction <- predict(models, testing_data, type = "response")
predicting_class <- ifelse(prediction > 0.5, 1, 0)

#evaluation
confusion_matrix <- table(Predicted = predicting_class, Actual = testing_data$Label)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 3)))
#Additional metrics
precision <- confusion_matrix[2,2] / sum(confusion_matrix[2,])
recall <- confusion_matrix[2,2] / sum(confusion_matrix[,2])
f1 <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", round(precision, 3)))
print(paste("Recall:", round(recall, 3)))
print(paste("F1-Score:", round(f1, 3)))

#visualization
train_data$predicted_prob <- predict(models, train_data, type = "response")

ggplot(train_data, aes(x = SrcBytes_log, y = predicted_prob, color = Protocol_Top5)) +
  geom_point(alpha = 0.7, size = 1) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Predicted Probability of Attack by Protocol",
       x = "Source Bytes", y = "Predicted Probability") +
  theme_minimal()

#examine associations between features and attack classfication (how)
#assess whether network protocol type is associated with connection Label
protocol_class_top5 <- table(df$Protocol_Top5, df$Label)
chi_output_top5 <- chisq.test(protocol_class_top5)
print(chi_output_top5)

#second one is the ANOVA to determine whether mean source bytes differ between normal and attack connection
anova <- aov(SrcBytes_log ~ Label, data = df)
summary(anova)

#Boxplot to visualize the distribution
ggplot(df, aes(x = Label, y = SrcBytes_log, fill = Label)) + geom_boxplot() + 
  labs(title = "Distribution of Source Bytes by connection type",
       x = "Connection type 0 is normal 1 is attack", y = "Source Bytes") +
  theme_minimal()

#To determine whether the protocol and srcbytes variables can accurately predict the label normal or attack 
#using a decision tree classfier (predictive)

#ensure Label is a factor for classification
df$Label <- as.factor(df$Label)

#build the Decision Tree model
dt_model <- rpart(Label ~ Protocol_Top6 + SrcBytes_log, 
                  data = train_data, 
                  method = "class", 
                  control = rpart.control(cp = 0.01))  #complexity parameter to avoid overfitting

#display summary of the model
print(dt_model)
summary(dt_model)

#visualize the Decision Tree
rpart.plot(dt_model, type = 2, extra = 103, fallen.leaves = TRUE, tweak = 1.1,
           main = "Decision Tree Structure")

#make predictions on test data
dt_predictions <- predict(dt_model, testing_data, type = "class")

#evaluate model performance with a confusion matrix
conf_matrix_dt <- confusionMatrix(dt_predictions, testing_data$Label)
print(conf_matrix_dt)

#extract overall accuracy
accuracy_dt <- conf_matrix_dt$overall['Accuracy']
cat("Decision Tree Accuracy:", round(accuracy_dt * 100, 2), "%\n")

#variable importance plot
var_imp <- dt_model$variable.importance
var_importance_df <- data.frame(Variable = names(var_imp),
                                Importance = as.numeric(var_imp))

ggplot(var_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) + 
  geom_col(fill = "green", alpha = 0.8) + 
  labs(title = "Variable importance in decision tree visualization",
       x = "Variable",
       y = "Importance Score") + 
  theme_minimal() +
  geom_text(aes(label = round(Importance, 0)), hjust = 0.5, size = 4)

#To build a more robust and consistent classification model using Random Forest that can better
#predict network attack based on Protocol and SrcBytes predictive analysis
#proceed to build the random forest model
rf_model <- randomForest(Label ~ Protocol_Top6 + SrcBytes_log,
                         data = train_data, 
                         ntree = 200,      
                         mtry = 2, 
                         importance = TRUE,
                         do.trace = 50)
#display for a quick summary
print(rf_model)

#predict the label for the test set
rf_prediction <- predict(rf_model, newdata = testing_data)

#check model performance using a confusion matrix
conf_matrix <- table(Predicted = rf_prediction, Actual = testing_data$Label)
print(conf_matrix)

#calculate the accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Random Forest Accuracy:", round(accuracy * 100, 2), "%\n")

#extract variable importance from the RF model
importance_df <- as.data.frame(rf_model$importance) %>%
  rownames_to_column(var = "Feature") %>%
  rename(Importance = MeanDecreaseAccuracy) %>% 
  arrange(desc(Importance))
importance_df
#plot with ggplot2
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity", width = 0.6) + 
  coord_flip() + 
  scale_fill_gradient(low = "green", high = "darkgreen") + 
  labs(title = "Random Forest Feature Importance",
       x = "",
       y = "Importance (Mean decrease accuracy)") + 
  theme_minimal(base_size = 14) + 
  theme(legend.position = "none")


#additional Features Correlation Analysis
#select only numeric variables
numeric_vars <- df[, sapply(df, is.numeric)]

#remov all column that has been created
columns_to_remove <- c("predicted_prob", "Protocol_Top5")
#remove column with zero variance
numeric_vars <- numeric_vars[, !names(numeric_vars) %in% columns_to_remove]
#compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")
#Convert to data frame to find strongest correlations
cor_data <- as.data.frame(as.table(cor_matrix))
#Remove self-correlations
cor_data <- cor_data[cor_data$Var1 != cor_data$Var2, ]

#add absolute correlations for sort
cor_data <- cor_data %>%
  mutate(abs_value = abs(Freq)) %>%
  arrange(desc(abs_value))

#select top 10 strongest correlations
top_features <- unique(c(cor_data$Var1[1:10], cor_data$Var2[1:10]))

#subset correlation matrix
cor_subset <- cor_matrix[top_features, top_features]

#plot heatmap
corrplot(cor_subset,
         method = "color",
         type = "lower",
         tl.col = "black",
         tl.cex = 0.9,
         addCoef.col = "black",
         number.cex = 0.8,
         cl.cex = 0.8,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         mar = c(0, 0, 2, 0),
         title = "Top 18 Most Correlated Network Traffic Features")

