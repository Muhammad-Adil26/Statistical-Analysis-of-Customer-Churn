library(ggplot2)
library(caret)
library(dplyr)

data <- read.csv('telecom_churn.csv')

summary(data)
str(data)

ggplot(data, aes(x = Churn)) + 
  geom_bar(fill = 'skyblue') + 
  labs(title = "Customer Churn Distribution", x = "Churn", y = "Count")

data$Churn <- as.factor(data$Churn)
data <- na.omit(data)

set.seed(123)
index <- createDataPartition(data$Churn, p = 0.7, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

model <- glm(Churn ~ tenure + MonthlyCharges + Contract + PaymentMethod, 
             data = train_data, family = binomial)

predictions <- predict(model, test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, "Yes", "No")

conf_matrix <- confusionMatrix(as.factor(predicted_class), test_data$Churn)
print(conf_matrix)

accuracy <- conf_matrix$overall['Accuracy']
cat("Accuracy:", accuracy, "\n")

ggplot(test_data, aes(x = tenure, y = predictions, color = Churn)) +
  geom_point() +
  labs(title = "Predicted Probabilities of Churn", x = "Tenure", y = "Predicted Probability")

set.seed(123)
ab_test_data <- sample_n(data, 500)
group <- sample(c("Control", "Test"), 500, replace = TRUE)
ab_test_data$Group <- group

test_group <- ab_test_data %>% filter(Group == 'Test')
control_group <- ab_test_data %>% filter(Group == 'Control')

test_churn_rate <- mean(test_group$Churn == "Yes")
control_churn_rate <- mean(control_group$Churn == "Yes")

cat("Test Group Churn Rate:", test_churn_rate, "\n")
cat("Control Group Churn Rate:", control_churn_rate, "\n")
