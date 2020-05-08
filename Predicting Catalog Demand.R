#importing the datasets using readxl
library(readxl)
p1_customers <- read_excel("C:/Users/asus/OneDrive - University of Waterloo/MOOCs/Udacity - Business Analyst nd008 v3.0.0/Part 01-Module 02-Lesson 05_Predicting Catalog Demand/Predicting Catalog Demand/p1-customers.xlsx")
p1_mailinglist <- read_excel("C:/Users/asus/OneDrive - University of Waterloo/MOOCs/Udacity - Business Analyst nd008 v3.0.0/Part 01-Module 02-Lesson 05_Predicting Catalog Demand/Predicting Catalog Demand/p1-mailinglist.xlsx")

#changing the column name of "#_Years_as_Customer" to "NumYears"
colnames(p1_customers)[colnames(p1_customers) == "#_Years_as_Customer"] <- "NumYears"
colnames(p1_mailinglist)[colnames(p1_mailinglist) == "#_Years_as_Customer"] <- "NumYears"

#loading ggplot2 package for data visualizations of each variable in p1_customers
library(ggplot2)

#Scatterplot of ZIP codes and Average Sales Amount
ggplot(p1_customers, aes(x = ZIP, y = Avg_Sale_Amount)) +
  geom_point() +   geom_smooth(method = "lm") +
  ylab("Average Sales Amount")+
  ggtitle("Relationship between 'ZIP' and 'Average Sales Amount'") + theme(plot.title = element_text(hjust = 0.5))

#Scatterplot of Store Number and Average Sales Amount
ggplot(p1_customers, aes(x = Store_Number, y = Avg_Sale_Amount)) +
  geom_point() +   geom_smooth(method = "lm") +
  scale_x_continuous(name = "Store Number",breaks = seq(100,109, by = 1))+
  ylab("Average Sales Amount")+
  ggtitle("Relationship between 'Store Number' and 'Average Sales Amount'") + theme(plot.title = element_text(hjust = 0.5))

#Boxplots of whether or not a customer responded to last catalog and Average Sales Amount
ggplot(p1_customers, aes(x = Responded_to_Last_Catalog, y = Avg_Sale_Amount)) +
  geom_boxplot() + 
  xlab("Responded to Last Catalog?")+
  ylab("Average Sales Amount")+
  ggtitle("Relationship between 'Responded to Last Catalog?' and 'Average Sales Amount'") + theme(plot.title = element_text(hjust = 0.5))
geom_smooth(method = "lm")

#Scatterplot of Average Number of Products Purchased and Average Sales Amount
ggplot(p1_customers, aes(x = Avg_Num_Products_Purchased, y = Avg_Sale_Amount)) +
  geom_jitter() +   geom_smooth(method = "lm") +
  xlab("Average Number of Products Purchased")+
  ylab("Average Sales Amount")+
  ggtitle("Relationship between 'Average Number of Products Purchased' and 'Average Sales Amount'") + theme(plot.title = element_text(hjust = 0.5))

#Scatterplot of Number of Years as a customer and Average Sales Amount
ggplot(p1_customers, aes(x = NumYears, y = Avg_Sale_Amount)) +
  geom_jitter() +   geom_smooth(method = "lm") +
  scale_x_continuous(name = "Number of Years as a customer",breaks = seq(1,8, by = 1))+
  ylab("Average Sales Amount")+
  ggtitle("Relationship between 'Number of Years as customer' and 'Average Sales Amount'") + theme(plot.title = element_text(hjust = 0.5))

#Boxplots of different customer segments and Average Sales Amount
ggplot(p1_customers, aes(x = Customer_Segment, y = Avg_Sale_Amount)) +
  geom_boxplot() +   geom_smooth(method = "lm") +
  xlab("Customer Segment")+
  ylab("Average Sales Amount")+
  ggtitle("Relationship between 'Customer Segment' and 'Average Sales Amount'") + theme(plot.title = element_text(hjust = 0.5))

#Running a multiple linear regression model where Average Sales Amount is the dependent variable and Average Number of Products Purchased and Customer Segments are predictors
linear_model <- lm(Avg_Sale_Amount ~ Avg_Num_Products_Purchased + Customer_Segment, data = p1_customers)
summary(linear_model)

#Checking for Linear Regression Assumptions
par(mfrow = c(2,2))
plot(linear_model)

#Adding a new column 'Predicted_Prices' to p1_mailinglist dataset using our regression model
p1_mailinglist$Predicted_Prices <- ifelse(p1_mailinglist$Customer_Segment == "Loyalty Club Only",303.46 + 66.98* p1_mailinglist$Avg_Num_Products_Purchased - 149.36, 
                                   ifelse(p1_mailinglist$Customer_Segment == "Loyalty Club and Credit Card", 303.46 + 66.98* p1_mailinglist$Avg_Num_Products_Purchased + 281.839, 
                                   ifelse(p1_mailinglist$Customer_Segment == "Store Mailing List",303.46 + 66.98* p1_mailinglist$Avg_Num_Products_Purchased - 245,303.46 + 66.98* p1_mailinglist$Avg_Num_Products_Purchased
)))

#Adding a new column 'Avg_Sales' to p1_mailinglist datasets which is a product of Predicted Prices and Probabilities of Customers willing to buy
p1_mailinglist$Avg_Sales <- p1_mailinglist$Predicted_Prices*p1_mailinglist$Score_Yes
Total_Profit <- sum(p1_mailinglist$Avg_Sales)
Total_Profit

#Computing the Expected Profit
Gross_Margin <- Total_Profit * 0.5
Expected_Profit <- Gross_Margin - (250*6.5)
Expected_Profit
