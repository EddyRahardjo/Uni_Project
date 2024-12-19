#Loading the libraries
options(warn = -1) #hiding the warning from loading the libraries
library(tidyverse)
library(ROSE)
library(caTools)
library(reshape2)
library(scales)
library(glmnet)   
library(psych) 
library(lubridate)
library(gridExtra)
library(e1071)
library(lmtest)
library(car)
library(readxl)
library(caret)


#Loading the dataset
dataset <- as.data.frame(read_csv("./Car_details_v3.csv"))


#Taking a peek on the dataset
head(dataset)

colSums(is.na(dataset))

str(dataset)

#Specifying the pattern to be removed by regex for torque column
pattern <- "(?<=@ )\\d+(?=[-\\d]*rpm)|(?<=@ )\\d+(?=\\()"
#Wrangling the data
dataset <- dataset %>%
  #Dropping name column
  #subset(., select = -c(name)) %>%
  #Wrangling the problematic column
  mutate(
    mileage = as.numeric(gsub(" kmpl| km/kg", "", mileage)), 
    engine = as.numeric(gsub(" CC", "", engine)), 
    max_power = as.numeric(gsub(" bhp", "", max_power)), 
    torque = as.numeric(str_extract(torque, pattern)), 
    seats = as.factor(seats)
  ) %>% 
  mutate_if(is.character, as.factor)#Converting to proper datatype 

#Showing summary for the dataset
summary(dataset)

head(dataset)

#Deleting the name column
dataset <- subset(dataset, select = -c(name, km_driven))

#Converting the year column to date datatype
dataset$year <- make_date(dataset$year)

#Checking if the column is  onverted to date datatype
unique(dataset$year)

#Deleting the null values 
dataset <- dataset %>%
    drop_na()

#Checking how many na is left in the dataset
print(paste('Na left in the dataset= ',sum(is.na(dataset))))

summary(dataset)

dataset <- dataset %>%
    filter(owner != 'Test Drive Car') %>%
    select(-selling_price, everything(), selling_price)

str(dataset)

#Making dataframe with only numeric variable
dataset_cont <- dataset %>%
    select(where(is.numeric))

#Making dataframe with only categorical variable with the selling_price column
dataset_cat <- dataset %>%
    select(where(is.factor), selling_price)

#Checking if both dataset has the needed column
head(dataset_cont)
head(dataset_cat)

#Transforming the long continuous dataset into wide format
cont_melt = melt(dataset_cont, variable_name = "Feature")
#Checking if the dataset is transformed successfuly
head(cont_melt)

options(repr.plot.width = 20, repr.plot.height = 10)

ggplot(cont_melt, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot(alpha=1) + ylab("Value") + xlab("Feature" ) +
  theme_bw() + 
  ggtitle("Boxplots, showing distribution of data for all numerical features") + 
  theme(legend.position="none", 
        axis.text.x = element_text(hjust = 1, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
        facet_wrap(~variable, nrow = 1, scales = "free")


#Checking the Q1 and Q3 of the columns
summary(dataset_cont)

#Calculating the IQR for the columns we'll dealt with
print(paste("IQR for mileage column: ", IQR(dataset_cont$mileage)))
print(paste("IQR for engine column: ", IQR(dataset_cont$engine)))
print(paste("IQR for max_power column: ", IQR(dataset_cont$max_power) ))
print(paste("IQR for selling_price column: ", IQR(dataset_cont$selling_price)))

#Dealing with the outlier using the IQR method to determine the upper and lower boundary
dataset_cont <- dataset_cont %>%
  filter(selling_price <= 1000000, mileage > 13, mileage <27, max_power <= 125, engine <= 1800,)

options(repr.plot.width = 20, repr.plot.height = 10)
cont_melt = melt(dataset_cont, variable_name = "Feature")

ggplot(cont_melt, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot(alpha=1) + ylab("Value") + xlab("Feature" ) +
  theme_bw() + 
  ggtitle("Boxplots, showing distribution of data for all numerical features") + 
  theme(legend.position="none", 
        axis.text.x = element_text(hjust = 1, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
        facet_wrap(~variable, nrow = 1, scales = "free")

#Setting the width and height of the plot
options(repr.plot.width = 15, repr.plot.height = 15)

#Using the ggplot for to plot the distribution over the boxplot
ggplot(cont_melt, aes(x=variable, y=value)) + 
geom_boxplot(alpha=0.7, color="black", aes(fill=variable)) + 
geom_jitter(color="black", size=0.4, alpha=0.7) +
facet_wrap(~variable, nrow=3, scales = "free") + 
labs(x="", y="", fill='variable') + 
ggtitle("Showing the distribution of all the data over the numerical boxplot") + 
theme_bw() + 
theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.text.x=element_blank())

#Filtering the main dataset according to what have been done to the continuous variable
dataset <- dataset %>% 
  filter(selling_price <= 1000000, mileage > 13, mileage <27, max_power <= 125, engine <= 1800,)

options(repr.plot.width = 15, repr.plot.height = 15)
#Making histogram for mileage
p1<-ggplot(aes(x=mileage), data = dataset_cont) +
    geom_histogram(color = I('black'), fill = "red") +
    ggtitle('Distribution of the mileage')

#Making histogram for engine
p2<-ggplot(aes(x=engine), data = dataset_cont) +
    geom_histogram(color = I('black'), fill = "red") +
    ggtitle('Distribution of the engine capacity of the car')

#Making histogram for max power
p3<-ggplot(aes(x=max_power), data = dataset_cont) +
    geom_histogram(color = I('black'), fill = "red") +
    ggtitle('Distribution of the max power of the car')

#Making histogram for torque
p4<-ggplot(aes(x=torque), data = dataset_cont) +
    geom_histogram(color = I('black'), fill = "red") +
    ggtitle('Distribution of the torque power of the car')

#Making histogram for selling price
p5<-ggplot(aes(x=selling_price), data = dataset_cont) +
    geom_histogram(color = I('black'), fill = "red") +
    ggtitle('Distribution of the price of the car')
# plot all 4, 2 x 2

grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

#Checking the skewness and kurtossis of the price using the e1701 library

print("Skewness is: ")
round(skewness(dataset_cont$selling_price),4)
print("Kurtosis is: ")
round(kurtosis(dataset_cont$selling_price),4)

#Using ggplot to plot the QQ plot

qq <- ggplot(cont_melt, aes(sample = value, color= variable)) +
    geom_qq() +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ variable, nrow = 2, scales = "free") +
    guides(color = 'none') +
    ggtitle("QQ plot of all the continuous variable") + 
    theme(text = element_text(size = 20))

qq

#Making the scatterplot matrix
pairs(dataset_cont)

#Making the correlation coefficient for all variables
round(cor(dataset_cont[1:5]),3)

#Deleting the max_power column
dataset <- subset(dataset, select = -c(max_power))

#Filtering the dataset according what have been done in continuous variables
dataset_cat <- dataset_cat %>%
  filter(selling_price <= 1000000)

#Pivoting the dataset to longer format
cat_pivot <- dataset_cat %>%
    pivot_longer(cols = c('fuel', 'seller_type', 'transmission', 'owner', 'seats'), names_to = 'category', values_to = 'value')

head(cat_pivot)

options(repr.plot.width = 10, repr.plot.height = 10)

#Plotting the barchart to count the categorical values
ggplot(cat_pivot, aes(x = value, fill = value)) +
    geom_bar() +
    facet_wrap(~category, scales = 'free') +
    guides(fill = 'none') +
    labs(x = 'Features') +
    labs(y = 'Count') +
    ggtitle('Barchart of Categorical Features')+
    theme(text = element_text(size = 20),
         axis.text.x = element_text(angle = 45, hjust = 1))

options(repr.plot.width = 10, repr.plot.height = 20)

#Plotting the boxplot with the distribution over it for all categorical variables
ggplot(cat_pivot, aes(x = value, y = selling_price, fill = value)) +
    geom_boxplot(alpha=0.7, color="black", aes(fill=value)) +
    geom_jitter(color="black", size=0.3, alpha=0.3) +
    facet_wrap(~category, nrow = 3, scales = 'free') +
    guides(fill = 'none') +
    scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
    labs(x = 'Variable') +
    labs(y = 'Selling Price') +
    theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Deleting the 2,4,14 variable from seat
dataset <- dataset[!(dataset$seats %in% c('2', '4', '14')),]

#Aggregate the continuous variable by sum
dataset_agg_sum <- aggregate(dataset[, c('mileage', 'engine', 'torque')], by=dataset['year'], sum)

#Aggregate the continuous variable by mean
dataset_agg_mean <- aggregate(dataset[, c("selling_price")], by=dataset["year"], mean)

#Both dataset is then merged by the date column
dataset_agg <- merge(dataset_agg_sum, dataset_agg_mean, by="year")

#Renaming the last column
colnames(dataset_agg)[colnames(dataset_agg) == "x"] <- "selling_price"

#Seeing if the dataset is successfully merged
head(dataset_agg)

options(repr.plot.width = 18, repr.plot.height = 15)

#Melting the dataset to make the timeseries for continuous variable
dataset_melt = melt(dataset_agg, id = "year", variable_name = "Feature")

#Plotting the timeseries line graph for the continuous ariable
ggplot(dataset_melt, aes(x=year, y=value, color=variable)) + 
geom_point(size = 1.5, alpha = 0.6, aes(fill=variable)) +
stat_smooth(method="loess", span=0.1, size=0.5) +
facet_wrap(~variable, ncol=2, scales = "free") + 
labs(x="Variables", y="", fill='variable') + 
ggtitle("Line graph for each variable by year") + 
theme_bw() + 
theme(legend.position="none", plot.title = element_text(hjust = 0.5), 
      axis.text.x = element_text(angle = 45, hjust = 1))

options(repr.plot.width = 18, repr.plot.height = 15)

#Making the linechart for the fuel variable
fig1 <- ggplot(dataset, aes(x=year, y=selling_price, group=fuel, color=fuel)) +
  geom_point(size = 1.1, alpha = 0) +
  stat_smooth(method="loess", span=0.5, size=1, alpha=0.05) +
  theme_bw() + 
  theme(legend.justification = "top", plot.title = element_text(hjust = 0.5)) + 
  labs(x="Price", y = "Fuel") +
  scale_x_date("Price") +
  guides(colour=guide_legend(title="Fuel")) +
  ggtitle("Selling Price per Fuel")

#Making the linechart for the transmission variable
fig2 <- ggplot(dataset, aes(x=year, y=selling_price, group=transmission, color=transmission)) +
  geom_point(size = 0.1, alpha = 0) +
  stat_smooth(method="loess", span=0.5, size=1, alpha=0.05) +
  theme_bw() + 
  theme(legend.justification = "top", plot.title = element_text(hjust = 0.5)) + 
  labs(x="Price", y = "Transmission") +
  scale_x_date("Price") +
  guides(colour=guide_legend(title="Transmission")) +
  ggtitle("Selling Price per Transmission")

#Making the linechart for the owner variable
fig3 <- ggplot(dataset, aes(x=year, y=selling_price, group=owner, color=owner)) +
  geom_point(size = 0.1, alpha = 0) +
  stat_smooth(method="loess", span=0.5, size=1, alpha=0.05) +
  theme_bw() + 
  theme(legend.justification = "top", plot.title = element_text(hjust = 0.5)) + 
  labs(x="Price", y = "Owner") +
  scale_x_date("Price") +
  guides(colour=guide_legend(title="Owner")) +
  ggtitle("Selling Price per Owner")

#Making the linechart for the seller_type variable
fig4 <- ggplot(dataset, aes(x=year, y=selling_price, group=seller_type, color=seller_type)) +
  geom_point(size = 0.1, alpha = 0) +
  stat_smooth(method="loess", span=0.5, size=1, alpha=0.05) +
  theme_bw() + 
  theme(legend.justification = "top", plot.title = element_text(hjust = 0.5)) + 
  labs(x="Price", y = "Seller Type") +
  scale_x_date("Price") +
  guides(colour=guide_legend(title="Seller Type")) +
  ggtitle("Selling Price per Seller Type")

#Making the linechart for the seats variable
fig5 <- ggplot(dataset, aes(x=year, y=selling_price, group=seats, color=seats)) +
  geom_point(size = 0.1, alpha = 0) +
  stat_smooth(method="loess", span=0.5, size=1, alpha=0.05) +
  theme_bw() + 
  theme(legend.justification = "top", plot.title = element_text(hjust = 0.5)) + 
  labs(x="Price", y = "Seats") +
  scale_x_date("Price") +
  guides(colour=guide_legend(title="Seats")) +
  ggtitle("Selling Price per Seats")

grid.arrange(fig1, fig2, fig3, fig4, fig5, ncol=2)

#Setting the seed so that the code can be replicated
set.seed(42)

#Making a new dataset for ML purposes
dataset_ml <- dataset

#Converting the year column to factor again and wrangled so it can be used for predictor
dataset_ml$year <- format(dataset_ml$year, "%Y")
dataset_ml <- dataset_ml %>%
    mutate(
        year = as.factor(year)
    )


#Splitting the dataset with 0.7 ratio
train_ratio<- sample(seq_len(nrow(dataset_ml)), size = 0.7 * nrow(dataset_ml))

# Create training and testing sets
train <- dataset_ml[train_ratio, ]
test <- dataset_ml[-train_ratio, ]

#Modelling the Linear Regression
lm_car = lm(selling_price~., data = train) 
#Getting the specifics of the linear regression from summary function
summary(lm_car) #Review the results

# Use the predict() function to make predictions on the training data then calculate the residuals
predictions = predict(lm_car, test)
residuals = test$selling_price - predictions

# Calculate MAE, MSE, RMSE
mae = mean(abs(residuals))
mse = mean(residuals^2)
rmse = sqrt(mse)
variance = var(test$selling_price)

# Print the results
print(paste("Variance: ", variance))
print(paste("Mean Absolute Error: ", mae))
print(paste("Mean Squared Error: ", mse))
print(paste("Root Mean Squared Error: ", rmse))


options(repr.plot.width = 15, repr.plot.height = 5)

#Plotting the histogram for the residual
lm_hist <- ggplot(data = train, aes(x = lm_car$residuals)) + 
    geom_histogram(bins=30, alpha=0.7, color="black", fill="red") + 
    labs(x="Residuals", y ="Count") +
    ggtitle("Residuals Histogram") + 
    theme_bw() + 
    theme(axis.title = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.title.y = element_text(vjust = 4), 
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

#Plotting the boxplot for the residual
lm_box <- ggplot(data= train, aes(lm_car$residuals)) + 
    geom_boxplot(alpha=0.7, fill="red") + ylab("") + xlab("Residuals") +
    theme_bw() + 
    ggtitle("Residuals Boxplot") + 
    theme(axis.title = element_text(size = 10), 
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

grid.arrange(lm_hist, lm_box, ncol=2)



options(repr.plot.width = 10, repr.plot.height = 8)

#Plotting the scatterplot for residuals vs fitted, scale vs location, residual vs leverage. And the qq plot for residuals
par(mfrow=c(2,2))
plot(lm_car, col='red')

#Breush Pagan and NCV test to evaluate the residuals
bptest(lm_car)

#Configuring the forward stepwise regression model
lm_forward <- step(lm_car, direction = "forward")
summary(lm_forward)

# Use the predict() function to make predictions on the test data
predictions_forward = predict(lm_forward, test)

# Calculate the residuals/errors
residuals_forward = test$selling_price - predictions_forward

# Calculate MAE
mae_forward = mean(abs(residuals))

# Calculate MSE
mse_forward = mean(residuals^2)

# Calculate RMSE
rmse_forward = sqrt(mse_forward)

# Print the metrics
print(paste("Variance: ", var(test$selling_price)))
print(paste("Mean Absolute Error: ", mae_forward))
print(paste("Mean Squared Error: ", mse_forward))
print(paste("Root Mean Squared Error: ", rmse_forward))

options(repr.plot.width = 15, repr.plot.height = 5)

#Plotting the histogram for the residual
forward_hist <- ggplot(data = train, aes(x = lm_forward$residuals)) + 
    geom_histogram(bins=30, alpha=0.7, color="black", fill="#ff0000") + 
    labs(x="Residuals", y ="Count") +
    ggtitle("Residuals Histogram") + 
    theme_bw() + 
    theme(axis.title = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.title.y = element_text(vjust = 4), 
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

#Plotting the boxplot for the residual
forward_box <- ggplot(data= train, aes(lm_forward$residuals)) + 
    geom_boxplot(alpha=0.7, fill="#ff0000") + ylab("") + xlab("Residuals") +
    theme_bw() + 
    ggtitle("Residuals Boxplot") + 
    theme(axis.title = element_text(size = 10), 
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

grid.arrange(forward_hist, forward_box, ncol=2)

options(repr.plot.width = 10, repr.plot.height = 8)

#Plotting the scatterplot for residuals vs fitted, scale vs location, residual vs leverage. And the qq plot for residuals
par(mfrow=c(2,2))
plot(lm_forward, col='red')

#Breush Pagan and NCV test to evaluate the residuals
bptest(lm_forward)

#Configuring the backward stepwise regression then getting the summary
lm_back <- step(lm_car)
summary(lm_back)

# Use the predict() function to make predictions on the training data
predictions = predict(lm_back, test)

# Calculate the residuals/errors
residuals_back = test$selling_price - predictions

# Calculate MAE
mae_back = mean(abs(residuals_back))

# Calculate MSE
mse_back = mean(residuals_back^2)

# Calculate RMSE
rmse_back = sqrt(mse_back)

# Print the metrics
print(paste("Variance: ", var(test$selling_price)))
print(paste("Mean Absolute Error: ", mae_back))
print(paste("Mean Squared Error: ", mse_back))
print(paste("Root Mean Squared Error: ", rmse_back))

options(repr.plot.width = 15, repr.plot.height = 5)


back_hist <- ggplot(data = train, aes(x = lm_back$residuals)) + 
    geom_histogram(bins=30, alpha=0.7, color="black", fill="red") + 
    labs(x="Residuals", y ="Count") +
    ggtitle("Residuals Histogram") + 
    theme_bw() + 
    theme(axis.title = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.title.y = element_text(vjust = 4), 
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

back_box <- ggplot(data= train, aes(lm_back$residuals)) + 
    geom_boxplot(alpha=0.7, fill="red") + ylab("") + xlab("Residuals") +
    theme_bw() + 
    ggtitle("Residuals Boxplot") + 
    theme(axis.title = element_text(size = 10), 
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

grid.arrange(back_hist, back_box, ncol=3)


options(repr.plot.width = 10, repr.plot.height = 8)
par(mfrow=c(2,2))
plot(lm_back, col='red')

#Breush Pagan to evaluate the residuals
bptest(lm_back)


#Transforming the train dataset into matrix form
train_m <- train %>%
  select(-selling_price) %>%
  bind_cols(train %>% select(selling_price))

y <- train_m %>%
    select(selling_price) %>%
    as.matrix() 

X <- model.matrix(~ . - 1, data = train_m %>% select(-selling_price))

#Transforming the test dataset into matrix form
test_m <- test %>%
  select(-selling_price) %>%
  bind_cols(test %>% select(selling_price))

y_test <- test_m %>%
    select(selling_price) %>%
    as.matrix() 

X_test <- model.matrix(~ . - 1, data = test_m %>% select(-selling_price))


# Train the Ridge regression model
ridge_model <- glmnet(X, y, alpha = 0)  # alpha = 0 for Ridge regression

# Selecting the optimal lambda using cross-validation
cv_ridge <- cv.glmnet(X, y, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min

# Train the final Ridge regression model with the best lambda
ridge_model_best <- glmnet(X, y, alpha = 0, lambda = best_lambda_ridge)
print(ridge_model_best)

#Calculating the residuals towards the test data
predictions_l2 = predict(ridge_model_best, X_test)
residuals_l2 = test$selling_price - predictions_l2

#Calculate MAE, MSE, and RMSE
mae_l2 = mean(abs(residuals_l2))
mse_l2 = mean(residuals_l2^2)
rmse_l2 = sqrt(mse_l2)

#Print MAE, MSE, and RMSE
print(paste("Variance: ", var(test$selling_price)))
print(paste("Mean Absolute Error: ", mae_l2))
print(paste("Mean Squared Error: ", mse_l2))
print(paste("Root Mean Squared Error: ", rmse_l2))

#Counting the train residuals
residual_ridge = test$selling_price - predict(ridge_model_best, X_test)
#Counting the test residuals
predict_ridge = predict(ridge_model_best, X_test)

#Combining both residuals into one dataframe
combine_ridge <- cbind(residual_ridge, predict_ridge)
colnames(combine_ridge) <- c('resi', 'pred')
combine_ridge <- as.data.frame(combine_ridge)

#Plotting the histogram for the Ridge Residuals
ridge_1 <- ggplot(combine_ridge, aes(x = resi)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Ridge Regression Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

#Plotting the boxplot for the Ridge residuals
ridge_2 <- ggplot(combine_ridge, aes(y = resi)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Boxplot of Ridge Regression Residuals",
       y = "Residuals") +
  theme_minimal() +
  coord_flip()

grid.arrange(ridge_1, ridge_2)

#Plotting the scatter plot of residual vs prediction
point_ridge <- ggplot(data=combine_ridge, aes(x = resi, y = pred)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot of Residual_Prediction", 
       x = "Residual", 
       y = "Prediction") +
  theme_minimal()

#Plotting the qq plot for the residual
qq_ridge <- ggplot(combine_ridge, aes(sample = resi)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "QQ Plot of Residuals") +
  theme_minimal()

#Plotting the scatter plot for scale location plot
combine_ridge$sqrt_resi <- sqrt(abs(combine_ridge$resi))
scale_ridge <- ggplot(combine_ridge, aes(x = pred, y = sqrt_resi)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Scale-Location Plot", x = "Fitted Values", y = "Sqrt |Standardized Residuals|") +
  theme_minimal()

#Plotting the scatter plot for residual leverage
combine_ridge$leverage <- seq(0.1, 0.9, length.out = nrow(combine_ridge))
leverage_ridge <- ggplot(combine_ridge, aes(x = leverage, y = resi)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals_Leverage", x = "Leverage", y = "Residuals") +
  theme_minimal()

grid.arrange(point_ridge, qq_ridge, scale_ridge, leverage_ridge)

#Breush Pagan to evaluate the residuals
bptest(residuals_l2 ~ X_test)

#Train the Lasso regression model
lasso_model <- glmnet(X, y, alpha = 1)  # alpha = 1 for Lasso regression

#Selecting the the optimal lambda using cross-validation
cv_lasso <- cv.glmnet(X, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min

#Train the final Lasso regression model with the best lambda
lasso_model_best <- glmnet(X, y, alpha = 1, lambda = best_lambda_lasso)
print(lasso_model_best)

#Calculating the residuals towards the test data
predictions_l1 = predict(lasso_model_best, X_test)
residuals_l1 = test$selling_price - predictions_l1

#Calculate MAE, MSE, and RMSE
mae_l1 = mean(abs(residuals_l1))
mse_l1 = mean(residuals_l1^2)
rmse_l1 = sqrt(mse_l1)

#Print MAE, MSE, and RMSE
print(paste("Variance: ", var(test$selling_price)))
print(paste("Mean Absolute Error: ", mae_l1))
print(paste("Mean Squared Error: ", mse_l1))
print(paste("Root Mean Squared Error: ", rmse_l1))

#Counting the train residuals
residual_lasso = test$selling_price - predict(lasso_model_best, X_test)
#Counting the test residuals
predict_lasso = predict(lasso_model_best, X_test)

#Combining both residuals into one dataframe
combine_lasso <- cbind(residual_lasso, predict_lasso)
colnames(combine_lasso) <- c('resi', 'pred')
combine_lasso <- as.data.frame(combine_lasso)

#Plotting the histogram for lasso residuals
lasso_1 <- ggplot(combine_lasso, aes(x = resi)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Lasso Regression Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

#Plotting the boxplot for lasso residuals
lasso_2 <- ggplot(combine_lasso, aes(y = resi)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Boxplot of Lasso Regression Residuals",
       y = "Residuals") +
  theme_minimal() +
  coord_flip()

grid.arrange(lasso_1, lasso_2)

#Plotting the scatter plot for residual_prediction
point_lasso <- ggplot(data=combine_lasso, aes(x = resi, y = pred)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot of Residual_Prediction", 
       x = "Residual", 
       y = "Prediction") +
  theme_minimal()

#Plotting the qqplot for residual
qq_lasso <- ggplot(combine_lasso, aes(sample = resi)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "QQ Plot of Residuals") +
  theme_minimal()

#Plotting the scatter plot for scale location
combine_lasso$sqrt_resi <- sqrt(abs(combine_lasso$resi))
scale_lasso <- ggplot(combine_lasso, aes(x = pred, y = sqrt_resi)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Scale-Location Plot", x = "Fitted Values", y = "Sqrt |Standardized Residuals|") +
  theme_minimal()

#Plotting the scatter plot for residual_leverage
combine_lasso$leverage <- seq(0.1, 0.9, length.out = nrow(combine_lasso))
leverage_lasso <- ggplot(combine_lasso, aes(x = leverage, y = resi)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals_Leverage", x = "Leverage", y = "Residuals") +
  theme_minimal()

grid.arrange(point_lasso, qq_lasso, scale_lasso, leverage_lasso)

#Breush Pagan to evaluate the residuals
bptest(residual_lasso ~ X_test)

#Train the Elastic Net model
elastic_net_model <- glmnet(X, y, alpha = 0.5)  # alpha = 0.5 for Elastic Net (between Ridge and Lasso)

#Selecting the optimal lambda using cross-validation
cv_elastic_net <- cv.glmnet(X, y, alpha = 0.5)
best_lambda_elastic_net <- cv_elastic_net$lambda.min

#Training the Elastic Net model with the best lambda
elastic_net_model_best <- glmnet(X, y, alpha = 0.5, lambda = best_lambda_elastic_net)
print(elastic_net_model_best)

#Counting the residual toward the train data
predictions_lc = predict(elastic_net_model_best, X_test)
residuals_lc = test$selling_price - predictions_lc

#Calculate MAE, MSE, and RMSE
mae_lc = mean(abs(residuals_lc))
mse_lc = mean(residuals_lc^2)
rmse_lc = sqrt(mse_lc)

#Print the MAE, MSE, and RMSE
print(paste("Variance: ", var(test$selling_price)))
print(paste("Mean Absolute Error: ", mae_lc))
print(paste("Mean Squared Error: ", mse_lc))
print(paste("Root Mean Squared Error: ", rmse_lc))

#Counting the train residuals
residual_elastic = test$selling_price - predict(elastic_net_model_best, X_test)
#Counting the test residuals
predict_elastic= predict(elastic_net_model_best, X_test)

#Combining both residuals into one dataframe
combine_elastic <- cbind(residual_elastic, predict_elastic)
colnames(combine_elastic) <- c('resi', 'pred')
combine_elastic <- as.data.frame(combine_elastic)

#Plotting the histogram for elastic net residuals
elastic_1 <- ggplot(combine_elastic, aes(x = resi)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Elastic Net Regression Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

#Plotting the boxplot for elastic net residuals
elastic_2 <- ggplot(combine_elastic, aes(y = resi)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Boxplot of Elastic Net Regression Residuals",
       y = "Residuals") +
  theme_minimal() +
  coord_flip()

grid.arrange(elastic_1, elastic_2)

#Plotting the scatter plot for residual_prediction
elastic_point <- ggplot(data=combine_elastic, aes(x = resi, y = pred)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot of Residual vs Prediction", 
       x = "Residual", 
       y = "Prediction") +
  theme_minimal()

#Plotting the qqplot for residual
elastic_qq <- ggplot(combine_elastic, aes(sample = resi)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "QQ Plot of Residuals") +
  theme_minimal()

#Plotting the scatter plot for scale location
combine_elastic$sqrt_resi <- sqrt(abs(combine_elastic$resi))
elastic_scale <- ggplot(combine_elastic, aes(x = pred, y = sqrt_resi)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Scale-Location Plot", x = "Fitted Values", y = "Sqrt |Standardized Residuals|") +
  theme_minimal()

#Plotting the scatter plot for residual_leverage
combine_elastic$leverage <- seq(0.1, 0.9, length.out = nrow(combine_elastic))
elastic_leverage <- ggplot(combine_elastic, aes(x = leverage, y = resi)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs Leverage", x = "Leverage", y = "Residuals") +
  theme_minimal()

grid.arrange(elastic_point, elastic_qq, elastic_scale, elastic_leverage)

#Breush Pagan to evaluate the residuals
bptest(residuals_lc ~ X_test)

#Applying the varImp function to linear regression model
var = arrange(varImp(lm_car))
#Arranging the predictor in descending order
var <- var %>% 
    arrange(desc(Overall))
#Showing the top 10 result
head(var, 10)

#Modelling the Linear Regression
lm_final = lm(selling_price~ engine + fuel + seller_type + transmission + owner +  seats, data = train) 
#Getting the specifics of the linear regression from summary function
summary(lm_final) #Review the results

# Use the predict() function to make predictions on the training data then calculate the residuals
predictions_final = predict(lm_final, test)
residuals_final = test$selling_price - predictions_final

# Calculate MAE, MSE, RMSE
mae_final = mean(abs(residuals_final))
mse_final = mean(residuals_final^2)
rmse_final = sqrt(mse_final)
variance = var(test$selling_price)

# Print the results
print(paste("Variance: ", variance))
print(paste("Mean Absolute Error: ", mae_final))
print(paste("Mean Squared Error: ", mse_final))
print(paste("Root Mean Squared Error: ", rmse_final))

options(repr.plot.width = 15, repr.plot.height = 5)

#Plotting the histogram for the residual
lm_hist <- ggplot(data = train, aes(x = lm_final$residuals)) + 
    geom_histogram(bins=30, alpha=0.7, color="black", fill="red") + 
    labs(x="Residuals", y ="Count") +
    ggtitle("Residuals Histogram") + 
    theme_bw() + 
    theme(axis.title = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.title.y = element_text(vjust = 4), 
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

#Plotting the boxplot for the residual
lm_box <- ggplot(data= train, aes(lm_final$residuals)) + 
    geom_boxplot(alpha=0.7, fill="red") + ylab("") + xlab("Residuals") +
    theme_bw() + 
    ggtitle("Residuals Boxplot") + 
    theme(axis.title = element_text(size = 10), 
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

grid.arrange(lm_hist, lm_box, ncol=2)

options(repr.plot.width = 10, repr.plot.height = 8)

#Plotting the scatterplot for residuals vs fitted, scale vs location, residual vs leverage. And the qq plot for residuals
par(mfrow=c(2,2))
plot(lm_final, col='red')

#Breush Pagan to evaluate the residuals
bptest(residuals_final ~ X_test)

#Modelling the Linear Regression
lm_final2 = lm(selling_price~ engine + fuel + seller_type + transmission + owner + seats + year, data = train) 
#Getting the specifics of the linear regression from summary function
summary(lm_final2) #Review the results

# Use the predict() function to make predictions on the training data then calculate the residuals
predictions_final2 = predict(lm_final2, test)
residuals_final2 = test$selling_price - predictions_final2

# Calculate MAE, MSE, RMSE
mae_final2 = mean(abs(residuals_final2))
mse_final2 = mean(residuals_final2^2)
rmse_final2 = sqrt(mse_final2)
variance = var(test$selling_price)

# Print the results
print(paste("Variance: ", variance))
print(paste("Mean Absolute Error: ", mae_final2))
print(paste("Mean Squared Error: ", mse_final2))
print(paste("Root Mean Squared Error: ", rmse_final2))

#Plotting the histogram for the residual
lm_hist2 <- ggplot(data = train, aes(x = lm_final2$residuals)) + 
    geom_histogram(bins=30, alpha=0.7, color="black", fill="red") + 
    labs(x="Residuals", y ="Count") +
    ggtitle("Residuals Histogram") + 
    theme_bw() + 
    theme(axis.title = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.title.y = element_text(vjust = 4), 
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

#Plotting the boxplot for the residual
lm_box2 <- ggplot(data= train, aes(lm_final2$residuals)) + 
    geom_boxplot(alpha=0.7, fill="red") + ylab("") + xlab("Residuals") +
    theme_bw() + 
    ggtitle("Residuals Boxplot") + 
    theme(axis.title = element_text(size = 10), 
          plot.title = element_text(hjust = 0.5, size = 11.5),
          axis.text.y = element_text(angle = 90, vjust = 3),
          panel.grid = element_blank()
         )

grid.arrange(lm_hist, lm_box, ncol=2)

options(repr.plot.width = 10, repr.plot.height = 8)

#Plotting the scatterplot for residuals vs fitted, scale vs location, residual vs leverage. And the qq plot for residuals
par(mfrow=c(2,2))
plot(lm_final2, col='red')

#Breush Pagan to evaluate the residuals
bptest(residuals_final2 ~ X_test)
