# Load all the necessary libraries
library(tidyverse)
library(gridExtra)
library(caret)
library(themis)
library(rpart)
library(rpart.plot)

# Load the dataset for training the machine learning model
df_raw <- read_csv("./A2_customer_churn_labeled.csv")

# Evaluate dimensions
cat("The dataset has", dim(df_raw)[1], "records, each with", dim(df_raw)[2],"attributes.")

# Check the first few records
head(df_raw, 5)

# Check the variable datatype and content of the variable
str(df_raw)

# Check for missing values in the dataset
missing_values <- df_raw %>% summarise_all(~ sum(is.na(.)))
#print the missing values
cat("\nThe missing values for each variable: \n")
print(missing_values)

# Print the unique values of each  variable
for (col in names(df_raw)) {
  # Skip the `customer_profile` column
  if (col == 'customer_profile') {
    next
  } else if (col %in% c('ID', 'credit_score', 'tenure', 'balance', 'number_of_products', 'salary')) {
    # Print out the range
    cat("\nThe values in variable", col, "are: ", min(df_raw[col]), '-' , max(df_raw[col]))
  } else{
    # Convert the unique values to a single string separated by commas
    unique_values <- paste(unique(df_raw[[col]]), collapse = ", ")
    cat("\nThe values in variable", col, "are:", unique_values)}
}

# Printing the 10 first value for the customer profile column
head(df_raw$customer_profile, 10)

# Wrangling the data
df <- df_raw %>%
    mutate(has_credit_card = factor(has_credit_card, levels = c(0,1), labels = c('No', 'Yes')),
           is_active_member = factor(is_active_member, levels = c(0,1), labels = c('No', 'Yes')),
           Y = factor(Y, levels = c(0,1), labels = c('Not Churn', 'Churn')),
           age = as.numeric(sub(".*\\bis an? (\\d+)-year-old.*", "\\1", customer_profile)),
           origin = factor(sub(".*\\bfrom ([A-Za-z]+).*", "\\1", customer_profile)),
           gender = factor(sub(".*\\b(male|female)\\b.*", "\\1", customer_profile), labels = c('Female', 'Male'))) %>%
    select(-Y, -customer_profile, Y)

# Making sure that the changes has taken effect
head(df)

# First, let's check the summary of the dataset
summary(df)

options(repr.plot.width = 16, repr.plot.height = 8, repr.plot.res = 100)

# Create a bar chart to see the class imbalance
ggplot(df, aes(x = Y, fill = Y)) +
    geom_bar() +
    guides(fill = 'none') +
    theme(text = element_text(size = 20))

# Pivot the numerical variable to long form to make it easier to plot
df_long_num <- df %>%
    pivot_longer(cols = c('credit_score', 'balance', 'salary', 'age'),
                names_to = 'variable', values_to = 'value')

# Making the boxplot of numerical variable with the ggplot function
ggplot(df_long_num, aes(x = variable, y = value, fill = variable)) +
    geom_boxplot(notch = FALSE) +
    facet_wrap(~ variable, nrow = 2, scales = 'free') +
    guides(fill = 'none', alpha = 'none') +
    theme(text = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Adding the data points to see the density of the datapoints of the boxplot
ggplot(df_long_num, aes(x = variable, y = value, fill = variable)) +
    geom_boxplot(notch = FALSE) +
    geom_jitter(aes(alpha = 0.6)) +
    facet_wrap(~ variable, nrow = 2, scales = 'free') +
    guides(fill = 'none', alpha = 'none') +
    theme(text = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

hist <- ggplot(df_long_num, aes(x = value, fill = variable)) +
    geom_histogram(color = 'white') +
    facet_wrap(~ variable, nrow = 1, scales = 'free') +
    guides(fill = 'none') +
    theme(text = element_text(size = 20))

qq <- ggplot(df_long_num, aes(sample = value, color = variable)) +
    geom_qq() +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ variable, nrow = 1, scales = 'free') +
    guides(color = 'none') +
    theme(text = element_text(size = 20))

# Arrange the resulting plots into a grid
grid.arrange(hist, qq)

# Transforming the dataset to long format
df_long_cat <- df %>%
    mutate(number_of_products = factor(number_of_products),
           tenure = factor(tenure)) %>%
    pivot_longer(cols = c('number_of_products', 'has_credit_card', 'tenure', 'is_active_member', 'origin', 'gender'),
                names_to = 'variable', values_to = 'value')

# Making barchart for each categorical variable
br1 <- ggplot(df, aes(x = number_of_products, fill = factor(number_of_products))) +
    geom_bar() +
    guides(fill = 'none') +
    theme(text = element_text(size = 20))

br2 <- ggplot(df, aes(x = has_credit_card, fill = has_credit_card)) +
    geom_bar() +
    guides(fill = 'none') +
    theme(text = element_text(size = 20))

br3 <- ggplot(df, aes(x = tenure, fill = factor(tenure))) +
    geom_bar() +
    scale_x_continuous(breaks = seq(0, 10, 2)) +
    guides(fill = 'none') +
    theme(text = element_text(size = 20))

br4 <- ggplot(df, aes(x = is_active_member, fill = is_active_member)) +
    geom_bar() +
    guides(fill = 'none') +
    theme(text = element_text(size = 20))

br5 <- ggplot(df, aes(x = origin, fill = origin)) +
    geom_bar() +
    guides(fill = 'none') +
    theme(text = element_text(size = 20))

br6 <- ggplot(df, aes(x = gender, fill = gender)) +
    geom_bar() +
    guides(fill = 'none') +
    theme(text = element_text(size = 20))

grid.arrange(br1, br2, br3, br4, br5, br6, nrow = 3)

df %>%
    select(-ID) %>%
    pairs(upper.panel = NULL, cex.labels = 1.5)

df %>%
    select(-ID) %>%
    mutate_at(c('has_credit_card', 'is_active_member', 'origin', 'gender', 'Y'),
              as.numeric) %>%
    cor()

#Plotting the density plot in relation to the numerical variable
ggplot(df_long_num, aes(x = value, color = Y)) +
    geom_density() +
    facet_wrap(~ variable, nrow = 2, scales = 'free') +
    scale_color_manual(values = c('#00cd00', '#d80000')) +
    theme(text = element_text(size = 20),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

ggplot(df_long_cat, aes(x = value, fill = Y)) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    scale_fill_manual(values = c('#00cd00', '#d80000')) +
    facet_wrap(~ variable, scales = 'free') +
    theme(text = element_text(size = 20))

options(repr.plot.width = 16, repr.plot.height = 25, repr.plot.res = 100)

# Scatterplot for Age in relation to credit score, balance, and salary
sp1 <- ggplot(df, aes(x = age, y = credit_score, color = Y)) +
    geom_point(aes(size = 1.5, alpha = 1)) +
    scale_color_manual(values = c('#07ba52f3', '#bd0000')) +
    guides(size = 'none', alpha = 'none') +
    theme(text = element_text(size = 20))

sp2 <- ggplot(df, aes(x = age, y = balance, color = Y)) +
    geom_point(aes(size = 1.5, alpha = 1)) +
    scale_color_manual(values = c('#07ba52f3', '#bd0000')) +
    guides(size = 'none', alpha = 'none') +
    theme(text = element_text(size = 20))

sp3 <- ggplot(df, aes(x = age, y = salary, color = Y)) +
    geom_point(aes(size = 1.5, alpha = 1)) +
    scale_color_manual(values = c('#07ba52f3', '#bd0000')) +
    guides(size = 'none', alpha = 'none') +
    theme(text = element_text(size = 20))

sp4 <- ggplot(df, aes(x = credit_score, y = balance, color = Y)) +
    geom_point(aes(size = 1.5, alpha = 1)) +
    scale_color_manual(values = c('#07ba52f3', '#bd0000')) +
    guides(size = 'none', alpha = 'none') +
    theme(text = element_text(size = 20))

sp5 <- ggplot(df, aes(x = credit_score, y = salary, color = Y)) +
    geom_point(aes(size = 1.5, alpha = 1)) +
    scale_color_manual(values = c('#07ba52f3', '#bd0000')) +
    guides(size = 'none', alpha = 'none') +
    theme(text = element_text(size = 20))

grid.arrange(sp1, sp2, sp3, sp4, sp5, ncol= 1)

# Remove the ID column
df <- df %>%
    select(-ID)

# Set seed for the random number generators
set.seed(370)

# Create the synthetic data
df_syn <- df %>%
  smotenc(var = 'Y', k = 5)

# Check the labels in the new dataset
summary(df_syn$Y)

# Set seed for the random number generator
set.seed(370)

# Start fitting the model
# Use Gini impurity as the splitting criteria
tree <- rpart(Y ~ ., data = df_syn, parms = list(split = "gini"))

options(repr.plot.width = 20, repr.plot.height = 10, repr.plot.res = 100)

# Adjusting color transparency using the rgb() function
transparent_blue <- rgb(173, 216, 230, max = 255, alpha = 150)  # Light blue with transparency
transparent_green <- rgb(144, 238, 144, max = 255, alpha = 150)  # Light green with transparency

# Plot the resulting Decision Tree
rpart.plot(tree,
           extra = 102,    # Display Gini or Entropy values
           box.palette = c(transparent_blue, transparent_green),  # Transparent box colors
           branch.lty = 3,  # Dashed lines connecting boxes
           fallen.leaves = TRUE,  # Display leaves horizontally
           faclen = 0,   # Adjust the length of factor labels
           under = TRUE, # Display text under terminal nodes
           shadow.col = "gray",  # Color of shadow under boxes
           nn = TRUE,    # Display node numbers
           yesno = TRUE, # Display "yes" and "no" for split decisions
           cex = 1.2)    # Adjust text size

# Load the unlabeled dataset and pre-processed it like in training set
test <- read_csv('A2_customer_churn_submission.csv') %>%
    mutate(has_credit_card = factor(has_credit_card, levels = c(0,1), labels = c('No', 'Yes')),
           is_active_member = factor(is_active_member, levels = c(0,1), labels = c('No', 'Yes')),
           Y = factor(Y, levels = c(0,1), labels = c('Not Churn', 'Churn')),
           age = as.numeric(sub(".*\\bis an? (\\d+)-year-old.*", "\\1", customer_profile)),
           origin = factor(sub(".*\\bfrom ([A-Za-z]+).*", "\\1", customer_profile)),
           gender = factor(sub(".*\\b(male|female)\\b.*", "\\1", customer_profile), labels = c('Female', 'Male'))) %>%
    select(-Y, -customer_profile)

# Add the prediction result to the dataframe
test['TARGET'] = predict(tree, test, type = 'class')

# Save the result into a CSV file
test %>%
  select(ID, TARGET) %>%
  mutate(TARGET = ifelse(TARGET == 'Churn', 1, 0)) %>%
  write_csv('pred_labels.csv')
