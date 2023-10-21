# Load the required libraries
library(ggplot2)
library(ggcorrplot)

# Set the working directory
setwd('C:/Users/soumi/Downloads')

# Read the dataset
data <- read.csv("summer_products.csv")

# Calculate the correlation coefficient between reviews and sales
correlation <- cor(data$rating, data$units_sold)
print(paste("Correlation coefficient:", correlation))

# Create a scatter plot to visualize the relationship between reviews and sales
ggplot(data, aes(x = rating, y = units_sold)) +
  geom_point() +
  labs(x = "Customer Reviews", y = "Product Sales") +
  ggtitle("Correlation between Customer Reviews and Product Sales")

# Create a correlation matrix plot
correlation_matrix <- cor(data[, c("rating", "units_sold")])
ggcorrplot(correlation_matrix, type = "full", title = "Correlation Matrix")

# Create a box plot to compare sales for different ratings
ggplot(data, aes(x = factor(rating), y = units_sold)) +
  geom_boxplot() +
  labs(x = "Rating", y = "Product Sales") +
  ggtitle("Comparison of Sales for Different Ratings")

# Create a density plot to compare the distribution of ratings for high and low sales
ggplot(data, aes(x = rating, fill = factor(units_sold > median(data$units_sold)))) +
  geom_density(alpha = 0.5) +
  labs(x = "Rating", y = "Density") +
  ggtitle("Distribution of Ratings for High and Low Sales") +
  scale_fill_discrete(name = "High Sales") +
  theme(legend.position = "top")

# Compare the mean sales for positive, negative, and no reviews
positive_reviews <- subset(data, rating >= 4)
negative_reviews <- subset(data, rating < 3)
no_review_products <- subset(data, rating_count == 0)

mean_positive_sales <- mean(positive_reviews$units_sold)
mean_negative_sales <- mean(negative_reviews$units_sold)
mean_no_review_sales <- mean(no_review_products$units_sold)

print(paste("Mean sales for products with positive reviews:", mean_positive_sales))
print(paste("Mean sales for products with negative reviews:", mean_negative_sales))
print(paste("Mean sales for products without reviews:", mean_no_review_sales))

# Create a bar plot to compare mean sales for positive, negative, and no reviews
mean_sales <- c(mean_positive_sales, mean_negative_sales, mean_no_review_sales)
review_categories <- c("Positive Reviews", "Negative Reviews", "No Reviews")
bar_colors <- c("green", "red", "gray")
barplot(mean_sales, names.arg = review_categories, col = bar_colors,
        xlab = "Review Status", ylab = "Mean Sales",
        main = "Comparison of Mean Sales: Positive, Negative, and No Reviews")



# Determine whether the correlation is positive, negative, or close to zero
if (correlation > 0) {
  print("Positive correlation: Customer reviews have a positive impact on product sales.")
} else if (correlation < 0) {
  print("Negative correlation: Customer reviews have a negative impact on product sales.")
} else {
  print("Correlation close to zero: There is little to no correlation between customer reviews and product sales.")
}

