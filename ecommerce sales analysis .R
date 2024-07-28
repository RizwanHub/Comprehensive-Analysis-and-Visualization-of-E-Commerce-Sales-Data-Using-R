# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the data
data <- read_excel("E:\\Downloads(hard disk)\\ecommerce sales data for r data analysis.xlsx")

# Convert Order Date to Date type
data$`Order Date` <- as.Date(data$`Order Date`)

# Ensure Quantity and Price are numeric
data$Quantity <- as.numeric(data$Quantity)
data$Price <- as.numeric(data$Price)

# Create a new column for Total Sales
data <- data %>%
  mutate(Total_Sales = Quantity * Price)

# 1. Total Sales by Category (Bar Chart)
category_sales <- data %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Total_Sales))

ggplot(category_sales, aes(x = Category, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Total Sales by Category", x = "Category", y = "Total Sales")

# 2. Daily Sales Over Time (Line Chart)
daily_sales <- data %>%
  group_by(`Order Date`) %>%
  summarise(Daily_Sales = sum(Total_Sales))

ggplot(daily_sales, aes(x = `Order Date`, y = Daily_Sales)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Daily Sales Over Time", x = "Date", y = "Daily Sales")

# 3. Payment Method Distribution (Pie Chart)
payment_summary <- data %>%
  group_by(`Payment Method`) %>%
  summarise(Total_Sales = sum(Total_Sales))

ggplot(payment_summary, aes(x = "", y = Total_Sales, fill = `Payment Method`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Payment Method Distribution")

# 4. Total Quantity Sold by Category (Bar Chart)
category_quantity <- data %>%
  group_by(Category) %>%
  summarise(Total_Quantity = sum(Quantity))

ggplot(category_quantity, aes(x = Category, y = Total_Quantity)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Total Quantity Sold by Category", x = "Category", y = "Total Quantity")

# 5. Average Order Value by Category (Bar Chart)
category_avg_order <- data %>%
  group_by(Category) %>%
  summarise(Average_Order_Value = mean(Total_Sales))

ggplot(category_avg_order, aes(x = Category, y = Average_Order_Value)) +
  geom_bar(stat = "identity", fill = "purple") +
  theme_minimal() +
  labs(title = "Average Order Value by Category", x = "Category", y = "Average Order Value")

# 6. Total Sales by Customer Location (Bar Chart)
location_sales <- data %>%
  group_by(`Customer Location`) %>%
  summarise(Total_Sales = sum(Total_Sales))

ggplot(location_sales, aes(x = `Customer Location`, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_minimal() +
  labs(title = "Total Sales by Customer Location", x = "Location", y = "Total Sales")

# 7. Total Sales by Payment Method (Bar Chart)
payment_sales <- data %>%
  group_by(`Payment Method`) %>%
  summarise(Total_Sales = sum(Total_Sales))

ggplot(payment_sales, aes(x = `Payment Method`, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "cyan") +
  theme_minimal() +
  labs(title = "Total Sales by Payment Method", x = "Payment Method", y = "Total Sales")

# 8. Monthly Sales Trend (Line Chart)
monthly_sales <- data %>%
  mutate(Month = floor_date(`Order Date`, "month")) %>%
  group_by(Month) %>%
  summarise(Monthly_Sales = sum(Total_Sales))

ggplot(monthly_sales, aes(x = Month, y = Monthly_Sales)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(title = "Monthly Sales Trend", x = "Month", y = "Monthly Sales")

# 9. Total Sales by Product (Bar Chart)
product_sales <- data %>%
  group_by(`Product Name`) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  arrange(desc(Total_Sales)) %>%
  head(10) # Top 10 products

ggplot(product_sales, aes(x = reorder(`Product Name`, Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "pink") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top 10 Products by Total Sales", x = "Product Name", y = "Total Sales")

# 10. Quantity Sold by Product (Bar Chart)
product_quantity <- data %>%
  group_by(`Product Name`) %>%
  summarise(Total_Quantity = sum(Quantity)) %>%
  arrange(desc(Total_Quantity)) %>%
  head(10) # Top 10 products

ggplot(product_quantity, aes(x = reorder(`Product Name`, Total_Quantity), y = Total_Quantity)) +
  geom_bar(stat = "identity", fill = "gold") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top 10 Products by Quantity Sold", x = "Product Name", y = "Total Quantity")
