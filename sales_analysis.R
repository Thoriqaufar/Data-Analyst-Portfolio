# Library that we need
library(tidyverse)
library(janitor)
library(readxl)

# Data Loading
sales_data = read_excel("C:/Users/thori/Desktop/Bismillah Kerja/Portofolio/RStudio/Portofolio_Data Analyst_1/walmart Retail Data.xlsx")

# Data Overview
sales_data = sales_data %>% 
  clean_names()

glimpse(sales_data)

# Data Transform
sum(is.na(sales_data))

sales_data = sales_data %>% 
  drop_na()

sales_data = sales_data %>% 
  mutate(month = month(order_date, label = TRUE),
         year = year(order_date))

# Data Processing
monthly_sales = sales_data %>% 
  group_by(year, month) %>% 
  summarise(total_sales = sum(order_quantity))

peak_sales_period = monthly_sales %>% 
  arrange(desc(total_sales)) %>% 
  head(1)

category_sales = sales_data %>% 
  group_by(product_sub_category) %>% 
  summarise(total_sales = sum(order_quantity)) %>% 
  arrange(desc(total_sales))

top_profit_sales_by_category = sales_data %>% 
  group_by(product_sub_category) %>% 
  summarise(total_profit = sum(profit)) %>% 
  arrange(desc(total_profit))

top_10_sales_by_state = sales_data %>% 
  group_by(state) %>% 
  summarise(total_sales = sum(order_quantity)) %>% 
  arrange(desc(total_sales)) %>% 
  head(10)

# Data Viz
ggplot(monthly_sales, aes(x = month, y = total_sales, group = year, color = factor(year))) +
  geom_line() +
  labs(title = "Monthly Sales Over Years", x = "Month", y = "Sales") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(category_sales, aes(x = reorder(product_sub_category, total_sales), y = total_sales, fill = product_sub_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sales by Category", x = "Category", y = "Total Sales") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = total_sales), vjust = 0.3, hjust = 1.2)

ggplot(top_profit_sales_by_category, aes(x = reorder(product_sub_category, total_profit), y = total_profit, fill = product_sub_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Profit by Category", x = "Category", y = "Total Sales") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(top_10_sales_by_state, aes(x = reorder(state, total_sales), y = total_sales, fill = state)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Sales by State", x = "State", y = "Total Sales") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = total_sales), vjust = 0.3, hjust = 1.2)