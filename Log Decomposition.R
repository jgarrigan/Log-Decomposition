pacman::p_load(tidyverse, readxl, skimr)

data <- read_excel("Adidas US Sales Datasets.xlsx", skip = 4)


# EDA ---------------------------------------------------------------------

skimr::skim(data)

# WHO ARE THE TOP RETAILERS
data %>%
  group_by(Retailer) %>%
  summarise(total_sales = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  mutate(Retailer = fct_reorder(Retailer, desc(total_sales))) %>%
  ggplot(aes(x = Retailer, y = total_sales, fill = Retailer)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "none") +
  labs(
    title = "Total Sales by Retailer",
    x = "Retailer",
    y = "Total Sales"
  )

# TREND OF TOTAL SALES OVER TIME
data %>%
  group_by(`Invoice Date`) %>%
  summarise(total_sales = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = `Invoice Date`, y = total_sales)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "none") +
  labs(
    title = "Total Sales by Date",
    x = "Invoice Date",
    y = "Total Sales"
  ) +
  theme_bw()

# WHICH REGIONS HAVE THE HIGEST TOTAL SALES
data %>%
  group_by(Region) %>%
  summarise(total_sales = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = fct_reorder(Region, desc(total_sales))) %>%
  ggplot(aes(x = Region, y = total_sales, fill = Region)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Total Sales by Region",
    x = "Region",
    y = "Total Sales"
  )

# TOTAL SALES BY PRODUCT
data %>%
  group_by(Product) %>%
  summarise(total_sales = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  mutate(Product = fct_reorder(Product, desc(total_sales))) %>%
  ggplot(aes(x = Product, y = total_sales, fill = Product)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Total Sales by Product",
    x = "Product",
    y = "Total Sales"
  )

# PREPARE DATA FOR DECOMPOSITION
log_decomp <- data %>%
  mutate(Period = case_when(
    `Invoice Date` >= "2020-01-01" & `Invoice Date` <= "2020-12-31" ~ "Pre",
    `Invoice Date` >= "2021-01-01" ~ "Post"
  )) %>%
  group_by(
    Retailer,
    Region,
    State,
    City,
    Product,
    `Sales Method`,
    Period
  ) %>%
  summarise(
    total_sales = sum(`Total Sales`, na.rm = TRUE),
    total_units = sum(`Units Sold`, na.rm = TRUE), .groups = "drop"
  ) %>%
  mutate(price_per_unit = total_sales / total_units) %>%
  group_by(Period) %>%
  mutate(total_sales = sum(total_sales, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(cols = c(total_units, price_per_unit), names_to = "metric", values_to = "values") %>%
  pivot_wider(id_cols = c(Retailer, Region, State, City, Product, `Sales Method`, metric), names_from = Period, values_from = c(total_sales, values)) %>%
  mutate(
    across(`values_Pre`, ~ ifelse(metric == "total_units" & is.na(.x), 1E-6, .x)),
    across(`values_Post`, ~ ifelse(metric == "total_units" & is.na(.x), 1E-6, .x))
  ) %>%
  mutate(
    across(`values_Pre`, ~ ifelse(metric == "price_per_unit" & is.na(.x), values_Post, .x)),
    across(`values_Post`, ~ ifelse(metric == "price_per_unit" & is.na(.x), values_Pre, .x))
  ) %>%
  mutate(across(c(total_sales_Pre, total_sales_Post), ~ ifelse(is.na(.), 1E-6, .))) %>%
  group_by(across(Retailer:`Sales Method`)) %>%
  mutate(
    recalc.pre = prod(values_Pre),
    recalc.post = prod(values_Post)
  ) %>%
  ungroup() %>%
  mutate(IMPACT = (recalc.post - recalc.pre) * (log(values_Post / values_Pre) / log(recalc.post / recalc.pre)))

# ANALYZE DECOMPOSITION RESULTS
log_decomp %>%
  filter(metric == "total_units") %>%
  summarise(across(starts_with("recalc."), sum)) %>%
  mutate(diff = recalc.post - recalc.pre)

log_decomp %>%
  group_by(metric) %>%
  summarise(
    Impact = sum(IMPACT, na.rm = TRUE)
  )

# VISUALIZE SALES TREND FOR A SPECIFIC PRODUCT
data %>%
  filter(
    Retailer == "Foot Locker",
    City == "Charleston",
    Product == "Men's Street Footwear",
    `Sales Method` == "Online"
  ) %>%
  ggplot(aes(x = `Invoice Date`, y = `Total Sales`)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Foot Locker - Men\'s Street Footwear - Online Sales - Charlestown",
    x = "Invoice Date",
    y = "Total Sales"
  ) +
  theme_bw()

# ALTERNATIVE DATA PREPARATION FOR ANALYSIS
test <- data %>%
  mutate(Period = case_when(
    `Invoice Date` >= "2020-01-01" & `Invoice Date` <= "2020-12-31" ~ "Pre",
    `Invoice Date` >= "2021-01-01" ~ "Post"
  )) %>%
  group_by(
    Retailer,
    Region,
    State,
    City,
    Product,
    `Sales Method`,
    Period
  ) %>%
  summarise(total_sales = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Period, values_from = total_sales, values_fill = 0) %>%
  mutate(difference = Post - Pre)