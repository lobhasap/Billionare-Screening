# Load required libraries
library(tidyverse)
library(readxl)

# Choose the Excel file using a file chooser dialog
file_path <- file.choose()

# Read the chosen Excel file
data <- read_excel(file_path)

# a) Prepare a frequency distribution table for a quantitative variable and compute relative frequencies
freq_var <- "finalWorth" # Replace with your desired quantitative variable name
freq_table <- data %>% count(!!sym(freq_var)) %>% 
  mutate(relative_frequency = n / sum(n),
         cumulative_frequency = cumsum(relative_frequency))

# b) Create a histogram and dotplot of the data
# Histogram
ggplot(data, aes(x = !!sym(freq_var))) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = paste("Histogram for", freq_var))

# Dotplot
ggplot(data, aes(x = !!sym(freq_var))) +
  geom_dotplot(binwidth = 1, fill = "skyblue", alpha = 0.7) +
  labs(title = paste("Dotplot for", freq_var))

# c) Stacked dotplots of the variable based on a categorical variable (e.g., "category")
cat_var <- "category" # Replace with your desired categorical variable name
ggplot(data, aes(x = !!sym(freq_var), fill = !!sym(cat_var))) +
  geom_dotplot(binwidth = 1, stackdir = "center", alpha = 0.7) +
  labs(title = paste("Stacked Dotplot for", freq_var, "by", cat_var))

# d) Calculate summary measures
# For the entire dataset
summary(data[[freq_var]])

# For different groups formed by a categorical variable
data %>% group_by(!!sym(cat_var)) %>% summarize(
  mean = mean(!!sym(freq_var)),
  sd = sd(!!sym(freq_var)),
  Q1 = quantile(!!sym(freq_var), 0.25),
  Q3 = quantile(!!sym(freq_var), 0.75),
  IQR = IQR(!!sym(freq_var))
)

# e) Box-and-whisker plot
# For the entire dataset
ggplot(data, aes(x = "", y = !!sym(freq_var))) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = paste("Boxplot for", freq_var, "(Entire Dataset)"))

# Side-by-side boxplots for different groups (e.g., "category")
ggplot(data, aes(x = !!sym(cat_var), y = !!sym(freq_var), fill = !!sym(cat_var))) +
  geom_boxplot() +
  labs(title = paste("Boxplots for", freq_var, "by", cat_var))

