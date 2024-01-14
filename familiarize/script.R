##### a

a <- 4 * sin(pi)
b <- 3 * a

compare_variables <- function(a, b) {
  if (a > b) {
    return("Variable 'a' is greater.")
  } else if (b > a) {
    return("Variable 'b' is greater.")
  } else {
    return("Both variables are equal.")
  }
}

result <- compare_variables(a, b)
cat(result, "\n")


##### b
help_max <- help(max)
cat(help_max)



##### c
a <- 90:115
average_squared <- mean(a^2)
cat("Average of squared numbers:", average_squared, "\n")


##### d
functions_with_max <- ls("package:base", pattern = "max")
cat("Functions containing 'max' in their name:", functions_with_max, "\n")

##### e
# Set the working directory
setwd("/home/r-environment")

# Create and save a variable 'a' to a file
a <- "lodowka z najwieksza pojemnoscia"
save(a, file = "variable_a.Rdata")

# Delete the variable 'a'
rm(a)

# Check if variable 'a' exists (it should be deleted)
if (exists("a")) {
  cat("Variable 'a' exists.\n")
} else {
  cat("Variable 'a' does not exist.\n")
}

# Load the variable 'a' from the file
load("variable_a.Rdata")
cat("Loaded variable 'a':", a, "\n")

#### f
# Install and load the 'gridExtra' package
# Install the gridExtra package if not already installed
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}

# Load the gridExtra package
library(gridExtra)


# Find a function for visualizing data tables
?grid.table

#### g
vector_extraGrid <- seq(from = 1000, to = 850, by = -2)

#### h
# Create vector 'a' with numbers from 30 to 5
a <- 30:5

# Create vector 'b' with numbers from 11 to 23
b <- 11:23

# Concatenate vectors 'b' and 'a' to create vector 'd' (b first, then a)
d <- c(b, a)

# Display the resulting vector 'd'
print(d)

#### i
# Vector of refrigerator names
nazwa <- c("Fridge1", "Fridge2", "Fridge3", "Fridge4", "Fridge5", "Fridge6", "Fridge7", "Fridge8", "Fridge9", "Fridge10")

# Vector of user capacity for the fridge
pojemnośc_użytkowa_ch <- c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190)

# Vector of user capacity for the freezer
pojemnośc_użytkowa_zamrażarki <- c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75)

# Vector of prices
cena <- c(500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400)

# Vector of the number of opinions
liczba_opinii <- c(50, 60, 70, 80, 90, 100, 110, 120, 130, 140)

fridges <- data.frame(nazwa, pojemnośc_użytkowa_ch, pojemnośc_użytkowa_zamrażarki, cena, liczba_opinii)

# Display the data frame
print(fridges)

# Calculate the average price
average_price <- mean(fridges$cena)
cat("Average price of refrigerators:", average_price, "\n")

#### j 
# Data for the new refrigerator
new_entry <- data.frame(nazwa = "Fridge11",
                        pojemnośc_użytkowa_ch = 200,
                        pojemnośc_użytkowa_zamrażarki = 60,
                        cena = 1500,
                        liczba_opinii = 50)

# Add the new entry to the data frame
fridges <- rbind(fridges, new_entry)

# Calculate the updated average price
average_price_updated <- mean(fridges$cena)
cat("Updated average price of refrigerators:", average_price_updated, "\n")


#### k
# Create a vector of customer ratings from 0 to 5 with a step of 0.5
# Create a vector of customer ratings from 0 to 5 with a step of 0.5
customer_ratings <- seq(0, 5, by = 0.5)

# Add the 'customer_ratings' column to the 'fridges' data frame
fridges$customer_ratings <- cut(fridges$cena, breaks = c(-Inf, customer_ratings[2:length(customer_ratings)], Inf),
                                labels = customer_ratings, include.lowest = TRUE)

# Convert the 'customer_ratings' column to factors
fridges$customer_ratings <- as.factor(fridges$customer_ratings)

# Calculate the average price for each rating
average_price_by_rating <- tapply(fridges$cena, fridges$customer_ratings, mean)
print(average_price_by_rating)

#### l
# Data for new refrigerators
new_fridges <- data.frame(
  nazwa = c("Fridge11", "Fridge12", "Fridge13", "Fridge14"),
  pojemnośc_użytkowa_ch = c(200, 210, 220, 230),
  pojemnośc_użytkowa_zamrażarki = c(70, 75, 80, 85),
  cena = sample(500:1500, 4, replace = TRUE),
  liczba_opinii = c(50, 60, 70, 80),
  customer_ratings = as.factor(c(4.0, 3.5, 4.5, 3.0))  # Assign customer ratings
)

# Add the new refrigerators to the 'fridges' data frame
fridges <- rbind(fridges, new_fridges)


# Load the ggplot2 package for plotting
library(ggplot2)

# Create a bar plot
ggplot(fridges, aes(x = customer_ratings)) +
  geom_bar() +
  labs(title = "Count of Representatives for Each Customer Rating",
       x = "Customer Ratings",
       y = "Count")


#### m
# Calculate the percentage distribution of customer ratings
percentage_distribution <- prop.table(table(fridges$customer_ratings)) * 100

# Convert to a data frame
percentage_df <- data.frame(customer_ratings = as.numeric(names(percentage_distribution)),
                            percentage = as.numeric(percentage_distribution))

# Create a pie chart (wykres kołowy)
pie_chart <- ggplot(percentage_df, aes(x = "", y = percentage, fill = factor(customer_ratings))) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Percentage Distribution of Customer Ratings (Pie Chart)",
       x = NULL,
       y = NULL,
       fill = "Customer Ratings") +
  theme_void() +
  theme(legend.position = "right")

# Create a bar chart (wachlarzowy)
bar_chart <- ggplot(percentage_df, aes(x = factor(customer_ratings), y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Percentage Distribution of Customer Ratings (Bar Chart)",
       x = "Customer Ratings",
       y = "Percentage") +
  theme_minimal()

# Display both charts side by side
library(gridExtra)
grid.arrange(pie_chart, bar_chart, ncol = 2)

#### n
# Add a new column 'status_opinii' based on the number of opinions
fridges$status_opinii <- cut(fridges$liczba_opinii,
                             breaks = c(-Inf, 0, 50, 100, Inf),
                             labels = c("nie ma", "mniej 50 opinii", "50-100 opinii", "więcej 100 opinii"))

# Convert 'status_opinii' to a factor
fridges$status_opinii <- as.factor(fridges$status_opinii)

# Load the ggplot2 package for plotting
library(ggplot2)

# Calculate the percentage distribution of 'status_opinii'
percentage_distribution <- prop.table(table(fridges$status_opinii)) * 100

# Create a pie chart (wykres kołowy) to show the percentage distribution
pie_chart <- ggplot(data = fridges, aes(x = "", fill = status_opinii)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Percentage Distribution of Opinion Status (Pie Chart)",
       x = NULL,
       y = NULL,
       fill = "Status Opinii") +
  scale_fill_manual(values = c("nie ma" = "red", "mniej 50 opinii" = "yellow",
                               "50-100 opinii" = "green", "więcej 100 opinii" = "blue")) +
  theme_void() +
  theme(legend.position = "right")

# Display the pie chart
print(pie_chart)

#### o
# Initialize an empty vector to store the sentences
sentences <- character()

# Iterate through the 'fridges' data frame
for (i in 1:nrow(fridges)) {
  # Extract the relevant information
  nazwa <- fridges$nazwa[i]
  ocena_klientów <- fridges$customer_ratings[i]
  liczba_opinii <- fridges$liczba_opinii[i]
  
  # Create the sentence and append it to the vector
  sentence <- paste(nazwa, "ma ocenę klientów", ocena_klientów, "bo ma liczbę opinii", liczba_opinii)
  sentences <- append(sentences, sentence)
}

# Print the generated sentences
cat(sentences, sep = "\n")

#### p
# Save the 'fridges' data frame to a CSV file
write.csv(fridges, "lodowki_data.csv", row.names = FALSE)
# Load the 'fridges' data frame from the CSV file
fridges_loaded <- read.csv("lodowki_data.csv")

# Print the loaded data frame
print(fridges_loaded)
