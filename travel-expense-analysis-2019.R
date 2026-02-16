# =====================================================
# BUSINESS TRAVEL EXPENSE ANALYSIS - 2019
# Data Source: Brazilian Transparency Portal
# =====================================================

# -----------------------------------------------------
# 1. Problem Definition
# -----------------------------------------------------

# Objective:
# Analyze government travel expenses in order to identify
# spending patterns and generate insights for decision-making.

# Business Questions:
# 1. What is the total amount spent per government department?
# 2. What is the total amount spent per city?
# 3. How many trips occurred per month?

# -----------------------------------------------------
# 2. Data Acquisition
# -----------------------------------------------------

# Loading dataset obtained from the official Transparency Portal

travel <- read.csv(
  "C:/Users/Luiza/Downloads/2019_Viagem.csv",
  sep = ";",
  dec = ","
)
travel <- read.csv(
  "C:/Users/Luiza/Downloads/2019_Viagem.csv",
  sep = ";",
  dec = ",",
  fileEncoding = "latin1"
)  
head (travel)
View (travel)
dim (travel)
summary(travel)
summary(travel$Valor.passagens)

install.packages("dplyr")
library(dplyr)
glimpse(travel)


# -----------------------------------------------------
# 3. Data Transformation
# -----------------------------------------------------

travel$data.inicio <- as.Date(travel$Período...Data.de.início, "%d/%m/%Y")

glimpse(travel)

travel$data.inicio.formatada <- format(travel$data.inicio, "%Y-%m")
travel$data.inicio.formatada

# -----------------------------------------------------
# 4. Exploratory Data Analysis (EDA)
# -----------------------------------------------------

# Histogram of ticket expenses
hist(travel$Valor.passagens)

# Filtering ticket expenses between 200 and 5000
passagens_filtro <- travel %>%
  dplyr::select(Valor.passagens) %>%
  dplyr::filter(Valor.passagens >= 200 & Valor.passagens <= 5000)

# Histogram after filtering extreme values
hist(passagens_filtro$Valor.passagens)

# Summary statistics
summary(travel$Valor.passagens)

# Boxplot visualization
boxplot(travel$Valor.passagens)
boxplot(passagens_filtro$Valor.passagens)

# Standard deviation
sd(travel$Valor.passagens)

# Checking missing values
colSums(is.na(travel))

# Converting categorical variable to factor
travel$Situação <- factor(travel$Situação)

# Category distribution
table(travel$Situação)
prop.table(table(travel$Situação)) * 100

# -----------------------------------------------------
# 5. Results Visualization
# -----------------------------------------------------

# -----------------------------------------------------
# 5.1 Total Ticket Expenses by Government Department
# -----------------------------------------------------

expenses_by_department <- travel %>%
  group_by(Nome.do.órgão.superior) %>%
  summarise(total_expense = sum(Valor.passagens, na.rm = TRUE)) %>%
  arrange(desc(total_expense))

names(expenses_by_department) <- c("department", "total_expense")

expenses_by_department

top15_departments <- expenses_by_department %>%
  slice_head(n = 15)

ggplot(top15_departments, 
       aes(x = reorder(department, total_expense), 
           y = total_expense)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 15 Departments by Ticket Expenses",
    x = "Department",
    y = "Total Expense"
  )

# -----------------------------------------------------
# 5.2 Total Ticket Expenses by Destination City
# -----------------------------------------------------

expenses_by_city <- travel %>%
  group_by(Destinos) %>%
  summarise(total_expense = sum(Valor.passagens, na.rm = TRUE)) %>%
  arrange(desc(total_expense))

names(expenses_by_city) <- c("destination", "total_expense")

expenses_by_city

top15_cities <- expenses_by_city %>%
  slice_head(n = 15)

ggplot(top15_cities, 
       aes(x = reorder(destination, total_expense), 
           y = total_expense)) +
  geom_bar(stat = "identity", fill = "#0ba791") +
  coord_flip() +
  labs(
    title = "Top 15 Destination Cities by Ticket Expenses",
    x = "Destination",
    y = "Total Expense"
  )

# -----------------------------------------------------
# 5.3 Number of Trips per Month
# -----------------------------------------------------

trips_per_month <- travel %>%
  group_by(data.inicio.formatada) %>%
  summarise(trip_count = n_distinct(Identificador.do.processo.de.viagem)) %>%
  arrange(data.inicio.formatada)

trips_per_month

ggplot(trips_per_month,
       aes(x = data.inicio.formatada,
           y = trip_count,
           group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Trips per Month",
    x = "Year-Month",
    y = "Number of Trips"
  )

# -----------------------------------------------------
# 6. Key Insights and Conclusions
# -----------------------------------------------------

---
  title: "Government Travel Expense Analysis - 2019"
author: "Luiza"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
library(dplyr)
library(ggplot2)
```

# 1. Introduction

This report analyzes government travel expenses for the year 2019 using data obtained from the Brazilian Transparency Portal.

# 2. Data Acquisition

```{r}
travel <- read.csv(
  "C:/Users/Luiza/Downloads/2019_Viagem.csv",
  sep = ";",
  dec = ",",
  fileEncoding = "latin1"
)

dim(travel)
```

# 3. Data Transformation

```{r}
travel$data.inicio <- as.Date(travel$Período...Data.de.início, "%d/%m/%Y")
travel$data.inicio.formatada <- format(travel$data.inicio, "%Y-%m")
```

# 4. Exploratory Data Analysis

```{r, echo=FALSE}
hist(travel$Valor.passagens)
```

```{r}
summary(travel$Valor.passagens)
```

# 5. Results

## 5.1 Total Ticket Expenses by Government Department

```{r}
expenses_by_department <- travel %>%
  group_by(Nome.do.órgão.superior) %>%
  summarise(total_expense = sum(Valor.passagens, na.rm = TRUE)) %>%
  arrange(desc(total_expense))

names(expenses_by_department) <- c("department", "total_expense")

head(expenses_by_department, 15)
```

```{r, echo=FALSE}
top15_departments <- expenses_by_department %>%
  slice_head(n = 15)

ggplot(top15_departments, 
       aes(x = reorder(department, total_expense), 
           y = total_expense)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 15 Departments by Ticket Expenses",
    x = "Department",
    y = "Total Expense"
  )
```

---
  
  ## 5.2 Total Ticket Expenses by Destination City
  
  ```{r}
expenses_by_city <- travel %>%
  group_by(Destinos) %>%
  summarise(total_expense = sum(Valor.passagens, na.rm = TRUE)) %>%
  arrange(desc(total_expense))

names(expenses_by_city) <- c("destination", "total_expense")

head(expenses_by_city, 15)
```

```{r, echo=FALSE}
top15_cities <- expenses_by_city %>%
  slice_head(n = 15)

ggplot(top15_cities, 
       aes(x = reorder(destination, total_expense), 
           y = total_expense)) +
  geom_bar(stat = "identity", fill = "#0ba791") +
  coord_flip() +
  labs(
    title = "Top 15 Destination Cities by Ticket Expenses",
    x = "Destination",
    y = "Total Expense"
  )
```

---
  
  ## 5.3 Number of Trips per Month
  
  ```{r}
trips_per_month <- travel %>%
  group_by(data.inicio.formatada) %>%
  summarise(trip_count = n_distinct(Identificador.do.processo.de.viagem)) %>%
  arrange(data.inicio.formatada)

trips_per_month
```

```{r, echo=FALSE}
ggplot(trips_per_month,
       aes(x = data.inicio.formatada,
           y = trip_count,
           group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Trips per Month",
    x = "Year-Month",
    y = "Number of Trips"
  )
```

# 6. Key Insights

- The dataset contains 756,704 travel records, of which 97.7% were completed trips.
- The Ministry of Education was the highest-spending department.
- Brasília/DF was the most expensive destination.
- November recorded the highest travel volume, while January had the lowest.
- The data reveals strong spending concentration and seasonal variation patterns.
