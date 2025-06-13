# ---------------------------------------------------------
# Project: India Macroeconomic Analysis (IMF Data 2018–2024)
# Author: Shubh Bhaskar
# Data: GDP Growth, Inflation, Unemployment, Govt Expenditure (India)
# ---------------------------------------------------------
library(tidyverse)
library(ggcorrplot) 
data_raw <- read_csv("India_MacroData.csv")
glimpse(data_raw)
head(data_raw)    
data_clean <- data_raw %>%
  filter(!is.na(`Subject Descriptor`)) %>%                     
  select(`Subject Descriptor`, `2018`, `2019`, `2020`, 
         `2021`, `2022`, `2023`, `2024`) %>%
  pivot_longer(cols = starts_with("20"),                    
               names_to = "Year",
               values_to = "Value")

data_wide <- data_clean %>%
  pivot_wider(names_from = `Subject Descriptor`, values_from = Value)

colnames(data_wide) <- c("Year", "GDP_Growth", "Inflation", 
                         "Unemployment", "Govt_Expenditure")

data_wide$Year <- as.numeric(data_wide$Year)

print(data_wide)
# -------------------------------------------------------------
# ANALYSIS SECTION
# -------------------------------------------------------------
cor_matrix <- cor(data_wide %>% select(GDP_Growth, Inflation, 
                                       Unemployment, Govt_Expenditure),
                  use = "complete.obs")

print(cor_matrix) 

ggcorrplot(cor_matrix, lab = TRUE, colors = c("red", "white", "green")) +
  ggtitle("Correlation Heatmap: India Macro Variables")
# -------------------------------------------------------------
# 10. Regression Models (OLS)
# -------------------------------------------------------------
# Model 1: GDP Growth ~ Government Expenditure
model1 <- lm(GDP_Growth ~ Govt_Expenditure, data = data_wide)
summary(model1)

# Model 2: Inflation ~ Government Expenditure
model2 <- lm(Inflation ~ Govt_Expenditure, data = data_wide)
summary(model2)

# Model 3: Inflation ~ Unemployment (Phillips Curve)
model3 <- lm(Inflation ~ Unemployment, data = data_wide)
summary(model3)
# -------------------------------------------------------------
# 11. Visualizations
# -------------------------------------------------------------
# A. Govt Expenditure vs GDP Growth
ggplot(data_wide, aes(x = Govt_Expenditure, y = GDP_Growth)) +
  geom_point(color = 'blue', size = 3) +
  geom_smooth(method = 'lm', se = FALSE, color = 'darkred') +
  labs(title = "Government Expenditure vs GDP Growth",
       x = "Govt Expenditure (% of GDP)",
       y = "GDP Growth (Constant Prices, % Change)")

# B. Govt Expenditure vs Inflation
ggplot(data_wide, aes(x = Govt_Expenditure, y = Inflation)) +
  geom_point(color = 'darkgreen', size = 3) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  labs(title = "Government Expenditure vs Inflation",
       x = "Govt Expenditure (% of GDP)",
       y = "Inflation (CPI, % Change)")

# C. Phillips Curve: Inflation vs Unemployment
ggplot(data_wide, aes(x = Unemployment, y = Inflation)) +
  geom_point(color = 'purple', size = 3) +
  geom_smooth(method = 'lm', se = FALSE, color = 'orange') +
  labs(title = "Phillips Curve: Inflation vs Unemployment",
       x = "Unemployment Rate (%)",
       y = "Inflation (CPI, % Change)")

# Plot Macro Variables over Time
data_long <- data_wide %>%
  pivot_longer(cols = c(GDP_Growth, Inflation, Unemployment, Govt_Expenditure),
               names_to = "Variable", values_to = "Value")

ggplot(data_long, aes(x = Year, y = Value, color = Variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "India Macro Indicators Over Time (2018-2024)",
       y = "Value", x = "Year") +
  theme_minimal()