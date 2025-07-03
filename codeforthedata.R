library(tidyverse)

data <- read.csv("C:/Users/melpk/OneDrive/Υπολογιστής/data/data.csv")

table <- table(data$Price, data$group)

table

chisq.test(table)

table_prothesi <- table(data$Prothesi, data$group)

table_prothesi

chisq.test(table_prothesi)

rownames(table) <- factor(rownames(table), levels = c("5 - 15", "15 - 35", "35 - 60"))

# Convert to long format without creating a new data frame manually
ggplot(as.data.frame.table(table), aes(x = factor(Var1, levels = c("5 - 15", "15 - 35", "35 - 60")),
                                       y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Price Distribution by Group",
       x = "Price Range",
       y = "Count",
       fill = "Group") +
  theme_minimal() +
  scale_fill_manual(values = c("Control" = "purple",  # blue
                               "High" = "blue",     # orange
                               "Low" = "red"))     # green

table(data$group)

ggplot(data, aes(x = group, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution by Group",
       x = "Group",
       y = "Count",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Man" = "#1f77b4", "Woman" = "#ff69b4"))

ggplot(data, aes(x = group, y = Age, fill = Gender)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(title = "Age Distribution by Group and Gender",
       x = "Group",
       y = "Age",
       fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Man" = "#1f77b4", "Woman" = "#ff69b4"))

ggplot(data, aes(x = group, y = Age, fill = group)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(title = "Age Distribution by Group and Gender",
       x = "Group",
       y = "Age") +
  theme_minimal() +
  scale_fill_manual(values = c("Control" = "purple",
                               "High" = "blue",     
                               "Low" = "red"))     


# Plot directly from the table, setting factor order for Prothesi (Var1)
ggplot(as.data.frame.table(table_prothesi), 
       aes(x = factor(Var1, levels = c("Not at all", "A bit", "Somewhat", "A lot")),
           y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prothesi by Group",
       x = "Prothesi (Intention)",
       y = "Count",
       fill = "Group") +
  theme_minimal() +
  scale_fill_manual(values = c("Control" = "purple",
                               "High" = "blue",     
                               "Low" = "red"))  

ggplot(as.data.frame.table(table_prothesi),
       aes(x = factor(Var1, levels = c("Not at all", "A bit", "Somewhat", "A lot")),
           y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(title = "Prothesi by Group (Stacked)",
       x = "Prothesi (Intention)",
       y = "Count",
       fill = "Group") +
  theme_minimal() +
  scale_fill_manual(values = c("Control" = "purple",
                               "High" = "blue",     
                               "Low" = "red"))  

glimpse(data)

# Make sure 'Price' is an ordered factor
data$Price <- factor(data$Price, levels = c("5 - 15", "15 - 35", "35 - 60"), ordered = TRUE)
data$Prothesi <- factor(data$Prothesi, 
                        levels = c("Not at all", "A bit", "Somewhat", "A lot"), 
                        ordered = TRUE)
data$group <- as.factor(data$group)
data$Price_num <- as.numeric(data$Price)
data$Prothesi_num <- as.numeric(data$Prothesi)

model <- lm(Price_num ~ group + Age + Gender + Income, data = data)
model_prothesi <- lm(Prothesi_num ~ group + Age + Gender + Income, data = data)

summary(model)
summary(model_prothesi)
#individuals of groupHigh, had a lower intention to buy of 1.33 

