
rm(list = ls())

print("sujit Kumar")
# Install required packages 
install.packages("gtsummary")
install.packages("psych")
install.packages("MASS")
install.packages("gt")
install.packages("flextable")
install.packages("report")



library(gtsummary)
library(psych)
library(readxl)
library(gt)
library(flextable)
library(report)

data <- read_excel("clean_data/AMR_KAP_Data.xlsx",sheet=2)

View(data)
dim(data)
data |>
  select(1:11) |>
  tbl_summary() |>
  as_gt() |>
  gtsave("table/Table1_Demographic.docx")



columnames=as.data.frame(colnames(data))
View(columnames)

length(data$`PCT Knowledge`)
data$knowledge_level <- cut(data$`PCT Knowledge`,
                            breaks = c(-Inf, 39, 59, Inf),
                            labels = c("Poor", "Moderate", "Good"),
                            right = FALSE)


table(data$knowledge_level)


#Calculate the counts of knowledge levels
knowledge <- table(data$knowledge_level)

# Calculate the percentage of each knowledge level
knowledge_percent <- prop.table(knowledge) * 100

# Combine counts and percentages
knowledge_table <- as.data.frame(cbind(frequency=knowledge, percent = round(knowledge_percent, 2)))


# Categorize attitude
data$attitude_level <- cut(data$`PCT Attitude`,
                           breaks = c(-Inf, 49, 79, Inf),
                           labels = c("Negative","Uncertain", "Positive"),
                           right = TRUE)



#Calculate the counts of attitude levels
attitude <- table(data$attitude_level)

# Calculate the percentage of each attitude level
attitude_percent <- prop.table(attitude) * 100

# Combine counts and percentages
attitude_table <- as.data.frame(cbind(frequency=attitude, percent = round(attitude_percent, 2)))


# Categorize practice
data$practice_level <- cut(data$`PCT Practice`,
                           breaks = c(-Inf, 79, Inf),
                           labels = c("Misuse", "Good"),
                           right = TRUE)
table(data$practice_level)

practice <- table(data$practice_level)

practice_percent <- prop.table(practice) * 100

practice_table <- as.data.frame(cbind(frequency=practice, percent = round(practice_percent, 2)))



colnames(data)

###add Characteristic 
knowledge_table$Characteristic=rownames(knowledge_table)
attitude_table$Characteristic=rownames(attitude_table)
practice_table$Characteristic=rownames(practice_table)

knowledge_table=knowledge_table[,c(3,1,2)]
attitude_table=attitude_table[,c(3,1,2)]
practice_table=practice_table[,c(3,1,2)]


save_as_docx(flextable(knowledge_table), path = "table/knowledge_table.docx")
save_as_docx(flextable(attitude_table), path = "table/attitude_table.docx")
save_as_docx(flextable(practice_table), path = "table/practice_table.docx")


# Convert all independent variables to factors
data$`Parent’s age (years)` <- factor(data$`Parent’s age (years)`)
data$`Parent’s sex` <- factor(data$`Parent’s sex`)
data$`Parent’s education level` <- factor(data$`Parent’s education level`)
data$`Employment status` <- factor(data$`Employment status`)
data$`Family type` <- factor(data$`Family type`)
data$`Your average household income per month (BDT)` <- factor(data$`Your average household income per month (BDT)`)
data$`Child’s sex` <- factor(data$`Child’s sex`)
data$`Child’s age (years)` <- factor(data$`Child’s age (years)`)
data$`Number of children` <- factor(data$`Number of children`)


# Ordinal Logistic Regression for Knowledge Level
colnames(data)
datak <- data[, c(1:9,46)]  # Subset data for knowledge level
colnames(datak)


str(datak)

# Ensure necessary packages are loaded
library(gtsummary)
library(MASS)
library(gt)



# Factor the knowledge level variable
datak$knowledge_level <- factor(datak$knowledge_level, levels = c("Poor", "Moderate", "Good"), ordered = TRUE)

# Fit ordinal regression model for Knowledge Level
tbl_uvreg_practices1 <- 
  datak |>
  tbl_uvregression(
    method = polr,  # Use polr for ordinal logistic regression
    y = knowledge_level,  # Response variable (ordinal)
    exponentiate = TRUE  # Show odds ratios
  ) |>
  add_global_p() |>
  bold_p(t = 0.10) |>
  bold_labels() |>
  italicize_levels()|>
  as_gt() |>
  gtsave("table/Table2_ordinal_regression attitude_model.docx")


# Ordinal Logistic Regression for Attitude Level

colnames(data)
dataa <- data[, c(1:9,47)]  # Subset data for attitude level

# Check distribution of attitude level
table(dataa$attitude_level)

# Factor the attitude level variable
dataa$attitude_level <- factor(dataa$attitude_level, levels = c("Negative", "Uncertain", "Positive"), ordered = TRUE)


# Fit ordinal regression model for Attitude Level
tbl_uvreg_practices2 <- 
  dataa |>
  tbl_uvregression(
    method = polr,  # Use polr for ordinal logistic regression
    y = attitude_level,  # Response variable (ordinal)
    exponentiate = TRUE, # Show odds ratios
    pvalue_fun = label_style_pvalue(digits = 2),
  ) |>
  add_global_p() |>
  bold_p(t = 0.10) |>
  bold_labels() |>
  italicize_levels()|>
  as_gt() |>
  gtsave("table/Table3_attitude_ordinal_regression.docx")  # Save the output

# Binomial Logistic Regression for Practice Status

# Convert "Misuse" = 0 and "Good" = 1 in the practice_level factor
data$practice_status <- factor(data$practice_level, 
                               levels = c("Misuse", "Good"), 
                               labels = c(0, 1))

# Subset the dataframe 'data' for logistic regression
data1 <- data[, c(1:9,46,47,49)]


# Fit univariate logistic regression models for each predictor
tbl_uvreg_practices3 <- 
  data1 |>
  tbl_uvregression(
    method = glm,  # Use glm for logistic regression
    y = practice_status,  # Response variable (binary)
    method.args = list(family = binomial(link = "logit")),  # Binomial logistic regression
    exponentiate = TRUE  # Show odds ratios
  ) |>
  as_gt() |>
  gtsave("table/table4_logistic_regression4.docx")  # Save the output


#################All Figure################################


#Distribution of knowledge of antibiotic resistance among parents of school-going children (N=704)

figdata=read_excel("clean_data/AMR_KAP_Data.xlsx",sheet=1)
colnames(figdata)
figda1=figdata[,12:23]
colnames(figda1)

# Load necessary libraries
library(tidyverse)

# Reshape the data into long format
data_long <- figda1 %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")


# Calculate proportions for each response within each question
data_summary <- data_long %>%
  group_by(Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


# Create the stacked bar plot with percentages
ggplot(data_summary, aes(x = Question, y = percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 3) + # Add percentages to the bars
  coord_flip() +
  labs(title = "Antibiotic Knowledge Survey Responses (with Percentages)",
       x = "Question",
       y = "Percentage of Responses") +
  scale_fill_manual(values = c("Yes" = "Dark Cyan", "No" = "White Smoke", "Don't Know" = "Wheat")) +
  theme_minimal()
ggsave("figures/Distribution of knowledge of antibiotic resistance.jpeg", width = 12, height = 10)


#Attitude towards antibiotic resistance and the misuse of antibiotics among parents of school-going children (N=704)
figda2=figdata[,24:33]
colnames(figdata)


# Load necessary libraries
library(tidyverse)

# Reshape the data into long format
data_long1 <- figda2 %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")


# Calculate proportions for each response within each question
data_summary1 <- data_long1 %>%
  group_by(Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


# Create the stacked bar plot with percentages
ggplot(data_summary1, aes(x = Question, y = percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 3) + # Add percentages to the bars
  coord_flip() +
  labs(title = "Antibiotic Attitude Survey Responses (with Percentages)",
       x = "Question",
       y = "Percentage of Responses") +
  scale_fill_manual(values = c("Neutral" = "Dark Cyan", "Disagree" = "White Smoke", "Agree" = "Wheat")) +
  theme_minimal()
ggsave("figures//Attitude towards antibiotic resistance_percentages_plot.jpeg", width = 12, height = 8)

#Practices among parents of school-going children regarding antibiotic resistance (N=704)
colnames(figdata)

figda3=figdata[,34:39]

# Load necessary libraries
library(tidyverse)

# Reshape the data into long format
data_long3 <- figda3 %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")


# Calculate proportions for each response within each question
data_summary3 <- data_long3 %>%
  group_by(Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


# Create the stacked bar plot with percentages
ggplot(data_summary3, aes(x = Question, y = percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 3) + # Add percentages to the bars
  coord_flip() +
  labs(title = "Practices among parents of school-going children regarding antibiotic resistance",
       x = "Question",
       y = "Percentage of Responses") +
  scale_fill_manual(values = c("Yes" = "Dark Cyan","No" = "Wheat")) +
  theme_minimal()


ggsave("figures//Practices among parents of school-going children regarding antibiotic resistance.jpeg", width = 12, height = 8)



