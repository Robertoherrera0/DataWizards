setwd("C:/Users/eduar/OneDrive/Documents/datafest")

items <- read.csv('items_sample.csv')
responses <- read.csv('responses_sample.csv')
pages <- read.csv('page_views_sample.csv')

#==================================================================================================================
# DATA CLEANING AND FEATURE ENGINEERING

responses$success_rate <- with(responses, points_earned / points_possible)
responses$attempt_rate <- with(responses, attempt)

library(dplyr)
library(tidyr)

student_performance <- responses %>%
  group_by(student_id) %>%
  summarise(
    avg_success_rate = mean(success_rate, na.rm = TRUE),
    total_attempts = sum(as.numeric(attempt), na.rm = TRUE),
    avg_attempts = mean(attempt, na.rm = TRUE),
    completed_pages = sum(as.numeric(completes_page), na.rm = TRUE), 
    review_flag_proportion = mean(as.numeric(review_flag), na.rm = TRUE) 
  ) %>%
  mutate(
    is_struggling = ifelse(avg_success_rate <= 0.65, 1, 0)
  )


student_performance$is_struggling <- as.factor(student_performance$is_struggling)

pages_summary <- pages %>%
  group_by(student_id) %>%
  summarise(
    avg_engaged = mean(engaged, na.rm = TRUE),
    proportion_complete = mean(as.numeric(was_complete), na.rm = TRUE),
    avg_idle_brief = mean(idle_brief, na.rm = TRUE),
    avg_idle_long = mean(idle_long, na.rm = TRUE),
    review_flag_rate = mean(as.numeric(review_flag), na.rm = TRUE)
  )

enhanced_student_performance <- student_performance %>%
  left_join(pages_summary, by = "student_id")

enhanced_student_performance <- na.omit(enhanced_student_performance)

for (col in names(enhanced_student_performance)) {
  if (is.numeric(enhanced_student_performance[[col]])) {
    enhanced_student_performance[[col]][is.na(enhanced_student_performance[[col]])] <- 
      mean(enhanced_student_performance[[col]], na.rm = TRUE)
  }
}
for (col in names(enhanced_student_performance)) {
  if (is.factor(enhanced_student_performance[[col]])) {
    mode_value <- names(which.max(table(enhanced_student_performance[[col]])))
    enhanced_student_performance[[col]][is.na(enhanced_student_performance[[col]])] <- mode_value
  }
}

enhanced_student_performance$avg_success_rate <- NULL
enhanced_student_performance$student_id <- NULL

#===============================================================================================
#SCATTERPLOT MATRIX

pairs(enhanced_student_performance[,-5], 
      col = c("blue", 'red')[enhanced_student_performance$is_struggling],
      main = "Scatterplot Matrix by Struggling Student")

legend("topright", 
       inset = c(-0.1, -0.1),  
       legend = levels(enhanced_student_performance$is_struggling),
       fill = c("blue", "red"),
       title = "Student Status",
       cex = 0.7, 
       box.lwd = 2)   


#===============================================================================================
# CLASSIFICATION TREE

library(tree)
set.seed(1)
samples <- sample(1:nrow(enhanced_student_performance), 0.9 * nrow(enhanced_student_performance))
training <- enhanced_student_performance[samples, ]
test <- enhanced_student_performance[-samples, ]
tree.t <- tree(is_struggling ~ ., training)
summary(tree.t)
tree.t
par(mar = c(1,1,1,1), xpd = TRUE)
plot(tree.t, main = "Decision Tree for Predicting Struggling Students", type = "uniform")
text(tree.t, pretty = 0, cex = 0.7)
title(main = "Decision Tree for Predicting Struggling Students", font.main = 2, cex.main = 1.5)


#===================================================================================================
# ANALYSIS OF AVERAGE SUCESS RATE AND LEARNING TYPES 

difficulty_by_lrn_type <- item_difficulty_analysis %>%
  group_by(lrn_type) %>%
  summarise(avg_success_rate = mean(avg_success_rate, na.rm = TRUE))

library(ggplot2)

difficulty_by_lrn_type <- difficulty_by_lrn_type %>%
  filter(!is.na(lrn_type))

ggplot(difficulty_by_lrn_type, aes(x = lrn_type, y = avg_success_rate, fill = lrn_type)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Success Rate by Learning Type", x = "Learning Type", y = "Average Success Rate")

item_metrics <- responses %>%
  group_by(item_id) %>%
  summarise(
    avg_points_earned = mean(points_earned, na.rm = TRUE),
    completion_rate = mean(as.numeric(completes_page), na.rm = TRUE),
    avg_attempt = mean(attempt, na.rm = TRUE)
  )

items <- items %>%
  mutate(lrn_type = as.character(lrn_type))

item_lrn_metrics <- items %>%
  distinct(item_id, .keep_all = TRUE) %>%
  left_join(item_metrics, by = "item_id")

difficulty_features <- difficulty_by_lrn_type %>%
  left_join(item_lrn_metrics, by = "lrn_type")

write.csv(difficulty_features, "difficulty_features.csv", row.names = FALSE)

