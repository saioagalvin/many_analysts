library(tidyverse)

# ------------------------------------------------------------------------------
# loading in data
# ------------------------------------------------------------------------------

smarvus_raw <- read_csv("https://osf.io/download/a4zmv/")

# To avoid downloading the file many times, save a copy as smarvus_complete_050524.csv and load that in future:
smarvus_raw %>% write_csv("smarvus_complete_050524.csv")
smarvus_raw <- read_csv("smarvus_complete_050524.csv")

# ------------------------------------------------------------------------------
# data processing
# ------------------------------------------------------------------------------

# filter to just UK
unique(smarvus_raw$country)
smarvus_uk <- smarvus_raw[smarvus_raw$country %in% c("England", "Scotland", "Northern Ireland"),]

# attention exclusions
smarvus_uk %>%
  select(contains("amnesty"), contains("attention"), matches("Q7.1_24"), matches("Q8.1_21"),matches("Q9.1_22"),matches("Q11.1_9"),matches("Q13.1_17"),matches("Q15.1_9")) %>%
  map(table, useNA = "ifany")

smarvus_uk %>%
  summarise(
    passed_amnesty = sum(attention_amnesty == "Yes", na.rm = TRUE),
    passed_check1  = sum(Q7.1_24 == 1, na.rm = TRUE),
    passed_check2  = sum(Q8.1_21 == 5, na.rm = TRUE),
    passed_check3 = sum(Q9.1_22 == 1, na.rm=TRUE),
    passed_check4 = sum(Q11.1_9 == 3, na.rm=TRUE),
    passed_check5 = sum(Q13.1_17 == 2, na.rm=TRUE),
    passed_check6 = sum(Q15.1_9 == 4, na.rm=TRUE)
  )

smarvus_uk_filtered <- smarvus_uk %>%
  filter(
    attention_amnesty == "Yes",
    Q7.1_24 == 1,
    Q8.1_21 == 5,
    
  )

nrow(smarvus_uk_filtered)


# ------------------------------------------------------------------------------
# maths/stats anxiety columns
# ------------------------------------------------------------------------------

stars_totals <- smarvus_uk_filtered %>%
  subset(select = -c(Q7.1_24)) %>%
  pivot_longer(
    cols = starts_with("Q7.1_"),
    values_to = "STARS"
  ) %>%
  summarise(
    .by = unique_id,
    stars_score = sum(STARS)
  )

stars_totals

smarvus_uk_scored <- smarvus_uk_filtered %>%
  left_join(stars_totals, by = "unique_id")

stars_summary <- smarvus_uk_scored %>%
  summarise(
    mean_score   = mean(stars_score, na.rm = TRUE),
    median_score = median(stars_score, na.rm = TRUE),
    sd_score     = sd(stars_score, na.rm = TRUE),
    min_score    = min(stars_score, na.rm = TRUE),
    max_score    = max(stars_score, na.rm = TRUE)
  )

print(stars_summary)

ggplot(smarvus_uk_scored, aes(x = stars_score)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "pink", color = "white", alpha = 0.7) +
  geom_density(color = "deeppink4", linewidth = 1) +
  labs(
    title = "Distribution of Statistics Anxiety Scores (STARS)",
    subtitle = "SMARVUS subset for UK universities",
    x = "Total STARS Score (Range: 23 - 115)",
    y = "Density"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# grade columns
# ------------------------------------------------------------------------------

smarvus_uk_clean <- smarvus_uk_scored %>% 
  mutate(grade_numeric = parse_number(grade))

grade_summary <- smarvus_uk_clean %>%
  summarise(
    n_students = sum(!is.na(grade_numeric)),
    n_missing  = sum(is.na(grade_numeric)),
    mean_grade = mean(grade_numeric, na.rm = TRUE),
    median_grade = median(grade_numeric, na.rm = TRUE),
    sd_grade   = sd(grade_numeric, na.rm = TRUE),
    min_grade  = min(grade_numeric, na.rm = TRUE),
    max_grade  = max(grade_numeric, na.rm = TRUE)
  )

print(grade_summary)

ggplot(smarvus_uk_clean, aes(x = grade_numeric)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    bins = 20, 
    fill = "lightgreen", 
    color = "white", 
    alpha = 0.7
    ) +
  geom_density(color = "darkgreen", linewidth = 1) +
  labs(
    title = "Distribution of Statistics Module Grades",
    subtitle = "UK universities subset",
    x = "Grade (%)",
    y = "Density"
  ) +
  theme_minimal()



# ------------------------------------------------------------------------------
# investigating relationship between maths/stats anxiety and grades
# ------------------------------------------------------------------------------

ggplot(smarvus_uk_clean, aes(x = stars_score, y = grade_numeric)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(
    title = "Relationship Between Statistics Anxiety and Module Grades",
    subtitle = "UK universities SMARVUS data",
    x = "Total STARS Score (23 - 115)",
    y = "Statistics Module Grade (%)"
  ) +
  theme_minimal()

cor_result <- cor.test(smarvus_uk_clean$stars_score, smarvus_uk_clean$grade_numeric)
print(cor_result)

effectsize::r_to_d(cor_result$estimate)


# ------------------------------------------------------------------------------
# investigating relationship between historic attainment and current grade
# ------------------------------------------------------------------------------

smarvus_uk_clean %>%
  count(math_edu_level, sort = TRUE)

smarvus_uk_clean %>%
  group_by(math_edu_level) %>%
  summarise(
    n            = n(),
    n_with_grade = sum(!is.na(grade_numeric)),
    mean_grade   = mean(grade_numeric, na.rm = TRUE),
    sd_grade     = sd(grade_numeric, na.rm = TRUE)
  )

gcse_alevel <- smarvus_uk_clean %>%
  filter(math_edu_level %in% c("GCSE", "A Level")) %>%
  mutate(math_edu_level = factor(math_edu_level, levels = c("GCSE", "A Level")))

ggplot(gcse_alevel, aes(x = math_edu_level, y = grade_numeric, fill = math_edu_level)) +
  geom_boxplot(alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.5, color = "steelblue") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "firebrick") +
  labs(
    title = "Statistics Module Grades by Prior Maths Education",
    subtitle = "Red blob is mean",
    x = "Highest High-School Maths Qualification",
    y = "Statistics Module Grade (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

library(effectsize)

d_math_edu <- cohens_d(grade_numeric ~ math_edu_level, data = gcse_alevel)
print(d_math_edu)


# ------------------------------------------------------------------------------
# investigating relationship between gender and current grade
# ------------------------------------------------------------------------------

smarvus_uk_clean %>%
  count(gender, sort = TRUE)

smarvus_uk_clean %>%
  group_by(gender) %>%
  summarise(
    n            = n(),
    n_with_grade = sum(!is.na(grade_numeric)),
    mean_grade   = mean(grade_numeric, na.rm = TRUE),
    sd_grade     = sd(grade_numeric, na.rm = TRUE)
  )

# making gender into a factor
smarvus_gender <- smarvus_uk_clean %>%
  mutate(gender = factor(gender, levels = c("Female/Woman", "Male/Man", "Another Gender")))

ggplot(smarvus_gender, aes(x = gender, y = grade_numeric, fill = gender)) +
  geom_boxplot(alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.5, color = "steelblue") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "firebrick") +
  labs(
    title = "Statistics Module Grades by Gender",
    subtitle = "Red blob is mean",
    x = "Gender",
    y = "Statistics Module Grade (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# cohen's d only works with 2 groups, so use anova
# d_gender <- cohens_d(grade_numeric ~ gender, data = smarvus_gender)
# print(d_gender)
anova(lm(grade_numeric ~ gender, data = smarvus_gender))

# ------------------------------------------------------------------------------
# investigating relationship between age and current grade
# ------------------------------------------------------------------------------

smarvus_uk_clean %>%
  count(age, sort = TRUE)

smarvus_uk_clean %>%
  group_by(age) %>%
  summarise(
    n            = n(),
    n_with_grade = sum(!is.na(grade_numeric)),
    mean_grade   = mean(grade_numeric, na.rm = TRUE),
    sd_grade     = sd(grade_numeric, na.rm = TRUE)
  )

# making age into a factor
smarvus_age <- smarvus_uk_clean %>%
  mutate(age = factor(age, levels = c("18-21", "22-25", "26+")))

ggplot(smarvus_age, aes(x = age, y = grade_numeric, fill = age)) +
  geom_boxplot(alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.5, color = "steelblue") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "firebrick") +
  labs(
    title = "Statistics Module Grades by Age Range",
    subtitle = "Red blob is mean",
    x = "Age range",
    y = "Statistics Module Grade (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# d_age <- cohens_d(grade_numeric ~ age, data = smarvus_age)
# print(d_age)
anova(lm(grade_numeric ~ age, data = smarvus_age))


# ------------------------------------------------------------------------------
# relationship between anxiety measures
# ------------------------------------------------------------------------------

# 1. STARS Score 
stars_totals <- smarvus_uk_filtered %>%
  subset(select = -c(Q7.1_24)) %>%
  pivot_longer(
    cols = starts_with("Q7.1_"),
    values_to = "STARS"
  ) %>%
  summarise(
    .by = unique_id,
    stars_score = sum(STARS, na.rm = TRUE)
  )

# 2. STARS-M Score 
stars_m_totals <- smarvus_uk_filtered %>%
  subset(select = -c(Q8.1_21)) %>%
  pivot_longer(
    cols = starts_with("Q8.1_"),
    values_to = "STARS_M"
  ) %>%
  summarise(
    .by = unique_id,
    stars_m_score = sum(STARS_M, na.rm = TRUE)
  )

# 3. R-MARS Score 
rmars_totals <- smarvus_uk_filtered %>%
  subset(select = -c(Q8.2_21, Q8.2_22, Q8.2_23, Q8.2_24)) %>%
  pivot_longer(
    cols = starts_with("Q8.2_"),
    values_to = "R_MARS"
  ) %>%
  summarise(
    .by = unique_id,
    rmars_score = sum(R_MARS, na.rm = TRUE)
  )

# 4. R-MARS-S Score 
rmars_s_totals <- smarvus_uk_filtered %>%
  subset(select = -c(Q7.2_20, Q7.2_21, Q7.2_22, Q7.2_23)) %>%
  pivot_longer(
    cols = starts_with("Q7.2_"),
    values_to = "R_MARS_S"
  ) %>%
  summarise(
    .by = unique_id,
    rmars_s_score = sum(R_MARS_S, na.rm = TRUE)
  )


smarvus_all_scores <- smarvus_uk_filtered %>%
  left_join(stars_totals, by = "unique_id") %>%
  left_join(stars_m_totals, by = "unique_id") %>%
  left_join(rmars_totals, by = "unique_id") %>%
  left_join(rmars_s_totals, by = "unique_id")


anxiety_corrs <- smarvus_all_scores %>%
  select(stars_score, stars_m_score, rmars_score, rmars_s_score) %>%
  cor(use = "complete.obs")

print(anxiety_corrs)


cor_long <- as.data.frame(as.table(anxiety_corrs))

ggplot(cor_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "green4", high = "firebrick", mid = "yellow2", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation (r)") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 4.5) +
  theme_minimal() +
  labs(
    title = "Correlation Heatmap of Anxiety Measures",
    x = "", 
    y = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

