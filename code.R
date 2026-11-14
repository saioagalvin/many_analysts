library(tidyverse)

# loading in data --------------------------------------------------------------

smarvus_raw <- read_csv("https://osf.io/download/a4zmv/")

# To avoid downloading the file many times, save a copy as smarvus_complete_050524.csv and load that in future:
smarvus_raw %>% write_csv("smarvus_complete_050524.csv")
smarvus_raw <- read_csv("smarvus_complete_050524.csv", show_col_types = FALSE)

# data processing --------------------------------------------------------------

# filter to just UK
smarvus_raw %>%
  count(country)
smarvus_uk <- smarvus_raw[smarvus_raw$country %in% c("England", "Scotland", "Northern Ireland"),]

UK <- c("England", "Scotland", "Northern Ireland")
# seeing proportion of respondents that are at UK unis
smarvus_raw %>%
  mutate(origin = if_else(country %in% UK, "UK", country)) %>%
  count(origin) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

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

# why have we only filtered out a couple of attention checks?
smarvus_uk_filtered <- smarvus_uk %>%
  filter(
    attention_amnesty == "Yes",
    Q7.1_24 == 1,
    Q8.1_21 == 5
  )

n_before <- nrow(smarvus_uk)
n_after  <- nrow(smarvus_uk_filtered)

n_before - n_after                 # rows removed
(n_before - n_after) / n_before * 100   # percentage removed
n_after / n_before * 100                # percentage kept

# integrity questions
# You have just answered three reasoning problems. Did you look any of the answers up online?
smarvus_uk %>%
  count(crt_check2)
smarvus_uk_filtered <- smarvus_uk_filtered[smarvus_uk_filtered$crt_check2 == "No", ]

# all postgrads are already excluded 
smarvus_uk %>%
  count(eligibility_ug)
smarvus_uk_filtered <- smarvus_uk_filtered[smarvus_uk_filtered$eligibility_ug == "Yes", ]

# exclude implausible ages
# age is split into 3 categories 18-21/22-25/26+/NA so can't remove 99+
smarvus_uk %>%
  count(age)

# only want students that have taken a statistics course (remove NA here?)
smarvus_uk %>%
  count(eligibility_stats)
smarvus_uk_filtered <- smarvus_uk_filtered[smarvus_uk_filtered$eligibility_stats == "Yes", ]

# only want psychology students
smarvus_raw %>%
  count(degree_major)
smarvus_uk %>%
  count(degree_major) %>%
  mutate(prop = n / sum(n) * 100) %>%
  arrange(desc(n))
smarvus_uk_filtered <- smarvus_uk_filtered[smarvus_uk_filtered$degree_major == "Psychology", ]

# only want people who aren't studying any other maths related courses?
# no, because "some other maths courses" includes statistics courses
# smarvus_uk_filtered %>%
#   count(degree_level_maths)
# smarvus_uk_filtered <- smarvus_uk_filtered[smarvus_uk_filtered$degree_level_maths == "No", ]

# checking proportion of respondents that are in each year
smarvus_uk %>%
  count(degree_year) %>%
  mutate(prop = n / sum(n) * 100)

# grade columns ----------------------------------------------------------------

# why is the majority of na grades in third year?
# maybe worth removing 3rd year?

# checking grade data for incorrect inputs (expecting all percentages)
smarvus_uk_clean <- smarvus_uk_filtered %>%
  mutate(grade_numeric = parse_number(grade, na = c("", "NA", "Not Available")))

smarvus_uk_clean %>%
  count(grade_high)

smarvus_uk_clean %>%
  summarise(
    too_low = sum(grade_numeric < grade_low),
    too_high = sum(grade_numeric > grade_high)
  )

ggplot(smarvus_uk_clean, aes(x=grade_numeric)) + 
  geom_histogram(bins=100)

# find percentage of na grade in each degree year
degree_nas <- smarvus_uk_clean %>%
  group_by(degree_year) %>%
  summarise(
    n = n(),
    n_na = sum(is.na(grade_numeric)),
    n_not_na = sum(!is.na(grade_numeric))
  )

degree_nas_long <- degree_nas %>%
  pivot_longer(
    cols = c(n_na, n_not_na),
    names_to = "grade_status",
    values_to = "count"
  ) %>%
  mutate(grade_status = recode(grade_status,
                               n_not_na = "Grade present",
                               n_na = "Grade missing"))

grade_na_bydegree <- ggplot(degree_nas_long,
                            aes(
                              x = degree_year, 
                              y = count, 
                              fill = grade_status)
                            ) +
  geom_col() +
  labs(
    title = "Grade missingness in SMARVUS by Degree Year",
    subtitle = "UK universities Psychology subset",
    x = "Degree year", 
    y = "Number of observations", 
    fill = NULL
       ) +
  theme_minimal()

grade_na_bydegree

# after making all missing into NA, missingness seems fairly uniform across years
# so maybe just need to accept that

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

grades_hist <- ggplot(smarvus_uk_clean, aes(x = grade_numeric)) +
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
    subtitle = "UK universities Psychology subset",
    x = "Grade (%)",
    y = "Density"
  ) +
  theme_minimal()

grades_hist


# maths/stats anxiety columns --------------------------------------------------

stars_totals <- smarvus_uk_clean %>%
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

smarvus_uk_scored <- smarvus_uk_clean %>%
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


# investigating relationship between maths/stats anxiety and grades ------------

ggplot(smarvus_uk_scored, aes(x = stars_score, y = grade_numeric)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(
    title = "Relationship Between Statistics Anxiety and Module Grades",
    subtitle = "UK universities SMARVUS data",
    x = "Total STARS Score (23 - 115)",
    y = "Statistics Module Grade (%)"
  ) +
  theme_minimal()

cor_result <- cor.test(smarvus_uk_scored$stars_score, smarvus_uk_scored$grade_numeric)
print(cor_result)

effectsize::r_to_d(cor_result$estimate)


# investigating relationship between historic attainment and current grade -----

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


# investigating relationship between gender and current grade ------------------

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

# investigating relationship between age and current grade ---------------------

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


# relationship between anxiety measures ----------------------------------------

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


