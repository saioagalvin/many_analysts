library(tidyverse)

# loading in data --------------------------------------------------------------

smarvus_raw <- read_csv("https://osf.io/download/a4zmv/")

# To avoid downloading the file many times, save a copy as smarvus_complete_050524.csv and load that in future:
smarvus_raw %>% write_csv("smarvus_complete_050524.csv")
smarvus_raw <- read_csv("smarvus_complete_050524.csv", show_col_types = FALSE)

# data processing --------------------------------------------------------------

# attention exclusions
smarvus_raw %>%
  select(
    contains("amnesty"), 
    contains("attention"), 
    matches("Q7.1_24"), 
    matches("Q8.1_21"),
    matches("Q9.1_22"),
    matches("Q11.1_9"),
    matches("Q13.1_17"),
    matches("Q15.1_9")
    ) %>%
  map(table, useNA = "ifany")

smarvus_raw %>%
  summarise(
    passed_amnesty = sum(attention_amnesty == "Yes", na.rm = TRUE),
    passed_check1  = sum(Q7.1_24 == 1, na.rm = TRUE),
    passed_check2  = sum(Q8.1_21 == 5, na.rm = TRUE),
    passed_check3 = sum(Q9.1_22 == 1, na.rm=TRUE),
    passed_check4 = sum(Q11.1_9 == 3, na.rm=TRUE),
    passed_check5 = sum(Q13.1_17 == 2, na.rm=TRUE),
    passed_check6 = sum(Q15.1_9 == 4, na.rm=TRUE)
  )

# Q: why have we only filtered out a couple of attention checks? 
# A: this was what George suggested we do as it was prompted in the Week 8 
# workshop Rmd. He suggested we think about whether we filter out people who
# failed any attention check, or all, or somewhere in between. I think the
# argument for using these specific ones is it filters out people at specific
# points, so Q7.1_24 occurs at the end of the STARS section, and Q8.1_21 occurs
# at the end of the STARS-M section. The question is then do we want to exclude
# people who stopped paying attention later, if they've got valid data for the
# parts we care about? -Louisa
smarvus_filtered <- smarvus_raw %>%
  filter(
    attention_amnesty == "Yes",
    Q7.1_24 == 1,
    Q8.1_21 == 5,
    Q9.1_22 == 1
  )

n_before <- nrow(smarvus_raw)

n_after  <- nrow(smarvus_filtered)

n_before - n_after                 # rows removed
(n_before - n_after) / n_before * 100   # percentage removed
n_after / n_before * 100                # percentage kept

# integrity questions
# You have just answered three reasoning problems. Did you look any of the answers up online?
smarvus_raw %>%
  count(crt_check2)
smarvus_filtered <- smarvus_filtered[smarvus_filtered$crt_check2 == "No", ]

# progress
progress_hist <- ggplot(smarvus_filtered) +
  geom_histogram(
    aes(x = progress,
        y = after_stat(density)),
    bins = 50,
    position = "identity"
  ) +
  theme_minimal()
progress_hist

# all postgrads are already excluded 
smarvus_raw %>%
  count(eligibility_ug)
smarvus_filtered <- smarvus_filtered[smarvus_filtered$eligibility_ug == "Yes", ]

# exclude implausible ages
# age is split into 3 categories 18-21/22-25/26+/NA so can't remove 99+
smarvus_raw %>%
  count(age)

# only want students that have taken a statistics course (remove NA here?)
smarvus_raw %>%
  count(eligibility_stats)
smarvus_filtered <- smarvus_filtered[smarvus_filtered$eligibility_stats == "Yes", ]

# only want psychology students
smarvus_raw %>%
  count(degree_major) %>%
  mutate(prop = n / sum(n) * 100) %>%
  arrange(desc(n))
smarvus_filtered <- smarvus_filtered[smarvus_filtered$degree_major == "Psychology", ]

# Q: only want people who aren't studying any other maths related courses?
# A: no, because "some other maths courses" includes statistics courses
# smarvus_uk_filtered %>%
#   count(degree_level_maths)
# smarvus_uk_filtered <- smarvus_uk_filtered[smarvus_uk_filtered$degree_level_maths == "No", ]

# checking proportion of respondents that are in each year
smarvus_raw %>%
  count(degree_year) %>%
  mutate(prop = n / sum(n) * 100)

# grade columns ----------------------------------------------------------------

# which all unis have submitted grade data?
# make sure all missing data is written as NA
# which of these unis has non-percentage grade data
smarvus_grades <- smarvus_filtered %>%
  mutate(
    grade = trimws(grade),
    grade = if_else(grade %in% c("", "NA", "Not Available"), NA_character_, grade),
    is_numeric = str_detect(grade, "^\\d+(\\.\\d+)?\\s*%?$"),
    is_letter  = str_detect(grade, "^[A-Fa-f][+-]?$")
  ) %>%
  group_by(university, country) %>%
  summarise(
    n = n(),
    n_grade = sum(!is.na(grade)),
    n_na_grade = sum(is.na(grade)),
    percent_na_grade = sum(is.na(grade)) / n * 100,
    has_grade_data = sum(!is.na(grade)) > 0,
    percent_grade_numeric = sum(is_numeric, na.rm = TRUE) / n_grade * 100,
    percent_grade_letter  = sum(is_letter,  na.rm = TRUE) / n_grade * 100
  ) %>%
  filter(has_grade_data) %>%
  select(-has_grade_data) %>%
  arrange(desc(n_grade))
smarvus_grades

smarvus_grades_long <- smarvus_grades %>%
  pivot_longer(
    cols = c(n_grade, n_na_grade),
    names_to = "grade_status",
    values_to = "count"
  ) %>%
  mutate(grade_status = recode(grade_status,
                               n_grade = "Grade present",
                               n_na_grade = "Grade missing"))
  
smarvus_grades_plot <- ggplot(smarvus_grades_long,
                              aes(
                                x = fct_reorder(university, 
                                           count * (grade_status == "Grade present"),
                                           .fun = sum, 
                                           .desc = TRUE),
                                y = count, 
                                fill = grade_status)
                              ) +
  geom_col() +
  labs(
    title = "Grade missingness in SMARVUS by University",
    subtitle = "Psychology subset of all universities surveyed with grade data provided",
    x = "University", 
    y = "Number of observations", 
    fill = NULL
  ) +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 45))
smarvus_grades_plot

# UK grades --------------------------------------------------------------------

# filter to just UK
smarvus_filtered %>%
  count(country)
smarvus_uk <- smarvus_filtered[smarvus_filtered$country %in% c("England", "Scotland", "Northern Ireland"),]

UK <- c("England", "Scotland", "Northern Ireland")
# seeing proportion of respondents that are at UK unis
smarvus_filtered %>%
  mutate(origin = if_else(country %in% UK, "UK", country)) %>%
  count(origin) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

# checking grade data for incorrect inputs (expecting all percentages)
smarvus_uk_clean <- smarvus_uk %>%
  mutate(grade_numeric = parse_number(grade, na = c("", "NA", "Not Available")))

# which UK universities have submitted grade data?
smarvus_uk_clean %>%
  count(university)

uk_university_nas <- smarvus_uk_clean %>%
  group_by(university) %>%
  summarise(
    n = n(),
    n_na = sum(is.na(grade_numeric)),
    n_not_na = sum(!is.na(grade_numeric))
  )
uk_university_nas

# total number of grade data if we took from full dataset
sum(smarvus_grades$n_grade)
# total number of grade data if we took from UK dataset
sum(uk_university_nas$n_not_na)

uk_nas_long <- uk_university_nas %>%
  pivot_longer(
    cols = c(n_not_na, n_na),
    names_to = "grade_status",
    values_to = "count"
  ) %>%
  mutate(grade_status = recode(grade_status,
                               n_not_na = "Grade present",
                               n_na = "Grade missing"))

grade_na_byuni <- ggplot(uk_nas_long,
                         aes(
                           x = fct_reorder(university, 
                                           count * (grade_status == "Grade present"),
                                           .fun = sum, 
                                           .desc = TRUE),
                           y = count, 
                           fill = grade_status)
) +
  geom_col() +
  labs(
    title = "Grade missingness in SMARVUS by UK University",
    subtitle = "UK universities Psychology subset",
    x = "University", 
    y = "Number of observations", 
    fill = NULL
  ) +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 45))

grade_na_byuni

# check for incorrect inputs
smarvus_uk_clean %>%
  count(grade_high)

smarvus_uk_clean %>%
  summarise(
    too_low = sum(grade_numeric < grade_low),
    too_high = sum(grade_numeric > grade_high)
  )

ggplot(smarvus_uk_clean, aes(x=grade_numeric)) + 
  geom_histogram(bins=100)

# grade distribution has very large spike at around 65? examine grade distribution by uni
# to look for outliers: note have commented out as better to do this 
# below after removing NA grades
# ggplot(smarvus_uk_clean, aes(x = university, y = grade_numeric)) +
#  geom_boxplot() +
#  labs(title = "Statistics Module Grades by University",
#    x = "University",
#    y = "Statistics Module Grade (%)") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45), hjust = 1)

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

#here remove all na grades then check distribution of grades by uni
smarvus_uk_gradecheck <- smarvus_uk_clean[
  !is.na(smarvus_uk_clean$grade_numeric), 
]


ggplot(smarvus_uk_gradecheck, aes(x = university, y = grade_numeric)) +
    geom_boxplot() +
    labs(title = "Statistics Module Grades by University",
      x = "University",
      y = "Statistics Module Grade (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

#actually do bar graph of each to look closer
grades_byuni <- smarvus_uk_gradecheck %>%
  filter(university %in% c(
    "Anglia Ruskin University",
    "Bournemouth University",
    "Queen's University Belfast",
    "Teesside University",
    "University of Bradford",
    "University of Lincoln",
    "University of Southampton",
    "University of Sussex"
    )) %>%
  mutate(
    grade_band = cut(
      grade_numeric,
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
      right = FALSE,
      labels = c(
        "0-9", "10-19", "20-29", "30-39", "40-49",
        "50-59", "60-69", "70-79", "80-89", "90-100"
      )
    )
  ) %>%
  group_by(university, grade_band) %>%
  summarise(count = n(), .groups = "drop")

ggplot(
  grades_byuni,
  aes(x = grade_band, y = count)
) +
  geom_col() +
  facet_wrap(~ university, scales = "free_y") +
  labs(
    title = "Grade Distributions by University",
    x = "Statistics Module Grade (%)",
    y = "Number of students"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# now investigate southampton and queen's specifically as their distributions are
# suspicious: vast majority of grades are in 60-69 range
# southampton
grades_southampton <- smarvus_uk_gradecheck %>%
  filter(university == "University of Southampton")

southampton_plot <- ggplot(
  grades_southampton,
  aes(x = factor(grade_numeric))
) +
  geom_bar(
    fill = "steelblue3",
    colour = "white"
  ) +
  labs(
    title = "Grade Distribution: University of Southampton",
    x = "Statistics Module Grade (%)",
    y = "Number of students"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

southampton_plot


# queen's
grades_queens <- smarvus_uk_gradecheck %>%
  filter(university == "Queen's University Belfast")

queens_plot <- ggplot(
  grades_queens,
  aes(x = factor(grade_numeric))
) +
  geom_bar(
    fill = "steelblue3",
    colour = "white"
  ) +
  labs(
    title = "Grade Distribution: Queen's University Belfast",
    x = "Statistics Module Grade (%)",
    y = "Number of students"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

queens_plot
# after making plots, queen's actually looks alright: 
# most grades are in 60-69, but could just be courses with small standard deviation
# some spikes at 63,64 and 68, but don't look unreasonable.
# southampton however is very suspicious: 
# over 60 students got exactly a 65. table below shows this
southampton_grade_table <- grades_southampton %>%
  count(grade_numeric, name = "Number of students") %>%
  arrange(grade_numeric)

southampton_grade_table
# so we have 1 grade of 46, 1 grade of 54, 1 grade of 69 and 68 grades of 65.
# very odd, go back and look at full distribution before we removed any students
# to see if issue remains
# all original Southampton respondents
southampton_raw <- smarvus_raw %>%
  filter(university == "University of Southampton")

# count each original grade, including missing values
southampton_raw %>%
  mutate(
    grade = trimws(grade),
    grade = na_if(grade, ""),
    grade = if_else(
      is.na(grade) | grade %in% c("NA", "Not Available"),
      "Missing",
      grade
    )
  ) %>%
  count(grade, name = "Number of students") %>%
  arrange(grade)
# so confirms our filtering did not create the odd distribution:
# before any of our filtering, of 163 students, 99 had grades reported, and 93
# of these recieved a grade of 65. hence we will remove southampton.
# after our filtering, of 71 students with reported grades, 68 received a 65.
# suggests 65 may represent some kind of placeholder grade or data cleaning issue.


# after making all missing into NA, missingness seems fairly uniform across years
# so maybe just need to remove all rows with missing grades, and additionally here 
# remove 0 grades and uni of southampton due to suspicious distribution of grades

smarvus_uk_cleangrades <- smarvus_uk_clean[
  !is.na(smarvus_uk_clean$grade_numeric) & 
    smarvus_uk_clean$grade_numeric != 0 &
    smarvus_uk_clean$university != "University of Southampton",
  ]
N <- nrow(smarvus_uk_cleangrades)

grades_hist <- ggplot(smarvus_uk_cleangrades, aes(x = grade_numeric)) +
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


# age eda ----------------------------------------------------------------------

unique(smarvus_uk_cleangrades$degree_year)

grade_density_year <- ggplot(
  filter(smarvus_uk_cleangrades, !is.na(degree_year), !is.na(grade_numeric)),
  aes(x = grade_numeric, fill = degree_year, color = degree_year)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    alpha = 0.4,
    position = "identity",
    color = "white"
  ) +
  geom_density(alpha = 0, linewidth = 1) +
  labs(
    title = "Distribution of Statistics Module Grades by Degree Year",
    subtitle = "UK universities Psychology subset",
    x = "Grade (%)",
    y = "Density",
    fill = "Degree year",
    color = "Degree year"
  ) +
  theme_minimal()

grade_density_year


# maths/stats anxiety columns --------------------------------------------------

stars_totals <- smarvus_uk_cleangrades %>%
  subset(select = -c(Q7.1_24)) %>%
  pivot_longer(
    cols = starts_with("Q7.1_"),
    values_to = "STARS"
  ) %>%
  summarise(
    .by = unique_id,
    stars_score = sum(STARS)
  )

# for write-up, check that stars_score doesn't include anyone with any NA 
# answers as this could skew sum.
# stars_totals %>%
#  summarise(
#    total = n(),
#    complete = sum(!is.na(stars_score)),
#    incomplete = sum(is.na(stars_score)),
#    percent_missing = mean(is.na(stars_score)) * 100
#  )
# great, total = 795 and complete = 795.

stars_totals

smarvus_uk_scored <- smarvus_uk_cleangrades %>%
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

STARS_hist <- ggplot(smarvus_uk_scored, aes(x = stars_score)) +
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

STARS_attainment <- ggplot(smarvus_uk_scored, aes(x = stars_score, y = grade_numeric)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(
    title = "Relationship Between Statistics Anxiety and Module Grades",
    subtitle = "UK universities SMARVUS data",
    x = "Total STARS Score (23 - 115)",
    y = "Statistics Module Grade (%)"
  ) +
  theme_minimal()

grades_cor_result <- cor.test(smarvus_uk_scored$stars_score, smarvus_uk_scored$grade_numeric)
print(grades_cor_result)

grades_d <- effectsize::r_to_d(grades_cor_result$estimate)

# predicting statistics grades at extreme values of STARS score for write-up, 
# we get 28 gives 70%, 115 gives 62%
# grades_model <- lm(grade_numeric ~ stars_score, data = smarvus_uk_scored)
# predicted_grades <- data.frame(stars_score = c(28, 115))
# predict(grades_model, newdata = predicted_grades)


# investigating relationship between historic attainment and current grade -----

# GCSE vs A Level maths education
smarvus_uk_scored %>%
  filter(math_edu_level %in% c("GCSE", "A Level")) %>%
  count(math_edu_level, sort = TRUE)

smarvus_uk_scored %>%
  filter(math_edu_level %in% c("GCSE", "A Level")) %>%
  group_by(math_edu_level) %>%
  summarise(
    n = n(),
    n_with_grade = sum(!is.na(grade_numeric)),
    mean_grade = mean(grade_numeric, na.rm = TRUE),
    sd_grade = sd(grade_numeric, na.rm = TRUE)
  )

# Keep only GCSE and A Level students
gcse_alevel <- smarvus_uk_scored %>%
  filter(math_edu_level %in% c("GCSE", "A Level")) %>%
  mutate(
    math_edu_level = factor(
      math_edu_level,
      levels = c("GCSE", "A Level")
    )
  )

# Violin plot of statistics grades by prior maths education
math_edu_violin <- ggplot(
  gcse_alevel,
  aes(x = math_edu_level, y = grade_numeric, fill = math_edu_level)
) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_jitter(width = 0.12, alpha = 0.5, color = "steelblue") +
  stat_summary(
    fun = mean, geom = "point", shape = 18,
    size = 4, color = "firebrick"
  ) +
  labs(
    title = "Statistics Module Grades by Prior Maths Education",
    subtitle = "Red diamond represents the mean",
    x = "Highest High-School Maths Qualification",
    y = "Statistics Module Grade (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

math_edu_violin

# Cohen's d for the unadjusted GCSE vs A Level comparison
d_math_edu <- effectsize::cohens_d(
  grade_numeric ~ math_edu_level,
  data = gcse_alevel
)
print(d_math_edu)

# ANCOVA: compare GCSE and A Level while controlling for STARS score
math_ancova_data <- gcse_alevel %>%
  drop_na(grade_numeric, stars_score)

math_ancova <- lm(
  grade_numeric ~ math_edu_level + stars_score,
  data = math_ancova_data
)

math_ancova_table <- car::Anova(math_ancova, type = 2)
math_ancova_summary <- summary(math_ancova)
math_ancova_eta <- effectsize::eta_squared(
  math_ancova_table,
  partial = TRUE
)

math_ancova_table
math_ancova_summary
math_ancova_eta

# Check homogeneity of regression slopes
math_interaction <- lm(
  grade_numeric ~ math_edu_level * stars_score,
  data = math_ancova_data
)
math_interaction_table <- car::Anova(math_interaction, type = 2)
math_interaction_table


# investigating relationship between gender and current grade ------------------

# Compare women and men only
smarvus_gender <- smarvus_uk_scored %>%
  filter(gender %in% c("Female/Woman", "Male/Man")) %>%
  mutate(
    gender = factor(
      gender,
      levels = c("Female/Woman", "Male/Man"),
      labels = c("Women", "Men")
    )
  )

smarvus_gender %>%
  count(gender, sort = TRUE)

smarvus_gender %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    n_with_grade = sum(!is.na(grade_numeric)),
    mean_grade = mean(grade_numeric, na.rm = TRUE),
    sd_grade = sd(grade_numeric, na.rm = TRUE)
  )

# Violin plot of statistics grades by gender
gender_violin <- ggplot(
  smarvus_gender,
  aes(x = gender, y = grade_numeric, fill = gender)
) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_jitter(width = 0.12, alpha = 0.5, color = "steelblue") +
  stat_summary(
    fun = mean, geom = "point", shape = 18,
    size = 4, color = "firebrick"
  ) +
  labs(
    title = "Statistics Module Grades by Gender",
    subtitle = "Red diamond represents the mean",
    x = "Gender",
    y = "Statistics Module Grade (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

gender_violin

# ANCOVA: compare women and men while controlling for STARS score
gender_ancova_data <- smarvus_gender %>%
  drop_na(grade_numeric, stars_score)

gender_ancova <- lm(
  grade_numeric ~ gender + stars_score,
  data = gender_ancova_data
)

gender_ancova_table <- car::Anova(gender_ancova, type = 2)
gender_ancova_summary <- summary(gender_ancova)
gender_ancova_eta <- effectsize::eta_squared(
  gender_ancova_table,
  partial = TRUE
)

gender_ancova_table
gender_ancova_summary
gender_ancova_eta

# Check homogeneity of regression slopes
gender_interaction <- lm(
  grade_numeric ~ gender * stars_score,
  data = gender_ancova_data
)
gender_interaction_table <- car::Anova(gender_interaction, type = 2)
gender_interaction_table

# investigating relationship between age and current grade ---------------------

smarvus_uk_cleangrades %>%
  count(age, sort = TRUE)

smarvus_uk_cleangrades %>%
  group_by(age) %>%
  summarise(
    n            = n(),
    n_with_grade = sum(!is.na(grade_numeric)),
    mean_grade   = mean(grade_numeric, na.rm = TRUE),
    sd_grade     = sd(grade_numeric, na.rm = TRUE)
  )

# making age into a factor
smarvus_age <- smarvus_uk_cleangrades %>%
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
stars_totals <- smarvus_uk_cleangrades %>%
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
stars_m_totals <- smarvus_uk_cleangrades %>%
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
rmars_totals <- smarvus_uk_cleangrades %>%
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
rmars_s_totals <- smarvus_uk_cleangrades %>%
  subset(select = -c(Q7.2_20, Q7.2_21, Q7.2_22, Q7.2_23)) %>%
  pivot_longer(
    cols = starts_with("Q7.2_"),
    values_to = "R_MARS_S"
  ) %>%
  summarise(
    .by = unique_id,
    rmars_s_score = sum(R_MARS_S, na.rm = TRUE)
  )


smarvus_all_scores <- smarvus_uk_cleangrades %>%
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


# investigating correlation of r-mars-s with attainment
grades_rmarss_cor_result <- cor.test(smarvus_all_scores$rmars_s_score, smarvus_all_scores$grade_numeric)

print(grades_rmarss_cor_result)

grades_rmarss_d <- effectsize::r_to_d(grades_rmarss_cor_result$estimate)
print(grades_rmarss_d)

grades_rmarss_CI <- effectsize::r_to_d(r= grades_rmarss_cor_result$conf.int[1:2])
print(grades_rmarss_CI)

forest_row <- data.frame(
  Variable = "Choices made",
  d = as.numeric(grades_rmarss_d),
  CI_low = as.numeric(grades_rmarss_CI[1]),
  CI_high = as.numeric(grades_rmarss_CI[2]))

#forest_data <- rbind(forest_row, forest_row2, forest_row3)

#ggplot(forest_data, aes(y = Variable, x = d, xmin = CI_low, xmax = CI_high)) +
  #geom_point(size = 3) +
  #geom_errorbarh(height = 0.2) +
  #geom_vline(xintercept = 0, linetype = "dashed") +
  #theme_minimal() +
  #xlab("Cohen's d")

