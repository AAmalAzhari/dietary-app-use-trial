## ----setup---------------------------------------------------------------------------------------------------------

library(rio)
library(tidyverse)
library(knitr)
library(gtsummary)


theme_set(theme_minimal(base_size = 12))

# Number formatters used for the inline results in the text
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
f3 <- function(x) formatC(x, format = "f", digits = 3)

# p-value formatter: returns "< 0.001" or "= 0.012" (the operator is included)
fp <- function(p) ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, format = "f", digits = 3)))

# Histogram with fitted normal curve, optional faceting by group
hist_normal <- function(d, x, g = NULL, bins = 40, xlab = x) {
  d <- d[!is.na(d[[x]]), ]
  p <- ggplot(d, aes(.data[[x]])) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   fill = "#2C7FB8", colour = "white")
  xs <- seq(min(d[[x]]), max(d[[x]]), length.out = 200)
  if (is.null(g)) {
    p <- p + geom_line(data = tibble(xv = xs, y = dnorm(xs, mean(d[[x]]), sd(d[[x]]))),
                       aes(xv, y), inherit.aes = FALSE, linewidth = 1)
  } else {
    curves <- d %>%
      group_by(across(all_of(g))) %>%
      summarise(m = mean(.data[[x]]), s = sd(.data[[x]]), .groups = "drop") %>%
      tidyr::crossing(xv = xs) %>%
      mutate(y = dnorm(xv, m, s))
    p <- p + geom_line(data = curves, aes(xv, y), inherit.aes = FALSE, linewidth = 1) +
      facet_wrap(g, ncol = 2)
  }
  p + labs(x = xlab, y = "Density")
}


## ----prep-import---------------------------------------------------------------------------------------------------
data <- import("dietaryapp.sav")
head(data)
glimpse(data)


## ----prep-missing--------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------
# PREP 1. MISSING-DATA CHECKING
# ---------------------------------------------------------------
missings_chk <- data %>%
  summarise(across(everything(), function(x) sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
  mutate(Percent = round(100 * Missing / nrow(data), 1)) %>%
  arrange(desc(Missing))
kable(missings_chk, caption = "Missing values per variable (raw data)")

# Duplicate participant IDs?
stopifnot("Duplicate PID found" = !anyDuplicated(data$PID))


## ----prep-derived--------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------
# PREP 2. DERIVED VARIABLES: weight change, BMI at 3 months, BMI group.
# BMI group codes: 1 underweight, 2 normal weight, 3 overweight, 4 obese.
# ---------------------------------------------------------------
data <- data %>%
  mutate(
    weightdiff  = weight3mnths - weightbase,
    BMI3mnths   = weight3mnths / height^2,
    BMIGrouping = case_when(BMI3mnths < 18.5 ~ 1,
                            BMI3mnths < 25   ~ 2,
                            BMI3mnths < 30   ~ 3,
                            BMI3mnths >= 30  ~ 4)
  )


## ----prep-clean----------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------
# PREP 3. CLEANING: set classes and labels
# (No special missing codes: the SPSS Variable View shows Missing = "None")
# ---------------------------------------------------------------
data <- data %>%
  mutate(
    # Identifier stored as text, not a number to analyse
    PID = as.character(PID),

    # Numeric codes -> labelled factors
    app         = factor(app,         levels = c(0, 1),    labels = c("No app", "On app")),
    sex         = factor(sex,         levels = c(0, 1),    labels = c("Female", "Male")),
    orexigenic  = factor(orexigenic,  levels = c(0, 1),    labels = c("Yes", "No")),
    agegroup    = factor(agegroup,    levels = 1:4,        labels = c("18-24", "25-29", "30-34", "35-40")),
    residence   = factor(residence,   levels = c(0, 1, 2), labels = c("urban", "suburban", "rural")),
    BMIGrouping = factor(BMIGrouping, levels = 1:4,
                         labels = c("underweight", "normal weight", "overweight", "obese"),
                         ordered = TRUE)
  )

# Safety net: recoding must not have created NAs (unexpected codes)
stopifnot("Unexpected code in a categorical variable" =
            !anyNA(data[c("app", "sex", "orexigenic", "agegroup", "residence", "BMIGrouping")]))

glimpse(data)


## ----prep-checks---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------
# PREP 4. CHECK THE RECODING 
#    Female 453 / Male 442; No app 447 / On app 448;
#    Urban 377 / Suburban 334 / Rural 184; Orexigenic Yes 259 / No 636
# ---------------------------------------------------------------
bind_rows(
  count(data, Variable = "Sex",         Level = as.character(sex)),
  count(data, Variable = "Trial arm",   Level = as.character(app)),
  count(data, Variable = "Residential", Level = as.character(residence)),
  count(data, Variable = "Orexigenic",  Level = as.character(orexigenic)),
  count(data, Variable = "Age group",   Level = as.character(agegroup))
) %>% kable(caption = "Category counts after recoding")

# Plausibility of numeric values (look for impossible values)
data %>%
  select(height, weightbase, weight3mnths, exercise, weightdiff, BMI3mnths) %>%
  summary()


## ----task1-table---------------------------------------------------------------------------------------------------
# ================================================================
# TASK 1. SUMMARY TABLE (overall and by trial arm)
#    Statistics: mean (SD) for approximately normal measures; median (IQR) for exercise
#                (skewed count); n (%) for categorical variables
#    Assumptions: descriptive only, so no inferential assumptions. The choice of statistic
#                 reflects distribution shape; percentages are column percentages and
#                 missing values are reported separately, not hidden.
# ================================================================
tbl1 <- data %>%
  select(app, height, weightbase, weight3mnths, exercise,
         sex, orexigenic, agegroup, residence) %>%
  tbl_summary(
    by = app,
    label = list(
      height       ~ "Height (m)",
      weightbase   ~ "Weight at baseline (kg)",
      weight3mnths ~ "Weight after 3 months (kg)",
      exercise     ~ "Times exercised for at least 30 min in last month",
      sex          ~ "Sex of participant",
      orexigenic   ~ "On orexigenic medication (appetite stimulant)",
      agegroup     ~ "Age group (years)",
      residence    ~ "Residential area"
    ),
    type = list(exercise ~ "continuous"),
    statistic = list(
      c(height, weightbase, weight3mnths) ~ "{mean} ({sd})",
      exercise                            ~ "{median} ({p25}, {p75})",
      all_categorical()                   ~ "{n} ({p}%)"
    ),
    digits  = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_overall(last = TRUE) %>%
  add_stat_label() %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~ "**Use of dietary advice app**") %>%
  modify_caption("**Table 1. Participant characteristics overall and by trial arm (No app vs On app)**") %>%
  modify_footnote(everything() ~ "SD = standard deviation; IQR = interquartile range (25th, 75th percentiles); n = count; % = column percentage.")

tbl1


## ----task2-hist----------------------------------------------------------------------------------------------------
#| fig-cap: "Weight after 3 months by trial arm, with fitted normal curves"
# ================================================================
# TASK 2. WEIGHT AT 3 MONTHS BETWEEN TRIAL ARMS
#    Test: independent-samples t-test
#    H0: mean weight at 3 months is the same in the No-app and On-app populations
#    Assumptions: independent groups; continuous outcome, approximately normal within
#                 each group; equal variances (Levene p > 0.05)
# ================================================================
# Normality: histograms with normal curve + Q-Q plots
hist_normal(data, "weight3mnths", g = "app", xlab = "Weight after 3 months (kg)")


## ----task2-qq------------------------------------------------------------------------------------------------------
#| fig-cap: "Normal Q-Q plots of weight after 3 months by trial arm"
ggplot(data, aes(sample = weight3mnths)) +
  stat_qq(alpha = .5) + stat_qq_line() + facet_wrap(~app) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles (kg)")


## ----task2-levene--------------------------------------------------------------------------------------------------
# Skewness and kurtosis within each arm
data %>%
  group_by(app) %>%
  summarise(N = n(),
            Skewness = DescTools::Skew(weight3mnths),
            Kurtosis = DescTools::Kurt(weight3mnths)) %>%
  kable(digits = 2, caption = "Shape of the weight distribution within each arm")

# Homogeneity of variance (centred on the mean)
lev <- car::leveneTest(weight3mnths ~ app, data = data, center = mean)
lev
lev_F <- lev$"F value"[1]
lev_p <- lev$"Pr(>F)"[1]


## ----task2-test----------------------------------------------------------------------------------------------------
# Group statistics
data %>%
  group_by(app) %>%
  summarise(N = n(), Mean = mean(weight3mnths), SD = sd(weight3mnths), SE = SD / sqrt(N)) %>%
  kable(digits = 3, caption = "Group statistics: weight after 3 months (kg)")

# t-test (mean difference = No app minus On app)
tt <- t.test(weight3mnths ~ app, data = data, var.equal = TRUE)
tt
cd <- rstatix::cohens_d(data, weight3mnths ~ app, var.equal = TRUE)

d2  <- unname(tt$estimate[1] - tt$estimate[2])
ci2 <- tt$conf.int


## ----task2-box-----------------------------------------------------------------------------------------------------
#| fig-cap: "Weight after 3 months by trial arm"
ggplot(data, aes(app, weight3mnths, fill = app)) +
  geom_boxplot(show.legend = FALSE, alpha = .8) +
  labs(x = NULL, y = "Weight after 3 months (kg)")


## ----task3-hist----------------------------------------------------------------------------------------------------
#| fig-cap: "Weight after 3 months by age group, with fitted normal curves"
#| fig-height: 6
# ================================================================
# TASK 3. WEIGHT AT 3 MONTHS BETWEEN AGE GROUPS
#    Test: one-way ANOVA + Scheffe post-hoc
#    H0: mean weight at 3 months is equal across the four age groups
#    Assumptions: independent groups; continuous outcome, approximately normal within
#                 each age group; equal variances (Levene p > 0.05)
# ================================================================
hist_normal(data, "weight3mnths", g = "agegroup", xlab = "Weight after 3 months (kg)")


## ----task3-levene--------------------------------------------------------------------------------------------------
lev3 <- car::leveneTest(weight3mnths ~ agegroup, data = data, center = mean)
lev3
lev3_F <- lev3$"F value"[1]
lev3_p <- lev3$"Pr(>F)"[1]

data %>%
  group_by(agegroup) %>%
  summarise(N = n(), Mean = mean(weight3mnths), SD = sd(weight3mnths), SE = SD / sqrt(N),
            Lower95 = Mean - qt(.975, N - 1) * SE, Upper95 = Mean + qt(.975, N - 1) * SE,
            Min = min(weight3mnths), Max = max(weight3mnths)) %>%
  kable(digits = 2, caption = "Weight after 3 months (kg) by age group")


## ----task3-anova---------------------------------------------------------------------------------------------------
fit3 <- aov(weight3mnths ~ agegroup, data = data)
an3  <- summary(fit3)[[1]]
an3
eta2 <- an3$"Sum Sq"[1] / sum(an3$"Sum Sq")

# Scheffe post-hoc 
sch3 <- DescTools::ScheffeTest(fit3, which = "agegroup", conf.level = 0.95)$agegroup %>%
  as.data.frame() %>%
  tibble::rownames_to_column("comparison")
sch3 %>%
  rename(`Comparison` = comparison, `Mean difference` = diff,
         `Lower 95% CI` = lwr.ci, `Upper 95% CI` = upr.ci, `Scheffe p` = pval) %>%
  kable(digits = 3, caption = "Scheffe post-hoc multiple comparisons")
sig_pairs <- sch3$comparison[sch3$pval < 0.05]


## ----task3-plot----------------------------------------------------------------------------------------------------
#| fig-cap: "Mean weight at 3 months by age group with 95% confidence intervals"
data %>%
  group_by(agegroup) %>%
  summarise(m = mean(weight3mnths), se = sd(weight3mnths) / sqrt(n())) %>%
  ggplot(aes(agegroup, m)) +
  geom_pointrange(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se)) +
  labs(x = "Age group (years)", y = "Mean weight at 3 months (kg), 95% CI")


## ----task4-table---------------------------------------------------------------------------------------------------
# ================================================================
# TASK 4. RESIDENTIAL AREA vs TRIAL ARM
#    Test: Pearson chi-square (3 x 2 table)
#    H0: no association between residential area and trial arm
#    Assumptions: categorical variables; independent observations (one row per participant);
#                 all expected counts > 1 and at least 80% of cells >= 5; counts, not percentages
# ================================================================
tab4 <- table(Residence = data$residence, Arm = data$app)
addmargins(tab4) %>% kable(caption = "Counts")

pct4 <- round(prop.table(tab4, 1) * 100, 1)       # % within residential area
kable(pct4, caption = "Row percentages (% within residential area)")

chi4 <- chisq.test(tab4, correct = FALSE)
chi4
round(chi4$expected, 2) %>% kable(caption = "Expected counts")
v4 <- DescTools::CramerV(tab4)


## ----task4-plot----------------------------------------------------------------------------------------------------
#| fig-cap: "Trial arm by residential area (% within residential area)"
as.data.frame(pct4) %>%
  ggplot(aes(Residence, Freq, fill = Arm)) +
  geom_col(position = "dodge") +
  labs(y = "% within residential area")


## ----task5-hist----------------------------------------------------------------------------------------------------
#| fig-cap: "Distribution of weight change (3 months minus baseline)"
# ================================================================
# TASK 5. CHANGE IN WEIGHT, BASELINE TO 3 MONTHS (all participants)
#    Test: paired t-test
#    H0: mean of the differences (3 months - baseline) is zero
#    Assumptions: paired observations (same participants twice); differences approximately
#                 normal; no extreme outliers in the differences
# ================================================================
hist_normal(data, "weightdiff", xlab = "Weight change: 3 months - baseline (kg)")


## ----task5-qq------------------------------------------------------------------------------------------------------
#| fig-cap: "Normal Q-Q plot of weight change"
ggplot(data, aes(sample = weightdiff)) +
  stat_qq(alpha = .5) + stat_qq_line() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles (kg)")


## ----task5-test----------------------------------------------------------------------------------------------------
tt5 <- t.test(data$weight3mnths, data$weightbase, paired = TRUE)
tt5
dz5 <- mean(data$weightdiff) / sd(data$weightdiff)


## ----task6-table---------------------------------------------------------------------------------------------------
# ================================================================
# TASK 6. BMI CATEGORY AT 3 MONTHS vs TRIAL ARM
#    Test: chi-square test for trend (linear-by-linear association), 2 x 4 table
#    H0: no association between BMI category and trial arm
#    Assumptions: binary arm and ordinal BMI category (scores 1-4); independent
#                 observations; all expected counts > 1 and at least 80% of cells >= 5;
#                 counts, not percentages
# ================================================================
tab6 <- table(Arm = data$app, BMI = data$BMIGrouping)
addmargins(tab6) %>% kable(caption = "Counts")

pct6 <- round(prop.table(tab6, 1) * 100, 1)       # % within trial arm
kable(pct6, caption = "Row percentages (% within trial arm)")

chi6 <- chisq.test(tab6, correct = FALSE)          # Pearson, for expected counts

# Linear-by-linear association: M^2 = (N - 1) * r^2, df = 1
trend_test <- function(arm, score) {
  ok <- complete.cases(arm, score)
  r  <- cor(as.numeric(arm[ok]), as.numeric(score[ok]))
  M2 <- (sum(ok) - 1) * r^2
  tibble(statistic = M2, df = 1, p.value = pchisq(M2, 1, lower.tail = FALSE), r = r)
}
trend6 <- trend_test(data$app, data$BMIGrouping)
kable(trend6, digits = 4, caption = "Chi-square test for trend")


## ----task6-plot----------------------------------------------------------------------------------------------------
#| fig-cap: "BMI category at 3 months by trial arm (% within trial arm)"
as.data.frame(pct6) %>%
  ggplot(aes(BMI, Freq, fill = Arm)) +
  geom_col(position = "dodge") +
  labs(x = "BMI category at 3 months", y = "% within trial arm")


## ----task7-hist----------------------------------------------------------------------------------------------------
#| fig-cap: "Exercise sessions in the last month by orexigenic medication"
# ================================================================
# TASK 7. OREXIGENIC MEDICATION vs EXERCISE FREQUENCY
#    Test: Mann-Whitney U (Wilcoxon rank-sum)
#    H0: distribution of exercise sessions is the same with and without orexigenic medication
#    Assumptions: independent groups; outcome at least ordinal (discrete count); non-normal
#                 distributions; similar distribution shapes if interpreting as a difference
#                 in medians; participants with missing exercise data excluded
#    Analysis uses participants with exercise data (N = 845: Yes 246, No 599)
# ================================================================
data_ex <- data %>% drop_na(exercise)
nrow(data_ex)

hist_normal(data_ex, "exercise", g = "orexigenic", bins = 30,
            xlab = "Times exercised for at least 30 min in last month")


## ----task7-box-----------------------------------------------------------------------------------------------------
#| fig-cap: "Exercise sessions in the last month by orexigenic medication"
ggplot(data_ex, aes(orexigenic, exercise, fill = orexigenic)) +
  geom_boxplot(show.legend = FALSE, alpha = .8) +
  labs(x = "On orexigenic medication (appetite stimulant)",
       y = "Times exercised for at least 30 min in last month")


## ----task7-desc----------------------------------------------------------------------------------------------------
# Skewness within each group
data_ex %>%
  group_by(orexigenic) %>%
  summarise(N = n(), Skewness = DescTools::Skew(exercise)) %>%
  kable(digits = 2, caption = "Skewness of exercise sessions by group")

# Percentiles
q7 <- data_ex %>%
  group_by(orexigenic) %>%
  summarise(N = n(),
            Q25    = quantile(exercise, .25, type = 6),
            Median = quantile(exercise, .50, type = 6),
            Q75    = quantile(exercise, .75, type = 6))
kable(q7, digits = 1, caption = "Exercise sessions in last month: median and quartiles")

# Ranks
data_ex %>%
  mutate(rk = rank(exercise)) %>%
  group_by(orexigenic) %>%
  summarise(N = n(), `Mean rank` = mean(rk), `Sum of ranks` = sum(rk)) %>%
  kable(digits = 2, caption = "Ranks")

# Median (Q25, Q75) as text
med_iqr <- function(g) {
  r <- q7[q7$orexigenic == g, ]
  sprintf("%.0f (%.0f, %.0f)", r$Median, r$Q25, r$Q75)
}


## ----task7-test----------------------------------------------------------------------------------------------------
wt7 <- wilcox.test(exercise ~ orexigenic, data = data_ex,
                   exact = FALSE, correct = FALSE, conf.int = TRUE)
wt7

n7 <- table(data_ex$orexigenic)
U7 <- min(wt7$statistic, prod(n7) - wt7$statistic)   
r7 <- rstatix::wilcox_effsize(data_ex, exercise ~ orexigenic)


## ----appendix-outliers---------------------------------------------------------------------------------------------
num_cols <- c("height", "weightbase", "weight3mnths", "exercise", "weightdiff", "BMI3mnths")

# Outliers by the boxplot rule (beyond 1.5 x IQR) for every numeric variable
outlier_tbl <- map_dfr(num_cols, function(v) {
  x   <- data[[v]]
  out <- DescTools::Outlier(x, method = "boxplot", na.rm = TRUE)
  tibble(Variable = v, N = sum(!is.na(x)), n_outliers = length(out),
         pct_outliers = round(100 * length(out) / sum(!is.na(x)), 1),
         lowest_out  = if (length(out)) min(out) else NA_real_,
         highest_out = if (length(out)) max(out) else NA_real_)
})
kable(outlier_tbl, caption = "Boxplot-rule outliers per numeric variable")

# Skewness, kurtosis and Shapiro-Wilk (judge normality mainly from the plots with N = 895)
map_dfr(num_cols, function(v) {
  x <- na.omit(data[[v]])
  tibble(Variable = v, N = length(x), Mean = mean(x), Median = median(x), SD = sd(x),
         Skewness = DescTools::Skew(x), Kurtosis = DescTools::Kurt(x),
         Shapiro_p = format.pval(shapiro.test(x)$p.value, digits = 3, eps = 0.001))
}) %>% kable(digits = 2, caption = "Distribution summary")


## ----session-------------------------------------------------------------------------------------------------------
sessionInfo()

