
# Dietary Advice App Trial: Statistical Analysis in R 

A reproducible analysis of a two-arm study evaluating whether a **dietary advice app** is
associated with lower body weight over 3 months (N = 895; No app vs On app).

## Project Overview

The report takes raw trial data through cleaning, validation and seven hypothesis-driven analyses.
For every analysis it states the question, the test and why it was chosen, the null hypothesis, the
assumptions, **the checks run on those assumptions**, and an interpretation covering effect size,
direction, p-value and confidence interval.

| # | Question | Method |
|---|----------|--------|
| 1 | Who took part? | Summary table (mean (SD), median (IQR), n (%)) |
| 2 | Does weight at 3 months differ between arms? | Independent-samples t-test, Levene's test, Cohen's d |
| 3 | Does weight at 3 months differ by age group? | One-way ANOVA, Scheffé post-hoc |
| 4 | Is residential area associated with trial arm? | Pearson chi-square, Cramér's V |
| 5 | Did weight change from baseline to 3 months? | Paired t-test |
| 6 | Is BMI category associated with trial arm? | Chi-square test for trend |
| 7 | Is orexigenic medication associated with exercise frequency? | Mann–Whitney U |

## Key findings


- Weight at 3 months was about **3.7 kg lower** in the app arm (95% CI 2.3 to 5.1 kg, p < 0.001).
- Across all participants, weight fell by **about 2.0 kg** from baseline.
- App users were less likely to be overweight or obese but more likely to be underweight.
- Residential area differed between arms, which is worth investigating in a randomised design.
- Participants on orexigenic medication exercised markedly less (median 5 vs 14 sessions per month).

## Skills demonstrated

- Data import, validation and recoding (labelled factors, derived variables such as BMI)
- Missing-data audit and available-case analysis
- Choosing and justifying statistical tests; checking assumptions graphically and formally
- Effect sizes and confidence intervals alongside p-values
- Reproducible reporting with Quarto: narrative text whose numbers update from the data

## Repository structure

```text
├── analysis/Dietary_App_Analysis.qmd   # full report (code + narrative)
├── scripts/Dietary_App_Use_Analysis.R  # code only, extracted from the report
├── data/README.md                      # data dictionary and where to put the file
├── docs/index.html                     # rendered report (GitHub Pages)
├── outputs/                            # exported plots
└── renv.lock                           # package versions
```

## Data availability

The dataset (`dietaryapp.sav`, SPSS format) is **not included** in this repository. The code expects
the file at `data/dietaryapp.sav`. Variables are described in [`data/README.md`](data/README.md).
The rendered report in `docs/` shows all results without exposing individual-level records.

## How to reproduce

1. Clone the repository and open `dietary-app-trial-analysis.Rproj` in RStudio.
2. Place your copy of the data at `data/dietaryapp.sav`.
3. Restore the packages: `renv::restore()`
4. Render the report: `quarto render analysis/Dietary_App_Use_Analysis.qmd`

**Requirements:** R ≥ 4.3, Quarto ≥ 1.4.

## Author

**Abdulrahman Alazhari**: 
[LinkedIn](https://www.linkedin.com/in/aaazhari/) · aamalazhari@hotmail.com
