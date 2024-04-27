---
title: "Patient Attrition Analysis for Substance Treatment Facility"
layout: post
image: "/posts/ENTERIMAGE.jpg"
tags: [R, Survival Analysis, People Analytics, Regression, Prediction]
---

** This example is based on an analysis I completed as a consultant for a substance abuse treatment facility but uses fictitious data to demonstrate the approach.

The facility wanted to understand the patterns of patients leaving a substance abuse rehabilitation program and specifically to understand a) if there were differences across racial groups in attrition from the program and b) to see what other demographic variables collected at intake would be useful for identifying the patient groups most at risk of leaving the program.

```r
library(tidyverse)
library(janitor)
library(survival)
library(ggsurvfit)
library(gtsummary)
library(gt)

data <- read.csv("example.csv", na.strings = c("", "#NULL!")) |>
  clean_names()

data <- data |>
  mutate(marital_status = 
           case_when(
            marital_status == "No Entry" ~ NA,
            marital_status == "P-LIFE PARTNER" ~ "Married",
            marital_status == "M-MARRIED" ~ "Married",
            marital_status == "D-DIVORCED" ~ "Separated / Divorced",
            marital_status == "X-SEPARATED" ~ "Separated / Divorced",
            marital_status == "W-WIDOWED" ~ "Separated / Divorced",
            marital_status == "U-UNKNOWN" ~ NA,
            marital_status == "S-SINGLE" ~ "Single"
            ),
         race = 
           case_when(
             race == "A-ASIAN" ~ "Asian",
             race == "B-AFRICAN AMER/BLACK" ~ "Black",
             race == "D-DECLINED" ~ NA,
             race == "H-HISPANIC/LATINO/WH" ~ "Other",
             race == "I-NATIVE AMER/ALASKA" ~ "Other",
             race == "No Entry" ~ NA,
             race == "O-OTHER/MULTIRACIAL" ~ "Other",
             race == "P-NATV HAWAII/PACF I" ~ "Other",
             race == "U-UNAVAILABLE/UNKNOW" ~ NA,
             race == "W-WHITE" ~ "White"
           ),
         education =
           case_when(
             education == "Associates's degree" ~ "Degree",
             education == "Bachelor's degree" ~ "Degree",
             education == "Graduate degree" ~ "Graduate Degree",
             education == "High School Diploma or GED" ~ "HS Grad",
             education == "Some college, no degree" ~ "Some college",
             is.na(education) ~ NA_character_,
             TRUE ~ "Non HS Grad"
           )
         )

data$marital_status <- factor(data$marital_status, levels = c("Single", "Separated / Divorced", "Married"))

data$race <- factor(data$race, levels = c("White", "Asian", "Black", "Other"))

data$education <- factor(data$education, levels = c("Non HS Grad", "HS Grad", "Some college", "Degree", "Graduate Degree"))

data$sex <- factor(data$sex, levels = c("F", "M"))
```

First, I examine the general pattern of attrition by plotting the survival function over 1 year. Attrition begins quickly, with approximately 25% of patients leaving within the first three months. Attrition slows and we see another 25% of patients exit the program within the following 9 months, for a 1 year attrition rate of approximately 50%. Note that the start of the time period is admission + 30 days. This is because patients are not considered admitted until the 30 day intake period ends and then a decision is made about whether the patient needs to be admitted. 

```r
data$admission_date <- as.Date(data$admission_date_1, "%d-%b-%y")
data$discharge_date <- as.Date(data$discharge_date_1, "%d-%b-%y")

data <- data |>
  mutate(
    duration_months = as.duration((admission_date + days(30)) %--% discharge_date) / dmonths(1),
    discharged = case_when(
      is.na(discharge_date) ~ 0,
      !is.na(discharge_date) ~ 1
    )) |>
  mutate(duration_months = 
           case_when(
    is.na(duration_months) ~ 12,
    !is.na(duration_months) ~ duration_months
           )) |>
  filter(duration_months > 0)

data$duration_months <- as.double(data$duration_months)

data <- data |>
  mutate(discharged = 
           case_when(
             duration_months > 12 ~ 0,
             .default = as.integer(discharged)
           ))

survfit2(Surv(duration_months, discharged) ~ 1, data = data) |>
  ggsurvfit() +
  labs(
    x = "Months",
    y = "Probability of Remaining",
    title = "Attrition Over 1 Year"
  ) +
  add_confidence_interval() +
  add_risktable() +
  scale_x_continuous(limits = c(0, 12), n.breaks = 12) +
  scale_y_continuous(limits = c(0, 1))
```

ENTER IMAGE 1 HERE

The table below shows the estimated one year retention rate to be between 49% and 52%.  

```r
survfit(Surv(duration_months, discharged) ~ 1, data = data) |> 
  tbl_survfit(
    times = 12,
    label_header = "**1-Year Retention (95% CI)**"
  )
```

ENTER TABLE1 HERE

As well, I examine the 3 month retention rate, which is estimated to be between 74% and 77%. 

```r
survfit(Surv(duration_months, discharged) ~ 1, data = data) |> 
  tbl_survfit(
    times = 3,
    label_header = "**3-Month Retention (95% CI)**"
  )
```

ENTER TABLE2 HERE

Because the primary question of our client was whether attrition over time differs by race of the patient, I examine the survival curves for each racial category. What we visually see is that patients who identify as white have higher retention in the program than patients who identify with another racial group. We can confirm that this pattern is reliable through both the confidence intervals displayed on the plot and through chi-square test to determine whether the retention rates are reliably different from one another. 

```r
race_surve <- survfit2(Surv(duration_months, discharged) ~ race, data = data)

survfit2(Surv(duration_months, discharged) ~ race, data = data) |>
  ggsurvfit(linewidth = 1.5) +
  labs(
    x = "Months",
    y = "Overall Probability of Remaining",
    title = "Retention Rates in Outpatient Substance Use Treatment Over 1 Year by Race"
  ) +
  scale_x_continuous(limits = c(0, 12), n.breaks = 12) +
  scale_y_continuous(limits = c(0, 1)) +
  add_confidence_interval() +
  scale_color_manual(values = c('#e26b16', '#d3215d', '#0465FF', '#002f43'))
```

ENTER OMNIBUSPLOT HERE
