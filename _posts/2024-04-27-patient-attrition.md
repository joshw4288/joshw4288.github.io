---
title: "Patient Attrition Analysis for Substance Abuse Treatment Facility"
layout: post
image: "/posts/omnibus_plot.jpg"
tags: [R, Survival Analysis, People Analytics, Regression, Prediction]
---

** I analyze the attrition patterns of patients at a substance abuse treatment facility. 

This example is based on an analysis I completed as a consultant for a substance abuse treatment facility but uses fictitious data to demonstrate the approach.

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

![alt text](/img/posts/image1.jpeg)

The table below shows the estimated one year retention rate to be between 49% and 52%.  

```r
survfit(Surv(duration_months, discharged) ~ 1, data = data) |> 
  tbl_survfit(
    times = 12,
    label_header = "**1-Year Retention (95% CI)**"
  )
```

![alt text](/img/posts/table1.jpg)

As well, I examine the 3 month retention rate, which is estimated to be between 74% and 77%. 

```r
survfit(Surv(duration_months, discharged) ~ 1, data = data) |> 
  tbl_survfit(
    times = 3,
    label_header = "**3-Month Retention (95% CI)**"
  )
```

![alt text](/img/posts/table2.jpg)

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

![alt text](/img/posts/omnibus_plot.jpeg)

Here we can see that there is a statistically reliable difference between the retention rates. 

```r
survdiff(Surv(duration_months, discharged) ~ race, data = data)
```
![alt text](/img/posts/table3.jpg)

Because we had four racial categories in our data and we can visually see that the difference in attrition is between the white category and the other racial categories, we can use the category of white patients as the baseline and compare each other racial category to the baseline to determine if there is a reliable difference for each of the comparisons. From these comparisons we can see that there is a reliable difference between white patients and the other racial categories. 

There are interesting subtle patterns in these comparisons. The disparity in attrition between white patients and black patients begins to form at around the two month mark. This provides useful information for follow-up so that the clinic can investigate why Black patients begin leaving the program at a faster rate starting around the two-month mark. The same pattern exists for patients in the other/multiracial category. Finally, if we examine the comparison between Asian patients and white patients we can see that something happens around the 6 month mark that causes more rapid attrition.  

```r
wvb <- data |>
  filter(race %in% c('White', 'Black') , !is.na(race))
wvb_diff <- survdiff(Surv(duration_months, discharged) ~ race, data = wvb)
wvb_diff

wvb_plot <- survfit2(Surv(duration_months, discharged) ~ race, data = wvb) |>
   ggsurvfit(linewidth = 1.5) +
  labs(
    x = "Months",
    y = "Overall Probability of Remaining",
    title = "Attrition Over 1 Year"
  ) +
  scale_x_continuous(limits = c(0, 12), n.breaks = 12) +
  scale_y_continuous(limits = c(0, 1)) +
 scale_color_manual(values = c('#e26b16', '#0465FF')) +
  add_confidence_interval()

wvb_plot
```
![alt text](/img/posts/table4.jpg)
![alt text](/img/posts/wvb_plot.jpeg)

```r
wvo <- data |>
  filter(race %in% c('White', 'Other') , !is.na(race))
wvo_diff <- survdiff(Surv(duration_months, discharged) ~ race, data = wvo)
wvo_diff

wvo_plot <- survfit2(Surv(duration_months, discharged) ~ race, data = wvo) |>
   ggsurvfit(linewidth = 1.5) +
  labs(
    x = "Months",
    y = "Overall Probability of Remaining",
    title = "Attrition Over 1 Year"
  ) +
  scale_x_continuous(limits = c(0, 12), n.breaks = 12) +
  scale_y_continuous(limits = c(0, 1)) +
   scale_color_manual(values = c('#e26b16', '#002f43')) +
  add_confidence_interval()

wvo_plot
```
![alt text](/img/posts/table5.jpg)
![alt text](/img/posts/wvo_plot.jpeg)

```r

wva <- data |>
  filter(race %in% c('White', 'Asian') , !is.na(race))
wva_diff <- survdiff(Surv(duration_months, discharged) ~ race, data = wva)
wva_diff

wva_plot <- survfit2(Surv(duration_months, discharged) ~ race, data = wva) |>
  ggsurvfit(linewidth = 1.5) +
  labs(
    x = "Months",
    y = "Overall Probability of Remaining",
    title = "Retention Rates in Outpatient Substance Use Treatment Over 1 Year by Race"
  ) +
  scale_x_continuous(limits = c(0, 12), n.breaks = 12) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c('#e26b16', '#d3215d')) +
  add_confidence_interval()

wva_plot
```

![alt text](/img/posts/table6.jpg)
![alt text](/img/posts/wva_plot.jpeg)

Here we can see the estimated 1 year retention rates across each racial group and again, we can clearly see that white patients have higher retention rates than all other racial categories. But what happens if we look at other periods of time? For example, looking at the 2-month retention rates we begin to see differences across racial groups. In the third table below, we can see that by the third month we do see reliably higher attrition among black patients and other/multiracial patients relative to white patients. Finally, we can confirm the pattern we see for asian and white patients at the sixth month mark in the fourth table. As a follow-up to all of this I would engage in further conversation with subject matter experts at the hospital to understand what possibilities could be causing disparities to arise at the 2-month and 6-month marks and then using the necessary data we can test those hypotheses that we derive from those conversations. 

```{r}
#survdiff(Surv(duration_months, discharged) ~ race, data = data)

table_ci <- survfit(Surv(duration_months, discharged) ~ race, data = data) |> 
  tbl_survfit(
    times = 12,
    label_header = "**1-Year Retention (95% CI)**",
    label = list(race ~ "Race")
  )

table_ci
```
![alt text](/img/posts/table7.jpg)

```{r}
table_ci2 <- survfit(Surv(duration_months, discharged) ~ race, data = data) |> 
  tbl_survfit(
    times = 2,
    label_header = "**2-month Retention (95% CI)**",
    label = list(race ~ "Race")
  )

table_ci2
```
![alt text](/img/posts/table8.jpg)

```{r}
table_ci3 <- survfit(Surv(duration_months, discharged) ~ race, data = data) |> 
  tbl_survfit(
    times = 3,
    label_header = "**3-month Retention (95% CI)**",
    label = list(race ~ "Race")
  )

table_ci3
```
![alt text](/img/posts/table9.jpg)

```{r}
table_ci6 <- survfit(Surv(duration_months, discharged) ~ race, data = data) |> 
  tbl_survfit(
    times = 6,
    label_header = "**6-month Retention (95% CI)**",
    label = list(race ~ "Race")
  )

table_ci6
```
![alt text](/img/posts/table10.jpg)

Based on the second question, how might other demographic variables be related to attrition rates over time at the hospital? My approach begins with a Cox Proportional Hazard Regression Model, which is a method of determining which input variables are most associated with attrition over a specified period of time. For simplicity of demonstration, I will use only a small selection of the variables examined: race, marital status, gender, and education level. 

```{r}
cox_model <- survival::coxph(
  formula = Surv(event = discharged, time = duration_months) ~ sex + marital_status + race + education,
  data = data)

summary(cox_model)
```

As expected from the prior analyses, race has the largest impact on attrition. Compared to...

![alt text](/img/posts/table11.jpg)
