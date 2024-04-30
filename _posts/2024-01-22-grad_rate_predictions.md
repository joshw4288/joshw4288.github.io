---
title: "Predicting College Graduation Rates (2023-2025)"
layout: post
image: "/posts/image_predicting_graduation_rates.jpg"
tags: [R, GLM, Predict]
---
In this project, I project future college graduation rates for a college in Texas. This is similar to a real project I completed for stakeholders in my role as a research scientist. All data used in this project are either publically available or fictitious.

# Context and Base Model

I was asked to review projected graduation rates, which had been provided to the executive leadership team by an analyst. The model presented to the executive leadership team was a linear regression with graduation rates for unknown years (2023, 2024, 2025) predicted by the linear extrapolation of graduation rate from year. This model is reproduced below and reflects a simple estimate based on an assumption of a linear trend over time. Based upon graduation rates from 2016-2022, the model produces the best linear fit that minimizes the squared distance between the actual values and the predicted values. Prediction intervals are wide and reflect the potential range we could reasonably expect given the uncertainty around predicting single values into the future. I then produce a revised model, which takes into account important features that provide additional information and improve the model. 

First, I load a series of packages that allow me to manipulate data using the tidyverse and select a nice theme for my ggplot figures.

```r
library(tidyverse)
library(ggthemes)
```

Next, I create a dataframe that contains the known graduation rates from 2016-2022 and create a dataframe which contains the unknown years 2023-2025.

```r
data <- data.frame(year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022), grad_rate = c(.28, .32, .41, .37, .42, .4, .37))
new_data <- data.frame(year = c(2023, 2024, 2025))
```
Note that graduation rates reflect the rate at which a cohort of students complete their program of study within 1.5x the typical length of a program. Because I am predicting graduation rates for students completing two-year degrees, these are three year graduation rates. What this means is that the 2022 graduation rate is for the entering cohort of 2019, the 2021 graduation rate is for the entering cohort of 2018 and so on.

Now I am going to reproduce the original analyst's model, which will be the base model. We model graduation rate as a function of year using the 2016-2022 data and I view the model using summary(). Approximately 33% of the variability in graduation rates can be explained as a function of year. 

```r
base_model <- lm(grad_rate ~ year, data = data)
summary(base_model)
```
Next, I use the predict() function to produce the estimates for 2016-2022 using the base model, which ultimately is the formula GradRate = -31.36 + year*0.0157. This also includes the prediction interval for understanding the potential range of possible values we could expect.

```r
pred.int <- data.frame(predict(base_model, interval = 'prediction'))
```

When I plot the model for stakeholders, I want to show the model, the known data for 2016-2022, and the predictions for 2023-2025. I need to combine my model predictions with the original data using a cbind()

```r
base_data <- cbind(data, pred.int)
```
We're still missing an important part, which is the predictions for the unknown years 2023-2025. To create these, I feed the "new_data" dataframe we created earlier in the predict function along with the model being used for prediction and then turn the output into a dataframe. Like we did before with the 2016-2022 data, I now combine the years 2023-2025 with the predicted graduation rates. I go ahead and add a grad_rate column, which will be missing for now. We then bind the rows of the "base_data", which contains the 2016-2022 grad rates and predictions, with the "fut_data", which contains the predictions for 2023-2025 and missing values for the actual graduation rates since we don't know those. 

```r
fut_pred <- data.frame(predict(base_model, new_data, interval = 'prediction'))
fut_data <- cbind(new_data, fut_pred)
fut_data$grad_rate <- c(NA, NA, NA)
all_data <- rbind(base_data, fut_data)

```

Finally, to complete my dataframe for plotting, I replace the missing graduation rate data for 2023-2025 with the predicted values from 2023-2025.

```r
all_data$grad_rate[8] <- all_data$fit[8]
all_data$grad_rate[9] <- all_data$fit[9]
all_data$grad_rate[10] <- all_data$fit[10]
```

The plot is the primary reference for the stakeholders, which will demonstrate the model itself, the known graduates rates from 2016-2022, and the projections for 2023-2025. We include the lower and upper bounds of the predictions to visualize the uncertainty. 

```r
plot <- all_data |>
  ggplot(aes(year, grad_rate)) +
  geom_point() +
  geom_line(aes(y = fit), color = "blue", linetype = "solid") +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  theme_economist_white() +
  scale_x_continuous(breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) +
  scale_y_continuous(limits = c(0.20, 0.60), breaks = c(.10, .20, .30, .40, .50, .60)) +
  labs(subtitle = "Prediction Based on Graduation Rate ~ Year\n", x = "", y = "Graduation Rate\n", caption = "Assumes graduation rates are a linear function of time") +
  geom_label(aes(label = round(grad_rate, 2)), vjust = -0.5) +
  geom_text(aes(label = "Predictions", x = 2024, y = 0.55)) +
  annotate("rect", xmin = 2022.5, xmax = 2025.5, ymin = 0.2, ymax = .6, alpha = .25, fill = "red")

print(plot)
```
<br>
![alt text](/img/posts/plota_prediction_graduation_rates.png "Predicting College Graduation Rates")
<br>

My biggest gripe about this model is that it does not account for any internal or external variables that plausibly impact graduation rates. I think we can better not just in terms of making accurate predictions but also with justifying the model conceptually for stakeholders. 

# Revised Model

The next model takes into account the correlations between the college's graduation rates from 2016-2022 and national cohort average math SAT scores (r = .41), the Texas unemployment rate (r = .40), and average graduation rates for two-year colleges in Texas (r = .87). We already know the 2023 unemployment rate and the 2020-2022 cohort math SAT scores so this model incorporates more known information that impacts graduation rates. However, assumptions still need to be made for 2023-2025 graduation rates for two-year colleges across Texas and for 2024-2025 unemployment rates. The model uses the 2020, 2021, and 2022 average math SAT scores, uses estimates of unemployment rates of 4.2% and 4.4% for 2024 and 2025 respectively, and assumes an increase by one percentage point per year for the average graduation rate for Texas two-year colleges. Under these assumptions, the college's predicted graduation rates for 2023, 2024, and 2025 are 40%, 41%, and 44% respectively. 

First, I'll create the dataframe we'll use to built the model.

```r
data <- data.frame(year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022), grad_rate = c(.28, .32, .41, .37, .42, .4, .37), texas_grad_rate = c(.186, .217, .233, .249, .258, .257, .251), sat_math_cohort = c(514, 513, 511, 508, 527, 531, 528), unemployment = c(.046, .043, .039, .035, .077, .056, .039))

```

This dataframe contains three additional variables: the average graduation rate for two-year colleges in the state of Texas, the national average math SAT score for each cohort of students, and the Texas unemployment rate. Examining the bivariate correlations between these variables and our college's graduation rate demonstrate that these variables may be useful indicators to improve our model.

```r
cor(data)
```

We then create a dataframe that contains the new years 2023-2025, and our assumptions about the three additional variables. For the average graduation rate for two-year colleges in Texas, we assume a 1 percentage point increase per year. For unemployment, we already know the 2023 Texas unemployment rate and we use official projections of unemployment rates for 2024 and 2025. Finally, we include the average math SAT scores for each cohort of students, which are known values because the graduation rates for 2023/2024/2025 are the 2020, 2021, and 2022 cohorts. 

```r
new_data <- data.frame(year = c(2023, 2024, 2025), texas_grad_rate = c(.261, .271, .281), sat_math_cohort = c(523, 528, 521), unemployment = c(.041, .042, .044))
```

Here we create our revised model to include the new variables and evaluate view the model with summary(). 

```r
better_model <- lm(grad_rate ~ texas_grad_rate + sat_math_cohort + unemployment, data = data)
summary(better_model)
```

Here we create a dataframe that contains the predicted graduation rates for each year 2016-2022 and then we bind those prediction to our original dataframe

```r
pred.int <- data.frame(predict(better_model, interval = 'prediction'))
better_data <- cbind(data, pred.int)
```

Then we need to create a dataframe that contains the predicted values for the new years 2023-2025, combine that information with the years and include missing values for the actual graduation rates, since we don't know those. 

```r
fut_pred2 <- data.frame(predict(better_model, new_data, interval = 'prediction'))
fut_data2 <- cbind(new_data, fut_pred2)
fut_data2$grad_rate <- c(NA, NA, NA)
```

We then combine the unknown years to the known years and pull the predicted values for 2023-2025 into the graduation rates column so that all graduation rates are in the same column. We now have a dataframe that contains 2016-2025, including known college graduation rates for 2016-2022 and predicted college graduation rates for 2023-2025.

```r
all_data2 <- rbind(better_data, fut_data2)
all_data2$grad_rate[8] <- all_data2$fit[8]
all_data2$grad_rate[9] <- all_data2$fit[9]
all_data2$grad_rate[10] <- all_data2$fit[10]
```

Finally, we plot our model, with the predictions and the lower and upper bounds of the predictions for our stakeholders. 

```r
plot <- all_data2 |>
  ggplot(aes(year, grad_rate)) +
  geom_point() +
  geom_line(aes(y = fit), color = "blue", linetype = "solid") +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  theme_economist_white() +
  scale_x_continuous(breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) +
  scale_y_continuous(limits = c(0.10, 0.60), breaks = c(.10, .20, .30, .40, .50, .60)) +
  labs(title = "Predicting College Graduation Rate (2023-2025)\n", subtitle = "Prediction Based on:\nGraduation Rate ~ Cohort MathSAT + TX Unemployment Rate + \nTX Two-year college Graduation Rates\n", x = "", y = "Graduation Rate\n", caption = "Uses math SAT cohort averages of 523, 528, & 521 for 2023-2025\n Uses Texas unemployment rates of 4.1%, 4.2% and 4.4% for 2023-2025\n Assumes + 1 percentage point to Texas two-year college graduation rate each year 2023-2025") +
  geom_label(aes(label = round(grad_rate, 2)), vjust = -0.5) +
  geom_text(aes(label = "Predictions", x = 2024, y = 0.55)) +
  annotate("rect", xmin = 2022.5, xmax = 2025.5, ymin = 0.2, ymax = .6, alpha = .25, fill = "red")

print(plot)
```
<br>
![alt text](/img/posts/plotb_predicting_graduation_rates.png "Predicting College Graduation Rates")
<br>


# Conclusions and Recommendations

Estimating future values depends upon the assumptions made for unknown inputs. Actual graduation rates will depend on the accuracy of the inputs and potential impact of unknown factors, both internal and external to the college. For example, course withdrawal rates are generally higher for online courses compared to traditional courses and performance is generally lower for online courses compared to traditional courses (see [here](https://files.eric.ed.gov/fulltext/EJ1017510.pdf)). Both of these factors will result in a reduction in the overall graduation rate if the percentage of online students increased over the assessed period.

The 2023 prediction is the graduation rate for the “Covid” cohort of 2020, and it strikes me as unreasonable to expect a 6 percentage point increase in graduation rate compared to 2022 (using the base model) and unreasonable to expect a new historical high in 2023 given that the 2020 cohort experienced a shift to hybrid and online learning at a scale that did not exist before. In my opinion, the estimates of 40%, 41%, and 44% are more likely to reflect future reality because they give consideration for the potential impact of known correlates of our college graduation rates, including math ability (see [here](https://www.ppic.org/wp-content/uploads/content/pubs/report/R_701JBR.pdf)), government projections of unemployment rates, and reasonable assumptions about average future graduation rates for two-year colleges in the state of Texas. 
