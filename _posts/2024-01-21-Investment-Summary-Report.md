---
title: "Annual Investment Performance Summary"
layout: post
image: "/posts/investment_performance_image.jpg"
tags: [R, Finance, Trends, Investment, Stock Market]
---

**I report on the returns on an investment portfolio and provide a comparative analysis of returns, volatility, and the information ratio as a metric of risk-adjusted return.**

# Introduction

This brief report provides an overview of an investment portfolio's 2023 performance and compares that performance to a low-cost index fund that tracks the S&P500 (SPY). 

First, I load the necessary libraries and load the data for the portfolio and the benchmark (SPY). The loaded data contains the daily return, the cumulative return, and the closing price.

```r
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggthemes)

SPY_data <- read.csv("/Users//JDW//Desktop/Portfolio Projects/Portfolio Review/2023//SPY2023.csv", header = TRUE)
SPY_data$Date <- as.Date(SPY_data$Date, "%m/%d/%y")
SPY_data <- SPY_data %>%
                  select(c("Date", "daily_return", "cum_return", "Close"))

portfolio_data <- read.csv("/Users//JDW//Desktop/Portfolio Projects/Portfolio Review/2023//portfolio2023.csv", header = TRUE)
portfolio_data$Date <- as.Date(portfolio_data$Date, "%m/%d/%y")
portfolio_data <- portfolio_data %>%
                  select(c("Date", "daily_return", "cum_return", "Close"))

```
Note that I concert the date column to a date type, which we need in order to filter for specific time periods and to plot the trends over time. 

Then I join the two dataframes by date so all data is contained in a single dataframe.

```r
all_data <- SPY_data %>%
                inner_join(portfolio_data, by = "Date") %>%
                rename(spy_daily = daily_return.x, spy_cumulative = cum_return.x, spy_close = Close.x, 
                       portfolio_daily = daily_return.y, portfolio_cumulative = cum_return.y, portfolio_close = Close.y)

```


# Performance

## 2023 Portfolio Comparison

In this first section I compare the performance of the investment porfolio for 2023 against the 2023 performance of SPY, a low cost exchange-traded fund that track the S&P500 index.

Here I create a table that filters the data to include only 2023, beginning with the last trading day of 2022 and running to the last trading day of 2023. I select the column that contains the closing values and then pivot the table such that there is a single column that contains the closing value, a single column that identifies the date, and a column that identifies the portfolio. 

```r
performance_table <- all_data %>%
  filter(Date == '2022-12-30' | Date == '2023-12-29') %>%
  select(Date, spy_close, portfolio_close) %>%
  pivot_longer(cols = c("spy_close", "portfolio_close"), 
               names_to = "Portfolio", values_to = "Close")
```

Then I take the long format table I just created and use a pivot wider so that I have a summary table with the portfolio, and then columns for the beginning and ending dates which contains the beginning values and ending values of the portfolios. These values reflect the adjusted values, which account for stock splits and dividends.

I go ahead and rename the columns for clarity. 

```r
performance_table_wide <- performance_table %>%
  pivot_wider(names_from = "Date", values_from = "Close")

names(performance_table_wide)[3] ="post"
names(performance_table_wide)[2] ="pre"

performance_table_wide$Portfolio <- factor(performance_table_wide$Portfolio)
performance_table_wide$Portfolio <- recode(performance_table_wide$Portfolio, spy_close = "SPY", portfolio_close = "Your Return")
```
Now we are in a position to calculate the 2023 return for each portfolio and plot those values for comparison. I use group by portfolio, calculate the percentage return using (post-pre)/pre and then plot the performance for each, applying useful text and labels on the plot for clarity. The investment portfolio returned 27.8% in 2023 compared to the benchmark of 26.2%!

```r
return_comparison <- performance_table_wide %>%
  group_by(Portfolio) %>%
  mutate(performance = (post-pre)/pre) %>%
  ggplot(aes(x = reorder(Portfolio, performance), y = performance)) +
  geom_col() +
  theme_economist() +
  labs(x = "", y = "Return, %\n", title = "2023 Return Comparison", caption = "Returns are based on adjusted close") +
  scale_y_continuous(labels = scales::percent, limits = c(0, .30), breaks = c(0, .05, .10, .15, .20, .25, .30)) +
  geom_text(aes(label = scales::percent(round(performance, digits = 4))), vjust = -0.5)
  
return_comparison
```

<br>
![alt text](/img/posts/return_comparison.jpg "2023 Investment Return Comparison")
<br>

## 2023 Cumulative Return Comparison

Next, I want to view the performance over time so that we can get a better sense of the pattern of performance throughout the year. 

I use ggplot to plot the cumulative portfolio performance by day for the portfolio and the benchmark and for clarity I add a horizontal bar at the annual performance value to clearly show the annual difference. 

```r

Performance_Graph <- ggplot(all_data, aes(x = Date, y = portfolio_cumulative)) + geom_line(aes(color = "Your Portfolio")) +
                           geom_line(aes(x = Date, y = spy_cumulative, color = "SPY")) +
                           theme_economist() + 
  xlab("\nDate") + 
  ylab("Time Weighted Return\n") + 
                           guides(color=guide_legend("Portfolio")) + labs(title = "2023 Cumulative Performance", caption = "Returns are based on adjusted close") +
                           scale_x_date(date_breaks = "1 month") + scale_y_continuous(label = scales::percent, limits = c(0, .30), breaks = c(0, .05, .10, .15, .20, .25, .30)) + scale_color_manual(values = c("blue","red")) +
  geom_hline(yintercept = all_data$portfolio_cumulative[250], color = "red") +
  geom_hline(yintercept = all_data$spy_cumulative[250], color = "blue") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

Performance_Graph
```

<br>
![alt text](/img/posts/performance_graph.jpg "2023 Cumulative Performance")
<br>

# Volatility

Standard deviation indicates the spread of asset prices from their average price. The larger the standard deviation, the larger the swings in day-to-day prices. The smaller the standard deviation, the smaller the swings in day-to-day prices. Typically, higher performance comes a cost of increased volatility and we can see this by comparing the average daily swing in value. 

Here we pivot the table so that we have one column that provides the daily return and then a column that specifies the portfolio. We group the data by portfolio and summarize the volatility as the standard deviation of the daily return. We can clearly see that the investment portfolio does have greater variability than the benchmark.

## Avg. Volatility
```r
summarize_risk <- all_data %>%
  pivot_longer(c("portfolio_daily", "spy_daily"), names_to = "portfolio", values_to = "daily_return")
summarize_risk$portfolio <- factor(summarize_risk$portfolio)

volatility_plot <- summarize_risk %>%
  group_by(portfolio) %>%
  summarize(volatility = sd(daily_return)) %>%
  ggplot(aes(reorder(portfolio, volatility), volatility)) + geom_col(show.legend = FALSE) + labs(title = "Daily Volatility Comparison, %", y = "Standard Deviation of Daily Return\n", x = "") + theme_economist() + scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = c("SPY", "Your Portfolio")) +
  geom_text(aes(label = scales::percent(round(volatility, digits = 4))), vjust = -0.5)

#summarize_risk %>%
 #group_by(portfolio) %>%
  #summarize(volatility = sd(daily_return))

volatility_plot
```

<br>
![alt text](/img/posts/volatility.jpg "2023 Daily Volatility Comparison")
<br>

## vs. SPY

Another useful way of viewing the volatility is to view it over time for each portfolio. In this graph it's easy to see that the investment portfolio has more extreme highs and lows in daily returns. 

```r
daily_volatility <- ggplot(all_data, aes(x = Date, y = portfolio_daily)) + geom_line(aes(color = "Your Portfolio")) +
                           geom_line(aes(x = Date, y = spy_daily, color = "SPY"), alpha = 0.40) +
                           theme_economist() + xlab("Date") + ylab("Daily Return (%)") + 
                           guides(color=guide_legend("Portfolio")) + labs(title = "Daily Volatility Compared to SPY") +
                           scale_x_date(date_breaks = "1 month") + scale_y_continuous(label = scales::percent) + scale_color_manual(values = c("blue", "red")) + theme(axis.text.x=element_text(angle=90, hjust=1))

daily_volatility
```

<br>
![alt text](/img/posts/daily_volatility.jpg "2023 Daily Volatility Compared to SPY")
<br>

## Information Ratio

The Information Ratio is a metric of risk adjusted return. Is the additional annual gain from the current strategy worth the additional volatility? The information ratio compares the performance of the current portfolio to the performance of SPY adjusted for the additional daily volatility of the current portfolio. The information ratio of 3.73 indicates that the current portfolio provides an additional return 3.73x the additional volatility, which is considered exceptional. 

```r
all_data <- all_data |>
  mutate(daily_difference = portfolio_daily - spy_daily)

std_difference <- sd(all_data$daily_difference)

portfolio_ir <- (.278 - .262)/std_difference
```

# Summary

The investment portfolio returned 27.8% in 2023 compared to a benchmark return (SPY) of 26.2%. The additional return (+1.6) came at a cost of a marginal increase in volatility. The information ratio of 3.73 suggests that the risk-adjusted return relative to SPY is exceptional. 
