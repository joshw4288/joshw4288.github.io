---
title: "Estimating Lifetime Return of College Degrees"
layout: post
image: "/posts/image_roi_cover.jpg"
tags: [R, Shiny, Return on Investment]
---
**In this project, I produce a web app using Shiny, which will take a series of inputs and produce an estimated net lifetime return of a college degree. Note that this project uses fictitious data; however, it mirrors a real project I completed for institutional stakeholders in my role as a research scientist.**

# Context

Institutional stakeholders were interested in evaluating the net return on investment of programs at a technical college. As part of marketing and recruitment efforts it would be useful if prospective students could estimate the value of the college degree they were interested in. This web app takes prospective student inputs, retrieves institutional data, and then calculates two metrics: the estimated net lifetime return on investment and the factored graduate return multiple (i.e., the multiplicative return of the cost of a program over a person's working life). 

# Code and Explanation

First, we load necessary libraries. 

```r
library(shiny)
library(tidyverse)
library(readxl)
```

Here, I load the necessary institutional data from an excel file. In real life, this would be pulled directly from a data warehouse. For purposes here, this file contains the name of a two-year associates degree, the cost of the program, and the median first-year wage of graduates from that program. The function call to drop_na() is used to ensure that only programs where we have the necessary wage and cost data are included in the drop-down menu within the web app. 

```r
data <- read_excel("/Users/JDW/Desktop/roi_project/roi_analysis_data.xlsx") |>
  drop_na()
```

This next section defines the user interface (UI) for the app. The theme is pulled from the bslib library and the bs_theme function. The next lines define the title, called "Estimated Return on Investment". Then the p() function call creates an introductory paragraph that explains the web app. Following this, the sidebarLayout defines a section of the app on the left-hand side that contains a panel of slider inputs that allow a prospective student to enter important information needed for the ROI calculations. 

```r
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "spacelab"),
    # Application title
    tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
           tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 'Estimated Return on Investment')),
    p("This calculator estimates the net lifetime economic return for completing a program at Fictitious College. The estimator takes into account your current (or counterfactual) income, the median first year earnings of graduates from the program, 
    your current age, and the cost of the program including loan interest, if applicable."),

    # Sidebar with a slider input for selecting age
    sidebarLayout(
      sidebarPanel(
        sliderInput("age",
                    "Select Your Current Age:",
                    min = 18,
                    max = 70,
                    value = 18),
        
        sliderInput("retirement_age",
                    "Select Your Intended Retirement Age:",
                    min = 50,
                    max = 75,
                    value = 65),
      
        sliderInput("income", 
                      "Select Your Current Income:",
                      min = 0, max = 100000, value = 21000,
                      step = 1000),
        
        sliderInput("interest_rate", 
                        "Select Your Loan Interest Rate:",
                        min = 0, max = 0.20, value = 0.06,
                        step = .01),
        selectInput ("program", "Select the Program of Interest",
                     choices = data$program,
                     selected = data$program[1]),
        width = 6
          ),
      
      # default, not complete
      mainPanel(
        textOutput("resultText"), width = 6)
    ))
```

Note that the sliders accept an input, which are given a name (e.g., "age"), a set of instructions for users (e.g., "Select your current age"), a range of possible values that can be selected, and then default value. The default value for age is set to 18 since this is a typical entering college student. The default for retirement age is set to 65 since this is a typical retirement age. The default for current income is set to $21,000 because this reflects approximaely a full-time job at $10/hr, a reasonable counterfactual income that a prospective student might make if they chose not to pursue a college degree. The loan interest rate defaults to 6%, and the program will default to the first program in the data file. 

The next code chunk defines the server, or the backend of the app, in which there are inputs and then there is an output that will be visible on the web app. The output of the app is defined as as text (resultText) produced by the function call to renderText(). Within the function call are the created inputs age, current_income, interest_rate, selected_program, and retirement_age. All of these inputs are defined by the input sliders that a user utilizes to select values. 

program_info defines two pieces of information that are contained within the data file: the median first year wage of graduates and the program cost. These values are pulled based upon the selected program that a user chooses in the program selector. 

There are two final pieces of the code. The output are defined and then an ouput is printed. A value defined as "fgr" reflects the factored graduate return multiple of the program cost and the value defined as returned_value reflects the lifetime ROI estimate. Note the two formulas, which are criticall important: 

ROI is best defined as an expected return minus a couterfactual return minus the total cost of a program. This is reflected in the return_value, which subtracts current income from the median first year wage of graduates (expected income) and then multiplies this value by the working life of an individual (retirement_age - age) and then subtracts the cost of the program including the cost of interest over a 10 year student loan.

returned_value <- ((median_wage - current_income) * (retirement_age - age)) - (cost*((1+interest_rate)^10)) 

The fgr is similar but instead of substracting the cost, it divides by the cost in order to calculate the multiple of net lifetime income earned compared to the cost of the program.

fgr <- ((median_wage - current_income) * (retirement_age - age)) / (cost * ((1 + interest_rate)^10))

Finally, there is an if-else statement, which will return an indication of negative net ROI if the fgr is below 0 or if the fgr not below 0 then a statement reporting the fgr and the estimated net lifetime return will be displayed. 

```r
server <- function(input, output) {

  output$resultText <- renderText({
    # Create input values
    age <- input$age
    current_income <- input$income
    interest_rate <- input$interest_rate
    selected_program <- input$program
    retirement_age <- input$retirement_age
    
    program_info <- data[data$program == selected_program, c("median_wage2020", "program_cost")]
    median_wage <- program_info$median_wage2020
    cost <- program_info$program_cost
    
    
    fgr <- ((median_wage - current_income) * (retirement_age - age)) / (cost * ((1 + interest_rate)^10))
    returned_value <- ((median_wage - current_income) * (retirement_age - age)) - (cost*((1+interest_rate)^10)) 
    # Return the result
    if (fgr < 0) {
      return("Your selections produce a negative net ROI.")
    } else {
      # Return the result
      return(paste("Completing", paste(selected_program), "will provide an estimated net lifetime return on investment of", paste0("$", round(returned_value), "."), "This return reflects a", paste(round(fgr, 2)), 
                   "multiple of the cost of the program over the course of your working life."))
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
```
# Output 

The last function call runs the application and produces the web app below. 

<br>
![alt text](/img/posts/image_roi_college.png "Estimating Lifetime Return of College Degrees")
<br>
