library(shiny)
library(shinythemes)
library(dplyr)
library(genderdata)
library(gender)
library(dygraphs)
library(xts)
library(RColorBrewer)
library(tidyr)
data("ssa_national")
source("helpers.R")

shinyUI(fluidPage(
  title = "Gender Predictor",
  theme = shinytheme("flatly"),
  tags$head(includeCSS("www/style.css")),
  h1("Predict your gender", align = "center"),
  h3("from your first name and birth year using historical data.",
     align = "center"),
  fluidRow(
    column(4, offset = 3, selectizeInput("name", "Your first name:", all_names)),
    column(2, numericInput("year", "Year you were born:", NULL,
                        min = 1880, max = 2012, step = 1))
    # column(2, div(id = "submit-button", submitButton("Predict your gender")))
  ),
  fluidRow(
    column(6, offset = 3, div(id = "prediction", class = "lead",
       textOutput("prediction_text"))
  )),
  fluidRow(
    column(5, offset = 1, dygraphOutput("user_percent_plot")),
    column(5, dygraphOutput("user_popularity_plot"))
  ),
  fluidRow(
    column(6, offset = 3,
      h3("See how the genders of other names change over time",
         align = "center", id = "cf-header"),
      p("The gender commonly associated with names changes over times. In the
        chart below, names were thought of as female when they appear near the
        top, and male when they appear near the bottom. You can change the names
        that are displayed below.")
    )
  ),
  fluidRow(
    column(7, offset = 2,
      dygraphOutput("interesting_names")
           ),
    column(1, div(id = "interesting-labels"))
  ),
  fluidRow(
    column(8, offset = 2,
      selectizeInput("comparison_names", "Names to compare", all_names,
                     selected = c("madison", "jordan", "hillary", "leslie",
                                  "jan", "addison", "sydney", "monroe"),
                     multiple = TRUE))
  ),
  fluidRow(
    column(8, offset = 2, includeHTML("www/explainer.html"))
  ),
  fluidRow(
  )
))
