shinyServer(function(input, output) {

  prediction <- reactive({
    gender(input$name, years = input$year)
  })

  user_name_ts <- reactive({
    if (input$name != "" & !is.na(input$year)) {
      user_name_df <- ssa_national %>%
        filter(name == input$name) %>%
        mutate(ratio_f = female / (male + female) * 100,
               year = year_to_date(year)) %>%
        select(year, ratio_f)
      xts(user_name_df$ratio_f, order.by = user_name_df$year)
    } else {
      NULL
    }
  })

  user_pop_ts <- reactive({
    if (input$name != "" & !is.na(input$year)) {
      user_pop_df <- ssa_national %>%
        mutate(total_name = female + male) %>%
        group_by(year) %>%
        mutate(total_births = sum(total_name)) %>%
        ungroup() %>%
        filter(name == input$name) %>%
        mutate(percentage_all_births = total_name / total_births * 100,
               year = year_to_date(year)) %>%
        select(year, percentage_all_births)
      xts(user_pop_df$percentage_all_births, order.by = user_pop_df$year)
    } else {
      NULL
    }
  })

  interesting_names_ts <- reactive({
    if (length(input$comparison_names) > 0) {
      names_df <- ssa_national %>%
        filter(name %in% input$comparison_names,
               year >= 1920) %>%
        mutate(percent_female = female / (male + female) * 100) %>%
        select(name, year, percent_female) %>%
        spread(name, percent_female)

      xts(names_df[-1], order.by = year_to_date(names_df$year))
    } else {
      NULL
    }

  })

  output$prediction_text <- renderText({
    if (!is.numeric(input$year)) {
      "Please enter your name and birth year to predict your gender and to
      see charts of how your name has changed over time."
    } else if (input$year > 2012 | input$year < 1880) {
      "Please enter a birth year between 1880 and 2012."
    } else {
      result <- prediction()
      if(nrow(result) == 0) {
        "Sorry, that name is not in my data set for that year."
      } else {
        paste0("You are probably ",
               result$gender,
               ", since of the people born in the United States in ",
               input$year,
               " with your name, ",
               result$proportion_female * 100,
               "% were female and ",
               result$proportion_male * 100,
               "% were male.")
      }
    }
  })

  output$user_percent_plot <- renderDygraph({
    ts <- user_name_ts()
    if (!is.xts(ts)) {
      NULL
    }
    else {
      dygraph(ts, main = "Percentage born female with your name",
              group = "user-name",
              height = 300) %>%
        dyEvent(year_to_date(input$year), "Your birth year",
                labelLoc = "bottom") %>%
        dyAxis("y", "% female", valueRange = c(0, 109)) %>%
        dyAxis("x", valueRange = c(1880, 2014)) %>%
        dyRoller(rollPeriod = 3, showRoller = FALSE) %>%
        dyOptions(drawGrid = TRUE,
                  colors = brewer.pal(8, "Dark2")) %>%
        dySeries("V1", label = "% female")
    }
  })

  output$user_popularity_plot <- renderDygraph({
    ts <- user_pop_ts()
    if (is.null(ts)) {
      NULL
    }
    else {
      dygraph(ts, main = "Popularity of your name", group = "user-name",
              height = 300) %>%
        dyEvent(year_to_date(input$year), "Your birth year",
                labelLoc = "bottom") %>%
        dyAxis("y", "% of all births") %>%
        dyAxis("x", valueRange = c(1880, 2014)) %>%
        # dyRoller(rollPeriod = 3, showRoller = FALSE) %>%
        dyOptions(drawGrid = TRUE, includeZero = TRUE,
                  colors = brewer.pal(8, "Dark2")) %>%
        dySeries("V1", label = "% all births")
    }
  })

  output$interesting_names <- renderDygraph({
    ts <- interesting_names_ts()
    if (!is.xts(ts)) {
      NULL
    }
    else {
      dygraph(ts, main = "Gender of names (top = female; bottom = male)") %>%
        dyAxis("y", "% female", valueRange = c(0, 109)) %>%
        dyAxis("x", valueRange = c(1920, 2012)) %>%
        dyRoller(rollPeriod = 3, showRoller = FALSE) %>%
        dyOptions(drawGrid = TRUE,
                  colors = brewer.pal(8, "Dark2")) %>%
        dyHighlight(highlightCircleSize = 3,
                    highlightSeriesBackgroundAlpha = 0.2) %>%
        dyLegend(labelsDiv = "interesting-labels", labelsSeparateLines = TRUE)
    }
  })

})
