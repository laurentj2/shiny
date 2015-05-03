# Gender Predictor

A web app built with [Shiny](http://shiny.rstudio.com/) to predict
gender from first name and birth year using historical data. It uses the
[gender](https://github.com/ropensci/gender) package for R.

## Running this app

The most basic way to run this app is using Shiny on your local machine.
You can run the app from GitHub:

    shiny::runGitHub("gender-predictor", "lmullen")

You can also run this application as a Docker container. This Docker
image extends [rocker/shiny](https://github.com/rocker-org/shiny). The
following command will run the app on port 80.

    docker run -d -p 80:3838 \
      -v /srv/shinylog/:/var/log/ \
      lmullen/gender-predictor
