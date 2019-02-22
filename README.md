# CAFE_dashboard

This repository contains the code for the dashboard for the CAFE-project.

## Running the dashboard locally

The dashboard is developed open source, which means you can run it locally.

#### Pull the repository

Go in a terminal and execute:

```
https://github.com/Lattice-Works/CAFE-dashboard.git
```

The repository should now be copied to your local machine.
If you have problems at this point, look up documentation about github.

#### Dependencies

To get it running locally, you need to recreate the environment with the appropriate dependencies:

1. Install [RStudio](https://www.rstudio.com/)
2. Install the dependencies, by running [this script](https://github.com/Lattice-Works/shiny/blob/master/install_dependencies.R) in R.

Your local environment should be ready.

#### Running the dashboard locally

If you open either the server.R or ui.R file in RStudio, a button with **Run App** will appear on the top of the upper left window.  Click this button, and the app should start.
If you have problems at this point, look up documentation about shiny.

You should be able to load and visualise your own data with your token.

#### Adding content

If you are somewhat familiar with R/shiny/github, you can add your own content and do a pull request to merge this in to the online dashboard.
