# Rscript run.R installs any missing packages and opens the app
pkgs <- c("shiny", "bslib", "reactable", "plotly", "htmltools")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
shiny::runApp(launch.browser = TRUE)
