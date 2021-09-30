# Example Git Repository for Code Horizons Course

This repository contains some data and code that will be used as an example for the [Code Horizons course](https://codehorizons.com/) on [Using GitHub for Data Analysis](https://codehorizons.com/Seminars/github-for-data-analysis/). A copy ("fork") of this repository will be provided for you in the GitHub Classroom for the course. I will use it for demonstration purposes in the class, and you can feel free to play around with it to test things out.

The data I am using for this example repository come from the 2016 panel of the [American National Election Studies](https://electionstudies.org/) (ANES). More information about the data source is provided in the `input` directory. The output directory contains the analytical data saved as an RData object named `politics.RData`. 

The `analysis.Rmd` file is an R Markdown with some analysis of this data source. To ensure that you have the libraries installed that the analysis requires, I recommend sourcing the `check_packages.R` script first.
