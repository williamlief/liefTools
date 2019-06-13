# liefTools
This is a collection of helpful functions that I find useful for data analysis. Most of these functions are very simple, but save me from having to type out the same five lines of code over and over again. As an example, `summarize_mean` takes a dataset and returns the mean value of every variable in it. The function also allows an optional grouping parameter, and will return a wide tibble with one row per variable and one column per group level. The package is fully documented in R, see the help index for a full list of variables. 

Install using 
` devtools::install_github("williamlief/liefTools") `
