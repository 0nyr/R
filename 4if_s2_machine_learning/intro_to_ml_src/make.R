#!/usr/bin/env R

# Render R markdown to PDF.
# Invoke with:
# > R -q -f make.R --args my_report.Rmd

# load packages
require(rmarkdown)

# require a parameter naming file to render
if (length(args) == 0) {
    stop("Error: missing file operand", call. = TRUE)
} else {
    # read report to render from command line
    for (rmd in commandArgs(trailingOnly = TRUE)) {
        # render Rmd to PDF
        if ( grepl("\\.Rmd$", rmd) && file.exists(rmd)) {
            render(rmd)
        } else {
            print(paste("Ignoring: ", rmd))
        }
    }
}
