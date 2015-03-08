# Set working directory
setwd("C:/Users/Steve/Desktop/Coursera/Statistical Inference")

# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("project2.Rmd")
markdownToHTML('project2.md', 'project2.html', options=c("use_xhtml"))
system("pandoc -s project2.html -o project2.pandoc.pdf")


