# Author
#       Brandon Dey 
#
# Date
#     10.6.18
#
# Purpose
#       Scrape articles from ODSC

#################
## ENVIRONMENT ##
#################

# libraries
library(htmltab)
library(tidyverse)
library(pdftools)
library(tm)

# harvestable url
url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"

# get table of all packages from CRAN
htmltab(doc = url, which = '/html/body/table') -> r_packs

###############################
## get package documentation ##
###############################
# urls follow this format: https://cran.r-project.org/web/packages/PACKAGENAME/index.html
r_packs <- r_packs %>% mutate(Date = as.Date(Date, "%Y-%m-%d"), 
                              yr = lubridate::year(Date), 
                              pdf_url = paste("https://cran.r-project.org/web/packages/",Package,"/", Package, ".pdf", sep = "")) 

###############################
## # download and save all pdfs
###############################
setwd("./All R Package Docs")
for (p in seq_along(r_packs$Package)){
  download.file(url = r_packs[p, "pdf_url"], 
                destfile = r_packs[p, "pdf_name"], 
                quiet = T,
                method = 'auto', 
                mode = "wb",
                cacheOK = TRUE, 
                extra = getOption("download.file.extra"))
  
}
