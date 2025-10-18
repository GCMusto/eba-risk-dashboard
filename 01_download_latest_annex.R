# Load libraries
if(!require("stringr")) install.packages("stringr")
if(!require("utils")) install.packages("utils")
if(!require("httr")) install.packages("httr")
if(!require("rvest")) install.packages("rvest")


# EBA Risk Dashboard page
url <- "https://www.eba.europa.eu/risk-analysis-and-data/risk-dashboard"

# Read the HTML
page <- read_html(url)

# Extract all links
links <- page %>%
  html_elements("a") %>%
  html_attr("href")

# Keep only .xlsx links containing 'Statistical' or 'Annex'
xlsx_links <- links[str_detect(links, "\\.xlsx") & 
                    str_detect(links, regex("Statistical|Annex", ignore_case = TRUE))]

# Keep unique and non-missing absolute URLs
xlsx_links <- xlsx_links[!is.na(xlsx_links)]
xlsx_links <- unique(xlsx_links)
xlsx_links <- ifelse(str_starts(xlsx_links, "http"), xlsx_links, paste0("https://www.eba.europa.eu", xlsx_links))

if (length(xlsx_links) == 0) stop("No Statistical Annex Excel files found on the page.")

# Assume first is the latest
latest_file <- xlsx_links[1]

# Clean file name
decoded_name <- URLdecode(basename(latest_file))
file_name <- str_replace_all(decoded_name, "\\s+", "_")

# Download the file
cat("Downloading:", latest_file, "\n")
GET(latest_file, write_disk(file_name, overwrite = TRUE))
cat("Saved as:", file_name, "\n")
