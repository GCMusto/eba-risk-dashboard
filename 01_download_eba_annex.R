download_eba_annex <- function(quarter = NULL, annex_type = c("Data", "KRI"), dest_dir = "data") {

	# Check and load required packages
	pkgs <- c("stringr", "httr", "rvest", "utils")
	missing_pkgs <- pkgs[!pkgs %in% installed.packages()]
	if (length(missing_pkgs)) install.packages(missing_pkgs)
	invisible(lapply(pkgs, library, character.only = TRUE))

	annex_type <- match.arg(annex_type)

	# EBA Risk Dashboard page
	url <- "https://www.eba.europa.eu/risk-analysis-and-data/risk-dashboard"

	# Read the HTML
	page <- read_html(url)

	# Extract all links
	links <- page %>%
	  html_elements("a") %>%
	  html_attr("href")

  	# Define pattern based on annex type
	if (annex_type == "Data") { 
		pattern <- "data%20annex|data annex"
	} else {
		pattern <- "risk%20parameter|risk parameter"
	}

	# Keep only .xlsx links containing 'Statistical' or 'Annex'
	xlsx_links <- links[str_detect(links, "\\.xlsx") & 
			    str_detect(links, regex(pattern, ignore_case = TRUE))]

	# Keep unique and non-missing absolute URLs
	xlsx_links <- unique(na.omit(xlsx_links))
	xlsx_links <- ifelse(str_starts(xlsx_links, "http"), xlsx_links, paste0("https://www.eba.europa.eu", xlsx_links))

	if (length(xlsx_links) == 0) stop("No Statistical Annex Excel files found on the page.")

	# Decode and prepare metadata for searching quarters
	decoded_names <- URLdecode(basename(xlsx_links))

	# If user specifies a quarter, find it
	if (!is.null(quarter)) {

		# Accept both "Q2 2024" and "Q2_2024"
		normalized_quarter <- str_replace_all(quarter, "_", "[_ ]?")
		match_idx <- which(stringr::str_detect(decoded_names, regex(normalized_quarter, ignore_case = TRUE)))

		if (length(match_idx) == 0) {
			cat("No file found for quarter '", quarter, "'.\n",
			    "Use format like 'Q2 2024' or 'Q2_2024'.\n",
			    "Nothing was downloaded.\n", sep = "")
			return(invisible(NULL))
		}
	    	selected_link <- xlsx_links[match_idx[1]]
	  } else {
	    	# Default will be the first link (which should be the latest added)
	    	selected_link <- xlsx_links[1]
	    	quarter <- stringr::str_extract(URLdecode(selected_link), "Q[1-4]\\s20[0-9]{2}")
  	}
	
	# Prepare destination
	if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

	# Build clean filename
	decoded_name <- URLdecode(basename(selected_link))
	file_name <- str_replace_all(decoded_name, "\\s+", "_")
	file_path <- file.path(dest_dir, file_name)

	# Download the file
	cat("Downloading EBA Statistical Annex", if (!is.null(quarter)) paste0("for ", quarter), "...\n")
	GET(selected_link, httr::write_disk(file_path, overwrite = TRUE))
	cat("Saved as:", file_path, "\n")

	invisible(file_path)
}
