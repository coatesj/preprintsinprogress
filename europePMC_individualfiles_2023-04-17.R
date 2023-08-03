library(XML)
library(tidypmc)

# Parsing EuropePMC preprints - individual XML files - to find short ones 
# 2023-04, Jessica Polka w/Chat GTP

# Search Europe PMC with a string such as: (OPEN_ACCESS:y) AND (SRC:"PPR") AND (FIRST_PDATE:[2023-01-01 TO 2023-03-31])
# Extract the .zip file to the dir specified by dir_path below


# Set the directory path to where the XML files are stored
dir_path <- "C:/Users/jessi/Downloads/EuropePMCpreprints/"
csv_path = paste(dir_path, "2021-Q12023.csv", sep = "")

# Get a list of all the XML files in the directory
xml_files <- list.files(dir_path, pattern = "\\.xml$", full.names = TRUE)
numberoffiles <- length(xml_files)

# Initialize empty lists to store the extracted data
dois <- vector(mode = "list", length = numberoffiles)
titles <- vector(mode = "list", length = numberoffiles)
abstracts <- vector(mode = "list", length = numberoffiles)
servers <- vector(mode = "list", length = numberoffiles)
#pub_dates <- list()
word_counts <- vector(mode = "list", length = numberoffiles)
figure_counts <- vector(mode = "list", length = numberoffiles)
table_counts <- vector(mode = "list", length = numberoffiles)
urls <- vector(mode = "list", length = numberoffiles)

# Loop through each XML file and extract the data
for (i in seq_along(xml_files)) {
  
  # Load the XML file
  xmlfile <- xmlParse(xml_files[i])
  
  # Extract the DOI, title, abstract, and publication date
  doi <- xpathSApply(xmlfile, "//front/article-meta/article-id[@pub-id-type='doi']", xmlValue)
  if (!is.character(doi) || length(doi) > 1) {
    doi <- "null"
  }
  title <- xpathSApply(xmlfile, "//front/article-meta/title-group/article-title", xmlValue)
  if (!is.character(title) || length(title) > 1) {
    message(paste(i, "has no proper title"))
    title <- "null"
  }
  abstract <- xpathSApply(xmlfile, "//front/article-meta/abstract", xmlValue)
  if (!is.character(abstract) || length(abstract) > 1) {
    message(paste(i, "has no proper abstract"))
    abstract <- "null"
  }
  
  server <- xpathSApply(xmlfile, "//front/journal-meta/journal-id", xmlValue)
  if (!is.character(server) || length(server) > 1) {
    message(paste(i, "has no proper server"))
    server <- "null"
  }
  pmcid <- xpathSApply(xmlfile, "//front/article-meta/article-id[@pub-id-type='archive']", xmlValue)
  if (!is.character(pmcid) || length(pmcid) > 1) {
    message(paste(i, "has no proper pmcid"))
    pmcid <- "null"
  }

#  pub_date <- xpathSApply(xmlfile, "//front/article-meta/pub-date[@pub-type='epub']", function(x) {
#    paste(x$year, x$month, x$day, sep = "-")
#  })
  
  # Extract the main text and count the words
  main_text <- xpathSApply(xmlfile, "//body", xmlValue)
  if (length(main_text) > 0) {
    word_count <- length(strsplit(main_text, "\\s+")[[1]])
  } else {
    word_count <- 0
  }
   
  
  # Extract the figures and count them
  figures <- xpathApply(xmlfile, "//fig", xmlAttrs)
  figure_count <- length(figures)
  
  # Extract the tables and count them
  tables <- xpathApply(xmlfile, "//table-wrap", xmlAttrs)
  table_count <- length(tables)
  
  # Append the extracted data to the respective lists

  dois[[i]] <- doi
  titles[[i]] <- title
  abstracts[[i]] <- abstract
  servers[[i]] <- server
#  pub_dates[[i]] < && length(x) > 0) x[[1]] else x),- pub_date
  word_counts[[i]] <- word_count
  figure_counts[[i]] <- figure_count
  table_counts[[i]] <- table_count
  urls[[i]] <- paste("https://europepmc.org/article/PPR/", pmcid, sep = "")
}

# Combine the extracted data into a data frame
data <- data.frame(
  DOI <- unlist(dois),
  #  DOI <- unlist(sapply(dois, function(x) if (is.list(x) && length(x) > 0) x[[1]] else x)),
  Title <- unlist(titles),
  Abstract <- sapply(abstracts, function(x) if (is.list(x) && length(x) > 0) x[[1]] else x),
  Server <- unlist(servers),
#  PubDate = unlist(pub_dates),
  WordCount = unlist(word_counts),
  FigureCount = unlist(figure_counts),
  TableCount = unlist(table_counts),
  URL = unlist(urls)
)


# Write the data frame to a CSV file
write.csv(data, file = csv_path, row.names = FALSE)

