library("rvest")
library("magrittr")
# URL of the website we are scraping
url <- 'https://www.garda.ie/en/Crime/'
# Reading the HTML code from the website
web_page <- read_html(url)
# Quick look at the contents of web_page
head(web_page)
str(web_page)
# Extracting the crime categories from the webpage
crime_cat_data_html <- html_nodes(web_page, '#aspnetForm > div.document-list-container.clearfix > div:nth-child(n) > a > h3')
# Quick look at the crime categories
head(crime_cat_data_html)
# Conevrting HTML nodes to HTML text
crime_cat_data <- # Extracting the crime categories from the webpage
    crime_cat_data_html %>%
    # Conevrting HTML nodes to HTML text
    html_text(crime_cat_data_html) %>%
    # Remove trailing and leading spaces from the text
    trimws() %>%
    # Convert the list into a vector
    unlist()


# Extracting the crime categories disclaimers from the webpage
disclaimer_data_html <- html_nodes(web_page, '#aspnetForm > div.document-list-container.clearfix > div:nth-child(n) > a > p')
# Quick look at the crime categories disclaimers
head(disclaimer_data_html)
# Conevrting HTML nodes to HTML text
disclaimer_data <- html_text(disclaimer_data_html)
# Remove trailing and leading spaces from the text
disclaimer_data <- gsub(' $', '', gsub(' ^', '', disclaimer_data))
crime_info_df <- data.frame(crime_cat_data, disclaimer_data)
colnames(crime_info_df) <- c('Crime Category', 'Disclaimer')
str(crime_info_df)
head(crime_info_df)