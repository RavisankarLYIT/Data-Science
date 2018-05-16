library(dplyr)
# Import all CSV files from the Crime Data folder
crime_data_dir <- "Data\\CrimeData\\"
files_list <- list.files(path = crime_data_dir, pattern = "*.csv", recursive = TRUE)
load_files <- function(base_dir_path, csv_file_names) {
    crime_data_df <- NULL
    for (csv_file in csv_file_names) {
        crime_df <- read.csv(paste(crime_data_dir, csv_file, sep = ""), stringsAsFactors = FALSE)
        crime_data_df <- rbind(crime_data_df, crime_df)
    }
    return(crime_data_df)
}
all_crime_data_df <- load_files(crime_data_dir, files_list)
all_crime_data_df$Location <- trimws(x = all_crime_data_df$Location, which = c("both"))
write.csv(all_crime_data_df, file = "AllNICrimeData.csv", row.names = FALSE)
str(all_crime_data_df)
count(all_crime_data_df)
head(all_crime_data_df)
#all_crime_data_df <- read.csv("allnicrimedata.csv", stringsAsFactors = FALSE)
post_codes_data_df <- read.csv("Data\\PostCodes\\CleanNIPostcodeData.csv", stringsAsFactors = FALSE)
post_codes_data_df <- post_codes_data_df[, c("PrimaryThorfare", "Postcode", "Town", "County")]
post_codes_data_df$PrimaryThorfare <- trimws(x = post_codes_data_df$PrimaryThorfare, which = c("both"))
post_codes_data_df$Postcode <- trimws(x = post_codes_data_df$Postcode, which = c("both"))

post_codes_data_df <- unique(post_codes_data_df)
post_code_popularity <- as.data.frame(table(unlist(post_codes_data_df$Postcode)))
colnames(post_code_popularity) <- c("PostCode", "Occurences")
post_code_popularity <- post_code_popularity[complete.cases(post_code_popularity),]
post_code_popularity <- post_code_popularity[post_code_popularity$PostCode != "None",]
print("Total number of rows in CrimeData set: ")
nrow(all_crime_data_df)
str(all_crime_data_df)
all_crime_data_df <- within(all_crime_data_df, rm("Crime.ID", "Reported.by", "Falls.within", "LSOA.code", "LSOA.name", "Last.outcome.category", "Context"))
str(all_crime_data_df)
head(all_crime_data_df)
all_crime_data_df[, "Crime.type"] <- as.factor(all_crime_data_df[, "Crime.type"])
str(all_crime_data_df)
head(all_crime_data_df)
crime_types <- unique(all_crime_data_df[, "Crime.type"])
crime_types
str(all_crime_data_df)
all_crime_data_df$Location <- trimws(x = gsub("On or near", "", all_crime_data_df$Location), which = c("both"))
all_crime_data_df$Location <- toupper(all_crime_data_df$Location)
all_crime_data_df$Location[all_crime_data_df$Location == ""] <- "Missing"
head(all_crime_data_df)

find_a_postcode <- function(location) {
    post_code <- location
    if (!is.null(location)) {
        post_codes <- post_codes_data_df$Postcode[post_codes_data_df$PrimaryThorfare == location]
        post_codes <- unlist(strsplit(post_codes, " "))
        if (!is.null(post_codes)) {
            if (length(post_codes) > 1) {
                filtered_post_codes <- post_code_popularity[is.element(post_code_popularity$PostCode, post_codes),]
                filtered_post_codes <- filtered_post_codes[complete.cases(filtered_post_codes),]
                complete.cases(filtered_post_codes)
                post_code <- as.character(filtered_post_codes[1, "PostCode"])
                occurence <- as.integer(filtered_post_codes[1, "Occurences"])
                if (! is.na(post_code)) {
                    for (row in 1:nrow(filtered_post_codes)) {
                        if (as.integer(filtered_post_codes[row, "Occurences"]) > occurence) {
                            post_code <- as.character(filtered_post_codes[row, "PostCode"])

                        }
                    }
                }
            } else {
                post_code <- as.character(post_codes[1])
            }

        }

    }
    return(post_code)
}

append_post_codes <- function() {
    for (index in 1:nrow(all_crime_data_df)) {
        location <- all_crime_data_df[index, "Location"]
        post_code <- all_crime_data_df$PostCode[all_crime_data_df$Location == location]

        if (!(is.null(location) | is.null(post_code))) {
            all_crime_data_df$PostCode[all_crime_data_df$Location == location] <- post_code
        }
    }
}


location_post_code_df <- data.frame(Location = all_crime_data_df[!duplicated(all_crime_data_df[, c('Location')]), 'Location'], stringsAsFactors = FALSE)
location_post_code_df$PostCode <- apply(location_post_code_df, 1, find_a_postcode)
str(all_crime_data_df)
head(all_crime_data_df)

all_crime_data_df$PostCode <- ""
for (index in 1:nrow(location_post_code_df)) {
    location <- location_post_code_df[index, "Location"]
    post_code <- location_post_code_df[index, "PostCode"]
    if (!(is.null(location) | is.null(post_code))) {
        all_crime_data_df$PostCode[all_crime_data_df$Location == location] <- post_code
    }
}
str(all_crime_data_df)
head(all_crime_data_df)

unique_locations_df <- all_crime_data_df[!duplicated(all_crime_data_df[, c('Longitude', 'Latitude', 'Location')]), c('Location', 'Longitude', 'Latitude')]
location_missing_df <- unique_locations_df[unique_locations_df$Location == "Missing" | unique_locations_df$Location == "NO LOCATION",]
unique_locations_df <- unique_locations_df[unique_locations_df$Location != "Missing" & unique_locations_df$Location != "NO LOCATION",]
unique_locations_df <- unique_locations_df[complete.cases(unique_locations_df),]
tidy_location <- function(lat, long, min_dist) {
    unique_locations_df$Distance <- ""
    for (index in 1:nrow(unique_locations_df)) {
        result <- sqrt((abs(lat - unique_locations_df[index, 'Latitude']) ^ 2) + (abs(long - unique_locations_df[index, 'Longitude']) ^ 2))
        location <- unique_locations_df[index, 'Location']
        if (!(is.null(result) | is.na(result)) & result <= min_dist) {
            return (location)
        }
        unique_locations_df[index, 'Distance'] <- result
    }
    unique_locations_df <- unique_locations_df[order(unique_locations_df[, 'Distance']),]
    return(unique_locations_df[1, 'Location'])

}

print(nrow(location_missing_df))
for (index in 1:nrow(location_missing_df)) {
    lat <- location_missing_df[index, "Latitude"]
    long <- location_missing_df[index, "Longitude"]
    distance <- tidy_location(lat, long, 75)
    all_crime_data_df$Location[all_crime_data_df$Latitude == lat & all_crime_data_df$Longitude == long & (all_crime_data_df$Location == "Missing" | all_crime_data_df$Location == "NO LOCATION")] <- distance
}
str(all_crime_data_df)

#all_crime_data_df <- subset(all_crime_data_df, Latitude != NA & Longitude != NA & Location != "NO LOCATION" & PostCode != "NO LOCATION")
#write.csv(all_crime_data_df, file = "tempMissingData.csv", row.names = FALSE)

all_crime_data_df <- subset(all_crime_data_df, Location != "NO LOCATION")
all_crime_data_df$Town <- ""
all_crime_data_df$County <- ""
post_codes_unique <- unique(all_crime_data_df[, c("PostCode")])
post_codes_data_df <- post_codes_data_df[complete.cases(post_codes_data_df),]
post_codes_data_temp_df <- unique(post_codes_data_df[, c("Postcode", "Town", "County")])
for (post_code in post_codes_unique) {
    #temp_data_df[temp_data_df$PostCode == post_code] <- 
    result <- post_codes_data_temp_df[post_codes_data_temp_df$Postcode == post_code, c("Postcode", "Town", "County")]
    result <- result[complete.cases(result),]
    if (nrow(result) > 0) {
        all_crime_data_df$Town[temp_data_df$PostCode == post_code] <- result[1, "Town"]
        all_crime_data_df$County[temp_data_df$PostCode == post_code] <- result[1, "County"]
    }
}
all_crime_data_df <- subset(all_crime_data_df, PostCode != "Missing")
str(all_crime_data_df)
head(all_crime_data_df)
write.csv(all_crime_data_df, file = "FinalNICrimeData.csv", row.names = FALSE)

# Without using PIPES
results_df <- all_crime_data_df[grep("STRABANE", all_crime_data_df$Town),]
results_df <- results_df[grep("BT82", all_crime_data_df$PostCode),]
results_df[1:10,]
#using PIPES
all_crime_data_df %>%
    filter(grepl("STRABANE", Town)) %>%
    filter(grepl("BT82", PostCode)) %>%
    slice(1:10)
