# Required library
require(data.table)

# Download & load data
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destFile <- "repdata-data-StormData.csv.bz2"
csvFile  <- "repdata-data-StormData.csv"

# Download and unzip the dataset
if (!file.exists(destFile)) {
  res <- tryCatch(
    download.file(fileUrl, method = "libcurl", destfile = destFile),
    error = function(e)
      1
  )
}

# Read data - only relevant columns
colClasses = c(
  rep("NULL", 7),
  "character",  # EVTYPE
  rep("NULL", 14),
  rep("numeric", 3),
  "character", # PROPDMGEXP: Factor w/ 19 levels
  "numeric",
  "character", # CROPDMGEXP: Factor w/ 9 levels
  rep("NULL", 9)
)

df <- read.table(
  bzfile(destFile),
  sep = ",",
  header = FALSE,
  quote = "\"",
  colClasses = colClasses,
  skip = 290000
)

colnames(df) <- c(
  "EVTYPE",
  "FATALITIES",
  "INJURIES",
  "PROPDMG",
  "PROPDMGEXP",
  "CROPDMG",
  "CROPDMGEXP"
)

df$EVTYPE <- factor(df$EVTYPE)
df$F <- as.integer(df$F)
df$FATALITIES <- as.integer(df$FATALITIES)
df$INJURIES   <- as.integer(df$INJURIES)

df$PROPDMGEXP <- toupper(df$PROPDMGEXP)
df$PROPDMGEXP <- factor(df$PROPDMGEXP)

df$CROPDMGEXP <- toupper(df$CROPDMGEXP)
df$CROPDMGEXP <- factor(df$CROPDMGEXP)


































my_conversion <- function(value, multiplier) {
  switch(multiplier,
         H={
           return(value * 100);
         },
         K={
           return(value * 1000);
         },
         M={
           return(value * 1000000);
         },
         B={
           return(value * 1000000000);
         },
         "0"={
           return(value);
         },
         "?"={
           return(value);
         },
         "-"={
           return(value / 10);
         },
         "+"={
           return(value);
         },
         "1"={
           return(value * 10);
         },
         "2"={
           return(value * 100);
         },
         "3"={
           return(value * 1000);
         },
         "4"={
           return(value * 10000);
         },
         "5"={
           return(value * 100000);
         },
         "6"={
           return(value * 1000000);
         },
         "7"={
           return(value * 10000000);
         },
         "8"={
           return(value * 100000000);
         },
         "9"={
           return(value * -100000000);
         }
  )
}
