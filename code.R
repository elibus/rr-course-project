# Required library
require(data.table)
require(lubridate)
require(stringdist)
require(dplyr)

# Functions
apply_exp <- function(value, exp) {
  exp <- as.character(exp)
  switch(
    exp,
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
       {
         return(value);
       }
  )
}

# Download & load data
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destFile <- "repdata-data-StormData.csv.bz2"
csvFile  <- "repdata-data-StormData.csv"

evtypesFile <- "evtypes.csv"

evtypes <- readLines(evtypesFile)
toupper(evtypes)

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
  "NULL",
  "character",
  rep("NULL", 5),
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
  header = TRUE,
  quote = "\"",
  colClasses = colClasses
)

colnames(df) <- c(
  "BGN_DATE",
  "EVTYPE",
  "FATALITIES",
  "INJURIES",
  "PROPDMG",
  "PROPDMGEXP",
  "CROPDMG",
  "CROPDMGEXP"
)

# df$BGN_DATE <- mdy_hms(df$BGN_DATE)
df$BGN_DATE <- as.Date(df$BGN_DATE, "%m/%d/%Y")
# df <- subset(df, BGN_DATE > as.Date("1996-01-01") )

df$EVTYPE <- factor(toupper(df$EVTYPE))

df$FATALITIES <- as.integer(df$FATALITIES)
df$INJURIES   <- as.integer(df$INJURIES)

df$PROPDMGEXP <- factor(toupper(df$PROPDMGEXP))

df$CROPDMGEXP <- factor(toupper(df$CROPDMGEXP))

df$PROPDMG <- mapply(apply_exp, df$PROPDMG, df$PROPDMGEXP)
df$CROPDMG <- mapply(apply_exp, df$CROPDMG, df$CROPDMGEXP)
































