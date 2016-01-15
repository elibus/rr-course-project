# Required library
require(data.table)
require(lubridate)
require(stringdist)
require(dplyr)
require(lattice)
require(gridExtra)

# Functions
apply_exp <- function(value, exp) {
  switch(
    exp,
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
    "1"={
      return(value * 10);
    },
    "2"={
      return(value * 10^2);
    },
    "3"={
      return(value * 10^3);
    },
    "4"={
      return(value * 10^4);
    },
    "5"={
      return(value * 10^5);
    },
    "6"={
      return(value * 10^6);
    },
    "7"={
      return(value * 10^7);
    },
    "8"={
      return(value * 10^7);
    },
    "9"={
      return(value * 10^7);
    },
    {
      return(value);
    }
  )
}

# Official event types
evtypesFile <- "evtypes.csv"
evtypes <- readLines(evtypesFile)
evtypes <- toupper(evtypes)

# Download & load data
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destFile <- "repdata-data-StormData.csv.bz2"

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
#df <- subset(df, BGN_DATE > as.Date("1996-01-01") )

df$EVTYPE <- toupper(df$EVTYPE)

df$FATALITIES <- as.integer(df$FATALITIES)
df$INJURIES   <- as.integer(df$INJURIES)

df$PROPDMGEXP <- toupper(df$PROPDMGEXP)
df$CROPDMGEXP <- toupper(df$CROPDMGEXP)

df$PROPDMG <- mapply(apply_exp, df$PROPDMG, df$PROPDMGEXP)
df$CROPDMG <- mapply(apply_exp, df$CROPDMG, df$CROPDMGEXP)

# Clean some EVTYPE
df$EVTYPE <- gsub("TSTM", "THUNDERSTORM", df$EVTYPE)
df$EVTYPE <- gsub(".*THUNDERSTORM WIND.*", "THUNDERSTORM WIND", df$EVTYPE)
df$EVTYPE <- gsub("URBAN/SML STREAM FLD", "FLOOD", df$EVTYPE)
df$EVTYPE <- gsub(".*FLOOD.*", "FLOOD", df$EVTYPE)
df$EVTYPE <- gsub("^FOG$", "DENSE FOG", df$EVTYPE)
df$EVTYPE <- gsub("^SNOW$", "HEAVY SNOW", df$EVTYPE)
df$EVTYPE <- gsub("^WIND$", "HIGH WIND", df$EVTYPE)
df$EVTYPE <- gsub(".*SURF.*", "HIGH SURF", df$EVTYPE)

df$EVTYPE <- evtypes[amatch(df$EVTYPE, evtypes, method = "soundex")]

# % of matched events
100 - sum(is.na(df$EVTYPE))*100/nrow(df)

# Damage summary
dmg_by_evtype <-
  df %>%
  group_by(EVTYPE) %>%
  summarise(
    avg_fatalities = mean(FATALITIES),
    avg_injuries   = mean(INJURIES),
    avg_propdmg    = mean(PROPDMG),
    avg_cropdmg    = mean(CROPDMG)
  ) %>%
  arrange(desc(avg_fatalities))

## Plots for threats to human healt
top_evtypes_by_fatalities <- head(dmg_by_evtype, 5)

x_labels <- factor(top_evtypes_by_fatalities$EVTYPE, levels = unique(top_evtypes_by_fatalities$EVTYPE))
fatalities_plot <- barchart(
  avg_fatalities ~ x_labels,
  data=top_evtypes_by_fatalities,
  main = "Fatalities by event type (average) - top 5",
  ylab = "Fatalities"
)


top_evtypes_by_injuries <- head(arrange(dmg_by_evtype, desc(avg_injuries)), 5)
x_labels <- factor(top_evtypes_by_injuries$EVTYPE, levels = unique(top_evtypes_by_injuries$EVTYPE))
injuries_plot <- barchart(
  avg_injuries ~ x_labels,
  data=top_evtypes_by_injuries,
  main = "Injuries by event type (average) - top 5",
  ylab = "Injuries"
)

grid.arrange(fatalities_plot, injuries_plot, ncol=1)


## Plots for damage
top_evtypes_by_economic_dmg <-
  head(
    arrange(
      dmg_by_evtype,
      desc(avg_propdmg+avg_cropdmg)
    ),
    5
  )

x_labels <-
  factor(
    top_evtypes_by_economic_dmg$EVTYPE,
    levels = unique(top_evtypes_by_economic_dmg$EVTYPE)
  )

barchart(
  avg_propdmg + avg_cropdmg ~ x_labels,
  data=top_evtypes_by_economic_dmg,
  stack = TRUE,
  main = "Economic loss for event type (average) - top 5",
  xlab = "Extreme Event Type",
  ylab = "Average loss",
  auto.key=list(
    corner = c(.98, .98),
    text = c("Property dmg", "Crop dmg")
  )
)

