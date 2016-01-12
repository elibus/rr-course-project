fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- "repdata-data-StormData.csv.bz2"

# Download and unzip the dataset
if (!file.exists(destfile)) {
  res <- tryCatch(
    download.file(fileUrl, method = "libcurl", destfile = destfile),
    error = function(e)
      1
  )
}

#bzfile(destfile)
