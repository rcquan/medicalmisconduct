# Check if file exists in working directory
# If not, downloads and reads Medical Misconduct dataset

fileName = "medMis.csv"
if (!file.exists(fileName)){
        url <- "https://health.data.ny.gov/api/views/ebmi-8ctw/rows.csv?accessType=DOWNLOAD"
        fileName <- "medMis.csv"
        download.file(url, fileName, method = "curl")
}
medMis <- read.csv(fileName)

# Exploratory Data Analysis
str(medMis)

table(medMis$Year.of.Birth, useNA = "ifany") # 192315, NA = 1610

# Multiple Offenders
with(medMis, table(medMis$Name)[table(medMis$Name) > 1]) # table of freqs

multi <- as.data.frame(table(medMis$Name))
colnames(multi) <- c("Name", "Name.Freq")
medMis <- merge(medMis, multi) # adds "Name.Freq" as column to main data frame

# Time-Series Analysis

# Convert variable from factor into "Date" class
medMis$Effective.Date <- as.character(medMis$Effective.Date)
medMis$Effective.Date <- as.POSIXct(medMis$Effective.Date, format = "%m/%d/%Y")
class(medMis$Effective.Date)

# Create extensible time-series (xts) object from data frame
medMis_xts <- as.xts(medMis, order.by = medMis$Effective.Date)
head(medMis_xts, n = 1)

# Order dataset by Effective.Date from 1990-2014
medMis <- (medMis[order(medMis$Effective.Date),])

# Create time-series data (yearly, montly, daily) using Effective.Date
require(xts)
medMis_year <- as.data.frame(table(format(medMis$Effective.Date, "%Y-01-01")), stringsAsFactors = FALSE)
medMis_month <- as.data.frame(table(format(medMis$Effective.Date, "%Y-%m-01")), stringsAsFactors = FALSE)
medMis_day <- as.data.frame(table(format(medMis$Effective.Date, "%Y-%m-%d")), stringsAsFactors = FALSE)

# Change column name to "Time" and convert to date-time class
colnames(medMis_month)[1] <-"Time"   
medMis_month$Time <- as.POSIXct(medMis_month$Time)

colnames(medMis_year)[1] <- "Time"
medMis_year$Time <- as.POSIXct(medMis_year$Time)

# Time-series plot
require(ggplot2)
require(scales)

# By month
m = ggplot(data = medMis_month, aes(x = Time, y = Freq)) + geom_line()
m + scale_x_datetime(labels = date_format("%Y"), breaks = date_breaks("years"))

# By year
m = ggplot(data = medMis_year, aes(x = Time, y = Freq)) + geom_line()
m + scale_x_datetime(labels = date_format("%Y"), breaks = date_breaks("years"))




# Subset by month, average monthly change
# Subset by year, average yearly change
# Graph by frequency, change, etc.


###########################################################

# Changes first column of data frame to "Date" and converts
# column to POSIXct class in prep for plotting
foo <- function(data){
        colnames(data)[1] <- "Date"
        data$Date <- as.POSIXct(as.Date(data$Date))
}

mapply(foo, medMis_ts)

require(ggplot2)
require(scales)



m = ggplot(data = medMis_ts, aes(x = Time, y = Freq)) + geom_line()
m + scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months"))
m + scale_x_datetime(labels = date_format("%Y"), breaks = date_breaks("years"))


require(xts)
xtsible(medMis_ts)

#Read data in
#convert df to xts
#graph