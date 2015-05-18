loadPackages <- function() {
# Load required packages for the analysis of NOAA weather data.
#
# Input:  None.
# Output: None.
    
    library(ggplot2)
    library(dplyr)
    library(gridExtra)
    library(reshape2)
    require(RColorBrewer)
}

readData <- function (directory) {
# This function reads NOAA weather data from the disk.
#
# Input:  The directory containing the file of weather data.
# Output: A data frame containing the severe weather data set.
    
    cwd <- getwd()
    setwd("/Users/mgast/Dropbox/data-science-specialization/5-reproducible-research/RepData-StormProject")
    raw.df <- read.csv("repdata-data-StormData.csv")
    setwd(cwd)
    raw.df 
}


countNAs <- function (df) {
# This function counts the number of NAs in a data frame per column, and returns the columns that
# have non-zero numbers of NAs.
#
# Input:  A data frame
# Output: A list of column names and the NA count in each column.  No output is printed.

    count.na <- as.data.frame(colSums(is.na(df)))
    names(count.na) <- c("count")

    # Only return columns that have NAs, filter out zero columns.
    count.na <- subset(count.na, count>0)
    count.na
}

exponentTranslator <- function (exp) {
#
#    Translate an exponent to a power of 10.
#
# Input:  A text exponent character.  The text may be a number 0-9, the
#         letter H for hundred, K for thousand, M for million, or B for
#         billion.
# Output: A numeric exponent.  The number is either the text number
#         given as input, 2 for hundreds, 3 for thousands, 6 for million,
#         9 for billion, and the number 1 in the case of any other
#         character.
    
    exp <- as.character(exp)
    if (exp %in% c("0","1","2","3","4","5","6","7","8","9")) {
        return(as.numeric(exp))
    }
    if (exp %in% c("h","H")) {
        return(2)
    }
    if (exp %in% c("k","K")) {
        return(3)
    }
    if (exp %in% c("m","M")) {
        return(6)
    }
    if (exp %in% c("b","B")) {
        return(9)
    }
    if (exp == "?") {
        return(1)
    }
    if (exp %in% c("-","+")) {
        # This is probably not right, but close enough
        return(1)
    }
    return(1)
}


topEvents <- function (df, num) {
# This function prints out the top N events in a data frame.
#
# Input:  A data frame and number are given as input.
# Output: A data frame is returned with the specified number
#         of top events.  The determination of a top event
#         is made purely by the number of occurrences, not
#         the effects of any of the events.

    if (missing(num)) {
        num <- 10
    }
    evtype.table <- table(df$event)
    rt.df <- head(sort(evtype.table,decreasing=TRUE),n=num)
    rt.df
}

# three of the top events are the same! (TSTM WIND, THUNDERSTORM WIND, THUNDERSTORM WINDS)

mergeEvents <- function (df, newEvent, ...  ) {
    
# This function combines many event types together.  The motivation
# for the function is that there are often events with slightly
# different text names that mean exactly the same thing.  In the NOAA
# data set, the names TSTM WIND, THUNDERSTORM WIND, and THUNDERSTORM
# WINDS all refer to the same event, but are counted separately.
#
# Input:  A data frame to combine elements in, a new event name to
#         consolidate in to, and a list of events to consolidate.
# Output: A data frame with all specified events at the end of the
#         argument list rewritten as the second argument.

    # Get event list for rewriting
    oldEvents <- c(...)

    # Rewrite each event in turn
    for (i in 1:length(oldEvents)) {
        count <- nrow(df[df$event == oldEvents[i],])
        if (count > 0) {
            df[df$event == oldEvents[i],]$event <- newEvent
        } else {
              message(paste("Warning: No events of type",
                            oldEvents[i],"to merge into",
                            newEvent))
          }
    }
    df
}

billions <- function (number) {
# This function returns the number of billions in a specified number.
#
# Input:   A number.
# Output:  The number of billions represented by the input number.
    return(round(number/1000000000,1))
}

thousands <- function (number) {
# This function returns the number of thousands in a specified number.
#
# Input:   A number.
# Output:  The number of thousands represented by the input number.

    return(round(number/1000,1))
}

initialCap <- function(s) {
# This function converts text from any case into initial capital
# letters.
#
# Input:  A string
# Output: The same string, but with all words having only the
#         first letter capitalized.

    words <- strsplit(s, " ")[[1]]
    paste(toupper(substring(words, 1,1)), tolower(substring(words, 2)),
      sep="", collapse=" ")
}
