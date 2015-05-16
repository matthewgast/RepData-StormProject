# Get the data

readData <- function(directory) {
# Read raw data
    setwd("/Users/mgast/Dropbox/data-science-specialization/5-reproducible-research/RepData-StormProject")
    raw.df <- read.csv("repdata-data-StormData.csv")
    raw.df 
}

loadPackages <- function() {
# Load packages
    library(ggplot2)
    library(dplyr)
    library(foreach)
}

countNAs <- function (df) {
# Count NAs in a package to see how many of them there are.
    count.na <- as.data.frame(colSums(is.na(df)))
    names(count.na) <- c("count")

    #only print names of columns with NAs in them
    count.na <- subset(count.na, count>0)
    count.na
}

exponentTranslator <- function (exp) {
# Translate an exponent to a power of 10.
    
    exp <- as.character(exp)
    if (exp %in% c("0","1","2","3","4","5","6","7","8","9")) {
        return(as.numeric(exp))
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
    if (exp %in% c("h","H")) {
        return(2)
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




# three of the top events are the same! (TSTM WIND, THUNDERSTORM WIND, THUNDERSTORM WINDS)

# Print out the top num events in the data frame
topEvents <- function (df, num) {
    # prints top events
    if (missing(num)) {
        num <- 10
    }
    evtype.table <- table(df$EVTYPE)
    head(sort(evtype.table,decreasing=TRUE),n=num)
}

# Put two together
eventMerge <- function (df, newEventName, oldEventName) {
    # In df, rewrites all "matchEvents" to "newEvents"
    # usage: eventMerge(storm.data, "TSTM WIND", "THUNDERSTORM"
    message(paste("merging",oldEventName,"into",newEventName))
    df[df$EVTYPE == oldEventName,]$EVTYPE <- newEventName
    df
}

mergeEvents <- function (df, newEvent, ...  ) {
    oldEvents <- c(...)
    for (i in 1:length(oldEvents)) {
        count <- nrow(df[df$EVTYPE == oldEvents[i],]$EVTYPE)
        message(count)
        if (count > 0) {
            message(paste("Merging",count,oldEvents[i],"into",newEvent))
            df[df$EVTYPE == oldEvents[i],]$EVTYPE <- newEvent
        } else {
              message(paste("No events of type",oldEvents[i],"to merge into",newEvent))
          }
    }
    df
}
                 


storm.propdmg <- aggregate(propertyDamage ~ EVTYPE,data=storm.data,FUN=sum)
storm.cropdmg <- aggregate(cropDamage ~ EVTYPE,data=storm.data,FUN=sum)
storm.die <- aggregate(FATALITIES ~ EVTYPE,data=storm.data, FUN=sum)
storm.wound <- aggregate(INJURIES ~ EVTYPE,data=storm.data, FUN=sum)

storm.prop.sort <- storm.propdmg[ order(storm.propdmg$propertyDamage,decreasing=TRUE,na.last=TRUE), ]
storm.crop.sort <- storm.cropdmg[ order(storm.cropdmg$cropDamage,decreasing=TRUE,na.last=TRUE), ]
storm.die.sort <- storm.die[ order(storm.die$FATALITIES,decreasing=TRUE,na.last=TRUE), ]
storm.wound.sort <- storm.wound[ order(storm.wound$INJURIES,decreasing=TRUE,na.last=TRUE), ]

head(storm.prop.sort)
head(storm.crop.sort)
head(storm.die.sort)
head(storm.wound.sort)

storm.data <- mutate(storm.data,totalDamage=propertyDamage+cropDamage)
storm.destruct <- aggregate(. ~ EVTYPE, data=storm.data, FUN=sum)
storm.destruct.sort <- storm.destruct [ order(storm.destruct$totalDamage, decreasing=TRUE, na.last=TRUE),]
head(storm.destruct.sort)

> head(storm.destruct.sort,n=20)
                EVTYPE FATALITIES INJURIES propertyDamage  cropDamage    totalDamage
138        FLASH FLOOD        978     1777 68202366963580  1421317100 68203788280680
711 THUNDERSTORM WINDS         64      908 20865316766860   190734780 20865507501640
758            TORNADO       5633    91346  1078950511110   415113110  1079365624220
212               HAIL         15     1361   315755837790  3025974480   318781812270
418          LIGHTNING        816     5230   172943309310    12092090   172955401400
154              FLOOD        470     6789   144657709870  5661968450   150319678320
372  HURRICANE/TYPHOON         64     1275    69305840000  2607872800    71913712800
168           FLOODING          6        2    59208255000     8855500    59217110500
599        STORM SURGE         13       38    43323536000        5000    43323541000
274         HEAVY SNOW        127     1021    17932589140   134653100    18067242240
84             DROUGHT          0        4     1046106000 13972566000    15018672000
363          HURRICANE         61       46    11868319010  2741910000    14610229010
529        RIVER FLOOD          2        2     5118945500  5029459000    10148404500
387          ICE STORM         89     1975     3949927810  5022113500     8972041310
772     TROPICAL STORM         58      340     7703890550   678346000     8382236550
888       WINTER STORM        206     1321     6688597250    26944000     6715541250
320          HIGH WIND        248     1137     5270046610   638571300     5908617910
875           WILDFIRE         75      911     4765114000   295472800     5060586800
779          TSTM WIND        504     6957     4490458440   554007350     5044465790
600   STORM SURGE/TIDE         11        5     4641188000      850000     4642038000
