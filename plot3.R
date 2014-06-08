## Create a plot3() function
plot3 <- function() {
    ## Check if "household_power_consumption.txt" exists in the working directory
    if (!file.exists("household_power_consumption.txt")) {
        ## If it does not, then check if "exdata_data_household_power_consumption.zip" exists in working directory
        if (!file.exists("exdata_data_household_power_consumption.zip")) {
            ## If it does not, then stop process, and display an error message
            stop("Error: Process expected to find household power text file or Zip file, 
                 but neither is avalable")
        } else {
            ## If "exdata_data_household_power_consumption.zip" does exist in working directory, then unzip it
            unzip("exdata_data_household_power_consumption.zip")
        }
    }

    ## Read "household_power_consumption.txt" file and assign data to "rt" data frame, with "character" type for all columns
    rt <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", colClasses = "character")

    ## Loop through columns 3 to 9 of "rt" data frame, and assign to "numeric" type
    for (Counta in 3:9) {
        rt[, Counta] <- as.numeric(rt[, Counta])
    }

    ## For those rows which had invalid numeric data, use complete.cases to remove them from "rt" data frame
    rt <- rt[complete.cases(rt), ]

    ## Paste the contents of the "Date" and "Time" columns of the "rt" data frame on a row by row basis to a vector "dateTimePaste"
    dateTimePaste <- paste(rt$Date, rt$Time)

    ## Format the "dateTimePaste" vector, and assign to the "Time" column of the "rt" data frame
    rt$Time <- strptime(dateTimePaste, "%d/%m/%Y %H:%M:%S")

    ## Format the "Date" column of the "rt" data frame as a date
    rt$Date <- as.Date(rt$Date, "%d/%m/%Y")

    ## Subset the "rt" data frame for those dates which are 1st or 2nd Feb 2007 and assign to "rtsub" data frame
    rtsub <- subset(rt, rt$Date == "2007-02-01" | rt$Date == "2007-02-02")

    ## Create a plot with name "plot3.png", with width and height of 480 pixels, and a white background
    ## This file will contain the graphic created next by the following "plot" command
    png(filename = "plot3.png", width = 480, height = 480, bg = "white")

    ## Plot "Sub_metering_1" against "Time" data as a line graph, with no main title, no label on the x-axis, and "Energy sub metering" as the label on the y-axis
    plot(rtsub$Time, rtsub$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
    ## Plot "Sub_metering_2" against "Time" data as a red line in the graph just created
    lines(rtsub$Time, rtsub$Sub_metering_2, col = "red")
    ## Plot "Sub_metering_3" against "Time" data as a blue line in the graph just created
    lines(rtsub$Time, rtsub$Sub_metering_3, col = "blue")
    ## Add a legend in the top right corner of the graph just created, black, red, and blue lines, the Sub_metering_1, 2, 3 data names, and with text magnification of 0.95
    legend("topright", col = c("black", "red", "blue"), lty = 1, legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex = 0.95)

    ## Switch the graphics device off
    dev.off()
}