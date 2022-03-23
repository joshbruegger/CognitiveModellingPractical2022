##
## SART model analysis
## Cognitive Science Practical 2022
##

# Read in the data
data_path <- "~/output/" # Change to the location of your output folder

# List of the data files
behfiles <- list.files(path = data_path, pattern = ".csv", full.names = TRUE)

# Combine all data files into a single data frame (behdat)
behdat <- data.frame()
for (i in seq_len(length(behfiles))) {
  behdat <- rbind(behdat,
                  read.csv(behfiles[i],
                  sep = ",",
                  strip.white = TRUE))
}

##---------------------------------------##
##      Analysing the model trace        ##
##---------------------------------------##

# Required packages
require(stringr)
require(data.table)
require(ggplot2)

# Read the trace file into memory
trace <- file(paste0(data_path, "sart-trace.txt"), "r")
lines <- readLines(trace)
close(trace)

# Variables
participant <- 0L
time <- 0
n <- length(lines)

# Pre-allocate a data.table where we can store UTILITY values:
# without this, the code runs very slowly due to the size of the trace file
utilities_attend <- data.table(participant = rep(0L, n),
                              time = rep(0, n),
                              utility = rep(0, n))
idx <- 0L

for (i in seq_len(n)) {
  # Read a single line
  line <- lines[i]
  # Detect the start of a new model run
  if (str_detect(line, "Run \\d+")) {
    participant <- participant + 1L
    print(participant)
  }

  regex_groups <- str_match(line, "UTILITY: ([E\\d\\.-]+) +([\\d\\.-]+)")

  utility <- as.numeric(regex_groups[2])
  time <- as.numeric(regex_groups[3])

  if (!is.na(utility)) {
    # add entry
    idx <- idx + 1L
    set(utilities_attend,
        idx,
        j = 1:3,
        value = list(participant, time, utility))
  }

}

# plot utility

utility_time <- setNames(aggregate(utilities_attend$utility,
                        list(utilities_attend$time),
                        mean), c("Time", "Utility"))
utility_time <- utility_time[-1, ]

ggplot(utility_time, aes(x = Time, y = Utility)) + geom_line() +
coord_cartesian(xlim = c(0, 500)) +
geom_hline(yintercept = 0, linetype = "dotted", col = "red") +
geom_ribbon(aes(ymin = -0.1,
      ymax = 0.1), colour = NA, fill = "red", alpha = 0.1)


# plot RT CV
# TODO: Add error bars

data_rt <- data.frame(type = c("data", "model"), rt = c(0.30348, 0.2924))
ggplot(data_rt, aes(y = rt, x = type, fill = type, ylab = "RTCV")) +
geom_bar(position = "dodge", stat = "identity") +
ylab("RTCV") + legend.position("none")


# plot SART errors
# TODO: Add error bars

data_errors <- data.frame(type = c("data", "model"),
                          rt = c(1 - 4.2111 / 240, 0.944))

ggplot(data_errors, aes(y = rt, x = type, fill = type, ylab = "RTCV")) +
geom_bar(position = "dodge", stat = "identity") + ylab("SART Accuracy") +
theme(legend.position = "none")