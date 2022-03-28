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
require(tidyverse)
require(data.table)

# Read the trace file into memory
trace <- file(paste0(data_path, "sart-trace.txt"), "r")
lines <- readLines(trace)
close(trace)

# Variables
participant <- 0L
time <- 0
n <- length(lines)
curr_block <- 0
time_per_block <- as.numeric(str_match(lines[length(lines) - 1],
                                       "  ([\\d.]+)   ")[2]) / 4

# Pre-allocate a data.table where we can store UTILITY values:
# without this, the code runs very slowly due to the size of the trace file
utilities_attend <- data.table(participant = rep(0L, n),
                               time = rep(0, n),
                               utility = rep(0, n))

memories_matched <- data.table(participant = rep(0L, n),
                               block = rep(0L, n),
                               is_tut = rep(0, n))

idx_util <- 0L
idx_mem <- 0L

for (i in seq_len(n)) {
  # Read a single line
  line <- lines[i]
  # Detect the start of a new model run
  if (str_detect(line, "Run \\d+")) {
    participant <- participant + 1L
    print(participant)
  }

  # Update time
  new_time <- as.numeric(str_match(line, "  ([\\d.]+)   ")[2])
  if (!is.na(new_time)) {
    time <- new_time
    curr_block <- ceiling(time / time_per_block)
  }


  matched_memory <- str_match(line, "Chunk ([\\w-]+) with")
  regex_groups <- str_match(line, "UTILITY: ([E\\d\\.-]+)")

  # If it's a line regarding retrieved memory
  if (!is.na(matched_memory[1])) {
    retrieved_chunk <- matched_memory[2]
    if (retrieved_chunk == "WITHHOLD-ON-Q" ||
        retrieved_chunk == "PRESS-ON-O" ||
        retrieved_chunk == "REMEMBER-TO-ATTEND") {
      is_tut <- FALSE
    }
    else
      is_tut <- TRUE

    idx_mem <- idx_mem + 1L
    set(memories_matched,
        idx_mem,
        j = 1:3,
        value = list(participant, curr_block, is_tut))
  }
  else
  # If it's a line regarding utility
  if (!is.na(regex_groups[1])) {
    utility <- as.numeric(regex_groups[2])
    # time <- as.numeric(regex_groups[3])

    # add entry
    idx_util <- idx_util + 1L
    set(utilities_attend,
        idx_util,
        j = 1:3,
        value = list(participant, time, utility))
  }
}

# remove unused rows

memories_matched <- subset(memories_matched, participant != 0)
utilities_attend <- subset(utilities_attend, participant != 0)

# plot utility

utilities_part1 <- subset(utilities_attend, participant == 1)
utility_time <- setNames(aggregate(utilities_part1$utility,
                                   list(utilities_part1$time),
                                   mean), c("Time", "Utility"))
utility_time <- utility_time[-1, ]

ggplot(utility_time, aes(x = Time, y = Utility)) + geom_line() +
  # coord_cartesian(xlim = c(0, 500)) +
  geom_hline(yintercept = 0, linetype = "dotted", col = "red") +
  geom_ribbon(aes(ymin = -0.1,
                ymax = 0.1), colour = NA, fill = "red", alpha = 0.1) +
  theme_light()


# plot MW proportion per block

tuts_by_block <- aggregate(memories_matched$is_tut,
                           list(memories_matched$block),
                           sum) %>%
                 add_column(aggregate(memories_matched$is_tut,
                            list(memories_matched$block),
                            length)[2]) %>%
                 rename(block = 1,
                        tut_sum = 2,
                        n = 3)
                        

ggplot(tuts_by_block, aes(x = block, y = (tut_sum / n))) +
  geom_line() +
  geom_point() +
  ylab("MW Proportion") +
  xlab("Block Number") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_light()


#t.test function
t.test2 <- function(m1, m2, s1, s2, n1, n2, m0=0, equal.variance=FALSE) {
  if (equal.variance == FALSE) {
    se <- sqrt((s1^2 / n1) + (s2^2 / n2))
    # welch-satterthwaite df
    df <- ((s1^2 / n1 + s2^2 / n2)^2) /
          ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
  } else {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt((1 / n1 + 1 / n2) *
               ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) /
               n1 + n2 - 2)
    df <- n1 + n2 - 2
  }
  t <- (m1 - m2 - m0) / se 
  dat <- c(m1 - m2, se, t, 2 * pt(-abs(t), df))
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat)
}

#t test on REACTION TIME by mean and sd

behdat_non_nil <- subset(behdat, rt != "NIL")
rt_by_part <- setNames(aggregate(as.numeric(behdat_non_nil$rt),
                                 list(behdat_non_nil$participant),
                                 mean),
                       c("Participant", "meanRT"))

m1 <- mean(rt_by_part$meanRT)
m2 <- 0.3398

s1 <- sd(as.numeric(rt_by_part$meanRT))
s2 <- 0.0737

n1 <- participant
n2 <- 116
t.test2(m1, m2, s1, s2, n1, n2)

# plot RT CV
# TODO: Add error bars

mean_rt <- mean(as.numeric(behdat_non_nil$rt))

data_rt <- data.frame(type = c("data", "model"), rt = c(0.3398, mean(rt_by_part$meanRT)))

ggplot(data_rt, aes(y = rt, x = type, fill = type, ylab = "RT")) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("RT") +
  theme(legend.position = "none")


#t test on SART Accuracy by mean and sd

acc_by_part <- behdat %>%
                  subset((response == "f" & stimulus == "O") |
                         (response == "NIL" & stimulus == "Q"))

acc_by_part <- setNames(aggregate(acc_by_part$response,
                            list(acc_by_part$participant),
                            length), c("Participant", "accuracy"))
                            
acc_by_part$accuracy <- acc_by_part$accuracy / (nrow(behdat) / 4)

m1 <- mean(acc_by_part$accuracy)
m2 <- 0.9426

s1 <- sd(as.numeric(acc_by_part$accuracy))
s2 <- 0.06

n1 <- participant
n2 <- 116

t.test2(m1, m2, s1, s2, n1, n2)


# plot SART accuracy
# TODO: Add error bars

data_errors <- data.frame(type = c("data", "model"),
                          SE = c(0.9426, mean(acc_by_part$accuracy)))

ggplot(data_errors, aes(y = SE, x = type, fill = type)) +
  geom_bar(position = "dodge", stat = "identity") + ylab("SART Accuracy") +
  theme(legend.position = "none")