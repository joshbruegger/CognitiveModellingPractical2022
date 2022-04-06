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
require(plotrix)

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
                               is_tut = rep(0L, n),
                               time = rep(0, n))

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
        j = 1:4,
        value = list(participant, curr_block, is_tut, time))
  }
  else
    # If it's a line regarding utility
    if (!is.na(regex_groups[1])) {
      utility <- as.numeric(regex_groups[2])
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
  # coord_cartesian(xlim = c(1620, 2160)) +
  geom_hline(yintercept = 0, linetype = "dotted", col = "red") +
  geom_ribbon(aes(ymin = -0.1,
                  ymax = 0.1), colour = NA, fill = "red", alpha = 0.1) +
  theme_light()

# Cumulative plot of tuts over time
memories_part1 <- subset(memories_matched, participant == 1)
ggplot(memories_part1, aes(x=time, y=cumsum(is_tut)/cumsum(is_tut != 2))) + geom_line()


# plot MW proportion per block

tuts_by_block <- aggregate(memories_matched$is_tut,
                           list(memories_matched$block,memories_matched$participant),
                           sum) %>%
  add_column(aggregate(memories_matched$is_tut,
                       list(memories_matched$block, memories_matched$participant),
                       length)[3]) %>%
  rename(block = 1,
         participant = 2,
         tut_sum = 3,
         tot = 4)%>%
  group_by(block) %>% 
  summarise(mean_proportion = mean(tut_sum/tot),se = std.error(tut_sum/tot))

  tuts_by_block$behav_dat = c(0.3518, 0.5444, 0.6303, 0.6865)
  tuts_by_block$behav_se = c(0.01318, 0.01662, 0.01662, 0.01662)



ggplot(tuts_by_block, aes(x = block, y = mean_proportion)) +
  geom_line(size = 0.8) +
  geom_point() +
  ylab("Proportion of Task Unrelated Thoughts (TUTs)") +
  xlab("Block Number") +
  coord_cartesian(ylim = c(0, 0.8)) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1), expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=14, face="bold")) +
  geom_errorbar(aes(ymin = mean_proportion-se, ymax = mean_proportion+se), width = 0.03) +
  geom_line(aes(y=behav_dat), col = "#e8505b", size=0.8) +
  geom_point(aes(y=behav_dat), col = "#e8505b") +
  geom_errorbar(aes(ymin = behav_dat-behav_se, ymax = behav_dat+behav_se), width = 0.03)

RMSD <- sqrt(mean((tuts_by_block$behav_dat - tuts_by_block$mean_proportion)^2))
RMSD

#t.test function
t.test2 <- function(m1, m2, s1, s2, n1, n2, m0=0, equal.variance = FALSE) {
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

rt_m1 <- mean(rt_by_part$meanRT)
rt_m2 <- 0.3398

rt_s1 <- sd(as.numeric(rt_by_part$meanRT))
rt_s2 <- 0.0737

rt_n1 <- participant
rt_n2 <- 116
t.test2(rt_m1, rt_m2, rt_s1, rt_s2, rt_n1, rt_n2)

# plot RT CV

mean_rt <- mean(as.numeric(behdat_non_nil$rt))

data_rt <- data.frame(type = c("data", "Model", "VanVugt"),
                      RT = c(0.3398, mean(rt_by_part$meanRT), 0.26242),
                      SD = c(rt_s2, rt_s1, 0.08715))

ggplot(data_rt, aes(y = RT, x = type, fill = type, ylab = "RT")) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("RT") +
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin = RT-SD, ymax = RT+SD), width = 0.2)


#t test on SART Accuracy by mean and sd

acc_by_part <- behdat %>%
  subset((response == "f" & stimulus == "O") |
           (response == "NIL" & stimulus == "Q"))

acc_by_part <- setNames(aggregate(acc_by_part$response,
                                  list(acc_by_part$participant),
                                  length), c("Participant", "accuracy"))

acc_by_part$accuracy <- acc_by_part$accuracy / (nrow(behdat) / 4)

sa_m1 <- mean(acc_by_part$accuracy)
sa_m2 <- 0.9426

sa_s1 <- sd(as.numeric(acc_by_part$accuracy))
sa_s2 <- 0.06

sa_n1 <- participant
sa_n2 <- 116

t.test2(sa_m1, sa_m2, sa_s1, sa_s2, sa_n1, sa_n2)


# plot SART accuracy

data_errors <- data.frame(type = c("data", "Model", "VanVugt"),
                          SE = c(0.9426, mean(acc_by_part$accuracy), 0.90525),
                          SD = c(sa_s2, sa_s1, 0.010797))

ggplot(data_errors, aes(y = SE, x = type, fill = type)) +
  geom_bar(position = "dodge", stat = "identity") + ylab("SART Accuracy") +
  theme(legend.position = "none") + 
  geom_errorbar(aes(ymin = SE - SD, ymax = SE+SD), width = 0.2)

# RTCV

ggplot(data_rt, aes(y = RT/SD, x = type, fill = type)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("RTCV") +
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin = RT/SD-SD, ymax = RT/SD+SD), width = 0.2)