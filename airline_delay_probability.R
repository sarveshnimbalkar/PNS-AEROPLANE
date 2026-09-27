# Airline Flight Delay Analysis Using Conditional Probability

options(stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(scales)

file_name <- "flight_data_2024.csv"

if (!file.exists(file_name)) {
  stop("ERROR: flight_data_2024.csv was not found in the working directory.\nPlease place the dataset in the same folder as this R script.")
}

cat("========================================\n")
cat("AIRLINE FLIGHT DELAY PROBABILITY ANALYSIS\n")
cat("========================================\n\n")

header_data <- read.csv(file_name, check.names = FALSE, nrows = 0)
candidate_columns <- c(
  "op_unique_carrier", "op_carrier", "airline", "carrier",
  "fl_date", "flight_date", "date", "day_of_week", "day", "weekday",
  "dep_delay", "departure_delay", "tail_num", "tail_number",
  "aircraft_registration", "registration", "crs_dep_time",
  "scheduled_departure_time", "cancelled", "canceled", "diverted", "diversion"
)
column_classes <- ifelse(names(header_data) %in% candidate_columns, NA_character_, "NULL")
raw_data <- read.csv(file_name, check.names = FALSE, colClasses = column_classes, na.strings = c("", "NA", "NULL"))
initial_rows <- nrow(raw_data)
cat("Initial rows:", initial_rows, "\n")
cat("Columns found:\n", paste(names(raw_data), collapse = ", "), "\n\n")

# Resolve common BTS naming variations without silently accepting missing core fields.
find_column <- function(data, candidates) {
  matched <- candidates[candidates %in% names(data)]
  if (length(matched) == 0) return(NA_character_)
  matched[[1]]
}

column_map <- c(
  airline = find_column(raw_data, c("op_unique_carrier", "op_carrier", "airline", "carrier")),
  flight_date = find_column(raw_data, c("fl_date", "flight_date", "date")),
  day_of_week = find_column(raw_data, c("day_of_week", "day", "weekday")),
  departure_delay = find_column(raw_data, c("dep_delay", "departure_delay")),
  tail_num = find_column(raw_data, c("tail_num", "tail_number", "aircraft_registration", "registration")),
  scheduled_departure = find_column(raw_data, c("crs_dep_time", "scheduled_departure_time")),
  cancelled = find_column(raw_data, c("cancelled", "canceled")),
  diverted = find_column(raw_data, c("diverted", "diversion"))
)

core_fields <- c("airline", "flight_date", "day_of_week", "departure_delay", "cancelled", "diverted")
missing_core <- names(column_map)[names(column_map) %in% core_fields & is.na(column_map)]
if (length(missing_core) > 0) {
  stop(paste("ERROR: required columns are missing:", paste(missing_core, collapse = ", ")))
}

missing_report <- data.frame(
  Field = names(column_map),
  Column = unname(column_map),
  Missing = sapply(column_map, function(x) if (is.na(x)) NA_integer_ else sum(is.na(raw_data[[x]])))
)
print(missing_report, row.names = FALSE)
duplicate_rows <- sum(duplicated(raw_data))

data <- data.frame(
  Airline = trimws(as.character(raw_data[[column_map[["airline"]]]] )),
  FlightDate = as.Date(as.character(raw_data[[column_map[["flight_date"]]]] ), format = "%Y-%m-%d"),
  DayValue = raw_data[[column_map[["day_of_week"]]]],
  DepDelay = suppressWarnings(as.numeric(raw_data[[column_map[["departure_delay"]]]] )),
  TailNum = if (is.na(column_map[["tail_num"]])) rep(NA_character_, nrow(raw_data)) else trimws(as.character(raw_data[[column_map[["tail_num"]]]] )),
  ScheduledDeparture = if (is.na(column_map[["scheduled_departure"]])) rep(NA_real_, nrow(raw_data)) else suppressWarnings(as.numeric(raw_data[[column_map[["scheduled_departure"]]]] )),
  Cancelled = as.numeric(raw_data[[column_map[["cancelled"]]]] ),
  Diverted = as.numeric(raw_data[[column_map[["diverted"]]]] )
)

cancelled_removed <- sum(data$Cancelled == 1, na.rm = TRUE)
diverted_removed <- sum(data$Diverted == 1, na.rm = TRUE)
data <- data %>% filter(Cancelled != 1, Diverted != 1)

day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
day_numeric <- suppressWarnings(as.numeric(as.character(data$DayValue)))
day_text <- trimws(as.character(data$DayValue))
day_index <- ifelse(!is.na(day_numeric) & day_numeric >= 1 & day_numeric <= 7, day_numeric, match(tolower(day_text), tolower(day_names)))
data$Day <- factor(day_names[day_index], levels = day_names)
data <- data %>%
  filter(!is.na(Airline), Airline != "", !is.na(DepDelay), !is.na(Day)) %>%
  mutate(Delay = DepDelay >= 15)

final_rows <- nrow(data)
cat("\nData quality report\n")
cat("Initial rows:", initial_rows, "\nFinal rows:", final_rows, "\nRows removed:", initial_rows - final_rows, "\n")
cat("Duplicate rows:", duplicate_rows, "\nCancelled flights removed:", cancelled_removed, "\nDiverted flights removed:", diverted_removed, "\n\n")

safe_probability <- function(numerator, denominator) {
  ifelse(denominator > 0, numerator / denominator, NA_real_)
}

total_flights <- nrow(data)
delayed_flights <- sum(data$Delay)
on_time_flights <- total_flights - delayed_flights
p_delay <- safe_probability(delayed_flights, total_flights)
p_no_delay <- safe_probability(on_time_flights, total_flights)
cat("Overall probability\nTotal Flights:", total_flights, "\nDelayed Flights:", delayed_flights,
    "\nOn-Time Flights:", on_time_flights, "\nP(Delay):", percent(p_delay, accuracy = 0.01),
    "\nP(No Delay):", percent(p_no_delay, accuracy = 0.01),
    "\nProbability sum:", p_delay + p_no_delay, "\n\n")

airline_results <- data %>%
  group_by(Airline) %>%
  summarise(Total.Flights = n(), Delayed.Flights = sum(Delay), On.Time.Flights = sum(!Delay), .groups = "drop") %>%
  mutate(P.Delay.Given.Airline = safe_probability(Delayed.Flights, Total.Flights)) %>%
  arrange(desc(P.Delay.Given.Airline), Airline)
day_results <- data %>%
  group_by(Day, .drop = FALSE) %>%
  summarise(Total.Flights = n(), Delayed.Flights = sum(Delay), On.Time.Flights = sum(!Delay), .groups = "drop") %>%
  filter(Total.Flights > 0) %>%
  mutate(P.Delay.Given.Day = safe_probability(Delayed.Flights, Total.Flights))
airline_day_results <- data %>%
  group_by(Airline, Day) %>%
  summarise(Total.Flights = n(), Delayed.Flights = sum(Delay), .groups = "drop") %>%
  mutate(P.Delay.Given.Airline.Day = safe_probability(Delayed.Flights, Total.Flights))

write.csv(airline_results, "airline_probability_results.csv", row.names = FALSE)
write.csv(day_results, "day_probability_results.csv", row.names = FALSE)
write.csv(airline_day_results, "airline_day_probability_results.csv", row.names = FALSE)
cat("Airline conditional probabilities:\n"); print(airline_results, row.names = FALSE)
cat("Day conditional probabilities:\n"); print(day_results, row.names = FALSE)

airline_bayes <- airline_results
airline_bayes$P.Airline <- if (total_flights > 0) airline_bayes$Total.Flights / total_flights else NA_real_
airline_bayes$P.Delay <- p_delay
airline_bayes$Bayes.Probability <- if (p_delay > 0) airline_bayes$P.Delay.Given.Airline * airline_bayes$P.Airline / p_delay else NA_real_
airline_bayes$Direct.Count.Probability <- if (delayed_flights > 0) airline_bayes$Delayed.Flights / delayed_flights else NA_real_
airline_bayes$Difference <- abs(airline_bayes$Bayes.Probability - airline_bayes$Direct.Count.Probability)
airline_bayes <- airline_bayes %>%
  select(Airline, P.Airline, P.Delay.Given.Airline, P.Delay, Bayes.Probability, Direct.Count.Probability, Difference)
write.csv(airline_bayes, "bayes_verification_results.csv", row.names = FALSE)
max_difference <- max(airline_bayes$Difference, na.rm = TRUE)
bayes_passed <- is.finite(max_difference) && max_difference <= 1e-10
cat("\nBayes verification (maximum difference):", max_difference, "\n")
if (bayes_passed) cat("Bayes Theorem Verification: PASSED\n") else cat("Bayes Theorem Verification: FAILED\n")

conditional_verification <- data %>%
  group_by(Airline) %>%
  summarise(Total = n(), Delayed = sum(Delay), .groups = "drop") %>%
  as.data.frame()
conditional_verification$Method.1 <- ifelse(conditional_verification$Total > 0, conditional_verification$Delayed / conditional_verification$Total, NA_real_)
conditional_verification$P.Joint <- if (total_flights > 0) conditional_verification$Delayed / total_flights else NA_real_
conditional_verification$P.Airline <- if (total_flights > 0) conditional_verification$Total / total_flights else NA_real_
conditional_verification$Method.2 <- ifelse(conditional_verification$P.Airline > 0, conditional_verification$P.Joint / conditional_verification$P.Airline, NA_real_)
conditional_verification$Absolute.Difference <- abs(conditional_verification$Method.1 - conditional_verification$Method.2)
cat("\nConditional probability verification:\n"); print(conditional_verification, row.names = FALSE)

# Sequential analysis requires a valid tail identifier and scheduled departure time.
sequential_available <- !is.na(column_map[["tail_num"]]) && !is.na(column_map[["scheduled_departure"]])
sequential_results <- data.frame()
if (sequential_available) {
  sequence_data <- data %>%
    filter(!is.na(TailNum), TailNum != "", !is.na(FlightDate), !is.na(ScheduledDeparture)) %>%
    mutate(ScheduledMinutes = floor(ScheduledDeparture / 100) * 60 + ScheduledDeparture %% 100,
           DuplicateScheduledTime = duplicated(cbind(TailNum, FlightDate, ScheduledMinutes)) | duplicated(cbind(TailNum, FlightDate, ScheduledMinutes), fromLast = TRUE)) %>%
    arrange(TailNum, FlightDate, ScheduledMinutes) %>%
    group_by(TailNum, FlightDate) %>%
    mutate(PreviousDelay = lag(Delay)) %>%
    ungroup() %>%
    filter(!is.na(PreviousDelay))
  sequential_results <- data.frame(
    Condition = c("Previous flight delayed", "Previous flight not delayed", "Overall current-flight sample"),
    Observations = c(sum(sequence_data$PreviousDelay), sum(!sequence_data$PreviousDelay), nrow(sequence_data)),
    Joint.Delayed.Current = c(sum(sequence_data$PreviousDelay & sequence_data$Delay), sum(!sequence_data$PreviousDelay & sequence_data$Delay), sum(sequence_data$Delay)),
    Probability = c(safe_probability(sum(sequence_data$PreviousDelay & sequence_data$Delay), sum(sequence_data$PreviousDelay)), safe_probability(sum(!sequence_data$PreviousDelay & sequence_data$Delay), sum(!sequence_data$PreviousDelay)), safe_probability(sum(sequence_data$Delay), nrow(sequence_data)))
  )
  write.csv(sequential_results, "sequential_probability_results.csv", row.names = FALSE)
  cat("Sequential observations used:", nrow(sequence_data), "\n")
  print(sequential_results, row.names = FALSE)
} else {
  cat("Sequential observations available: 0 (tail_num and/or scheduled departure column is absent)\n")
}

overall_plot_data <- data.frame(Status = factor(c("Delayed", "Not Delayed"), levels = c("Delayed", "Not Delayed")), Count = c(delayed_flights, on_time_flights)) %>% mutate(Proportion = Count / sum(Count))
ggsave("01_overall_delay_distribution.png", ggplot(overall_plot_data, aes(Status, Count, fill = Status)) + geom_col() + geom_text(aes(label = percent(Proportion, accuracy = 0.1)), vjust = -0.3) + labs(title = "Overall Flight Delay Distribution", x = "Flight status", y = "Number of flights") + theme_minimal() + theme(legend.position = "none"), width = 8, height = 5, dpi = 150)
ggsave("02_airline_conditional_probability.png", ggplot(airline_results, aes(reorder(Airline, P.Delay.Given.Airline), P.Delay.Given.Airline)) + geom_col(fill = "steelblue") + coord_flip() + scale_y_continuous(labels = percent) + labs(title = "Conditional Probability of Delay by Airline", x = "Airline", y = "Conditional Probability of Delay") + theme_minimal(), width = 8, height = 6, dpi = 150)
ggsave("03_day_conditional_probability.png", ggplot(day_results, aes(Day, P.Delay.Given.Day)) + geom_col(fill = "darkorange") + scale_y_continuous(labels = percent) + labs(title = "Conditional Probability of Delay by Day", x = "Day of week", y = "Conditional Probability of Delay") + theme_minimal(), width = 8, height = 5, dpi = 150)
ggsave("04_airline_day_heatmap.png", ggplot(airline_day_results, aes(Day, Airline, fill = P.Delay.Given.Airline.Day)) + geom_tile() + geom_text(aes(label = percent(P.Delay.Given.Airline.Day, accuracy = 0.1)), size = 2.5) + scale_fill_continuous(labels = percent, na.value = "grey90") + labs(title = "Delay Probability by Airline and Day", x = "Day of week", y = "Airline", fill = "Probability") + theme_minimal(), width = 10, height = 7, dpi = 150)
if (nrow(sequential_results) >= 2) {
  sequential_plot_data <- bind_rows(data.frame(Condition = "Overall P(Delay)", Probability = p_delay), sequential_results[1:2, c("Condition", "Probability")])
  ggsave("05_sequential_delay_probability.png", ggplot(sequential_plot_data, aes(Condition, Probability, fill = Condition)) + geom_col() + scale_y_continuous(labels = percent) + labs(title = "Unconditional and Sequential Delay Probabilities", x = NULL, y = "Probability") + theme_minimal() + theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1)), width = 8, height = 5, dpi = 150)
}

report_extremes <- function(values, labels, highest = TRUE) {
  valid <- is.finite(values)
  if (!any(valid)) return("Unavailable")
  target <- if (highest) max(values[valid]) else min(values[valid])
  paste(labels[valid & values == target], collapse = ", ")
}
cat("\n========================================\nAIRLINE FLIGHT DELAY PROBABILITY ANALYSIS\n========================================\n")
cat("Dataset: ", file_name, "\nTotal Flights: ", total_flights, "\nDelayed Flights: ", delayed_flights, "\nOn-Time Flights: ", on_time_flights, "\nOverall P(Delay): ", p_delay, "\n", sep = "")
cat("Highest observed airline conditional delay probability: ", report_extremes(airline_results$P.Delay.Given.Airline, airline_results$Airline), "\n", sep = "")
cat("Lowest observed airline conditional delay probability: ", report_extremes(airline_results$P.Delay.Given.Airline, airline_results$Airline, FALSE), "\n", sep = "")
cat("Highest observed day conditional delay probability: ", report_extremes(day_results$P.Delay.Given.Day, as.character(day_results$Day)), "\n", sep = "")
cat("Lowest observed day conditional delay probability: ", report_extremes(day_results$P.Delay.Given.Day, as.character(day_results$Day), FALSE), "\n", sep = "")
cat("Highest observed airline-day conditional delay probability: ", report_extremes(airline_day_results$P.Delay.Given.Airline.Day, paste(airline_day_results$Airline, airline_day_results$Day, sep = " / ")), "\n", sep = "")
cat("Lowest observed airline-day conditional delay probability: ", report_extremes(airline_day_results$P.Delay.Given.Airline.Day, paste(airline_day_results$Airline, airline_day_results$Day, sep = " / "), FALSE), "\n")
cat("Bayes verification: ", ifelse(bayes_passed, "PASSED", "FAILED"), "\n", sep = "")
cat("Sequential probability: ", ifelse(nrow(sequential_results) > 0, sequential_results$Probability[1], "Unavailable"), "\n\n", sep = "")
cat("Interpretation\nThe calculated P(Delay) is the observed proportion of cleaned flights delayed by at least 15 minutes.\nP(Delay | Airline) is the observed delay probability within each airline subset. Bayes' Theorem reverses the conditioning direction from P(Delay | Airline) to P(Airline | Delay).\nThe sequential conditional probability measures the observed current delay probability given a delayed preceding aircraft flight; this is an association in observed data and does not establish causation.\n\n")
cat("Limitations\nThis is observational data. Conditional probability identifies observed relationships within subsets and does not establish causation. Weather, air traffic control, airport congestion, maintenance and other factors may affect delays. Sequential aircraft analysis depends on accurate aircraft registration and flight ordering; missing or inconsistent identifiers can reduce its usable sample. Results describe this dataset and should not automatically be generalized to every airline or future period.\n")