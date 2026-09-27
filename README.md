# Airline Flight Delay Analysis Using Conditional Probability

This project analyzes the 2024 BTS flight dataset using probability and statistical frequency calculations. It demonstrates:

- Overall delay probability
- Conditional probability by airline
- Conditional probability by day of week
- Joint airline-day conditional probability
- Bayes' theorem
- Data cleaning and quality reporting

No machine learning, regression, simulation, or predictive modeling is used.

## Requirements

- R 4.x
- R packages: `dplyr`, `ggplot2`, and `scales`
- `flight_data_2024.csv` in the project directory

The full dataset is excluded from GitHub because it is larger than GitHub's 100 MB file limit. The local script expects the file to be named exactly:

```text
flight_data_2024.csv
```

## Run the Analysis

From this directory, run:

```powershell
Rscript airline_delay_probability.R
```

On Windows, if R is not on the PATH:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" airline_delay_probability.R
```

A flight is classified as delayed when its departure delay is at least 15 minutes. Cancelled and diverted flights are removed before the probability calculations.

## Generated Outputs

The script creates these plots:

- `01_overall_delay_distribution.png`
- `02_airline_conditional_probability.png`
- `03_day_conditional_probability.png`
- `04_airline_day_heatmap.png`
- `05_sequential_delay_probability.png` when valid aircraft sequence data exists

It also creates CSV result tables:

- `airline_probability_results.csv`
- `day_probability_results.csv`
- `airline_day_probability_results.csv`
- `bayes_verification_results.csv`
- `sequential_probability_results.csv` when sequential analysis is possible

## Current Dataset Result

Using the supplied dataset after cleaning:

- Total flights: 6,965,267
- Delayed flights: 1,434,766
- On-time flights: 5,530,501
- Overall probability of delay: approximately 20.60%
- Bayes theorem verification: passed

The supplied data does not contain an aircraft registration column such as `tail_num`, so sequential aircraft delay propagation is reported as unavailable and plot 5 is not generated.

## Limitations

This is observational data. Conditional probability describes observed proportions within subsets and does not establish causation. Weather, air traffic control, congestion, maintenance, and other factors may affect delays. The results describe this dataset and should not automatically be generalized to every airline or future period.
