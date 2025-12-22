if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")

library(ggplot2)
library(dplyr)
library(readr)

#' joules used
#'
#' @returns joules consuption
#' @export
#'
#' @examples
read_energy_j <- function() {
uj <- suppressWarnings(read_lines("/sys/class/powercap/intel-rapl:0/energy_uj"))
as.numeric(uj) / 1e6  # Convert from microjoules to joules
}


# Logger function
log_cpu_power <- function(duration = 60, interval = 5, cost_kwh = 0.25) {
  timestamps <- numeric()
  energies <- numeric()

  message("Logging for ", duration, " seconds...")
  start_energy <- read_energy_j()
  start_time <- Sys.time()

  for (i in seq(0, duration, by = interval)) {
    now <- Sys.time()
    energy <- read_energy_j()
    timestamps <- c(timestamps, as.numeric(difftime(now, start_time, units = "secs")))
    energies <- c(energies, energy)
    Sys.sleep(interval)
  }

  # Compute deltas and power
  df <- data.frame(
    time_sec = timestamps,
    energy_j = energies
  ) %>%
    mutate(
      delta_j = c(0, diff(energy_j)),
      delta_t = c(0, diff(time_sec)),
      power_w = ifelse(delta_t > 0, delta_j / delta_t, NA)
    )

  total_energy_j <- tail(df$energy_j, 1) - df$energy_j[1]
  total_energy_wh <- total_energy_j / 3600
  cost_estimate <- total_energy_wh * cost_kwh

  cat("\n--- Summary ---\n")
  cat(sprintf("Duration: %.1f seconds\n", duration))
  cat(sprintf("Energy used: %.2f Joules (%.6f Wh)\n", total_energy_j, total_energy_wh))
  cat(sprintf("Estimated electricity cost: €%.4f (at %.2f €/kWh)\n", cost_estimate, cost_kwh))

  # Plot
  ggplot(df[-1,], aes(x = time_sec, y = power_w)) +
    geom_line(color = "steelblue", size = 1) +
    labs(
      title = "CPU Power Consumption Over Time",
      x = "Time (s)",
      y = "Power (W)"
    ) +
    theme_minimal()
}

log_cpu_power(duration = 60, interval = 3, cost_kwh = 0.25)
