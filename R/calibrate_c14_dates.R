#' Calibrate Radiocarbon Dates
#'
#' This function calibrates radiocarbon dates using the OxCal calibration curve via the \code{oxcAAR} package.
#'
#' @param names A character vector of sample names.
#' @param dates A numeric vector of uncalibrated radiocarbon dates (BP).
#' @param standard_deviations A numeric vector of standard deviations for the dates.
#' @param sigma The sigma range to use for calibration (\code{"two_sigma"} by default).
#' @param plot Logical; if \code{TRUE}, generates a calibration plot.
#' @param output_plot_file Optional; file path to save the plot. If \code{NULL}, the plot is displayed but not saved.
#' @param output_csv_file Optional; file path to save the calibrated data as a CSV file. If \code{NULL}, no file is saved.
#'
#' @details
#' The function calibrates radiocarbon dates and converts the results to BP (Before Present) format. It can also generate a calibration plot and save the results to a CSV file.
#'
#' @return A data frame containing the original dates and the calibrated date ranges in BP.
#'
#' @examples
#' \dontrun{
#' # Example data
#' names <- c("Sample1", "Sample2")
#' dates <- c(1000, 1500)
#' standard_deviations <- c(50, 100)
#'
#' # Calibrate dates
#' results <- calibrate_c14_dates(
#'   names = names,
#'   dates = dates,
#'   standard_deviations = standard_deviations,
#'   sigma = "two_sigma",
#'   plot = TRUE,
#'   output_plot_file = "calibrated_c14_dates_plot.png",
#'   output_csv_file = "calibrated_c14_data.csv"
#' )
#' }
#'
#' @importFrom oxcAAR oxcalCalibrate calcurve_plot get_tidy_oxcalresult isOxcalSetup
#' @importFrom grDevices png dev.off
#' @export
calibrate_c14_dates <- function(names, dates, standard_deviations, sigma = "two_sigma", plot = FALSE, output_plot_file = NULL, output_csv_file = NULL) {
  # Your function code here...
}

calibrate_c14_dates <- function(names, dates, standard_deviations, sigma = "two_sigma", plot = TRUE, output_plot_file = "Cal_c14_curve.png", output_csv_file = "Cal_c14_dates.csv") {

  # Check if OxCal is set up
  packages <- c("oxcAAR", "ggridges", "ggplot2")

  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }


  quickSetupOxcal(path = "~/OxCal")


  # Calibrate the dates
  calibrated_c14_dates <-
    oxcAAR::oxcalCalibrate(bp = dates, std = standard_deviations, names = names)

  # Plot and save the plot if requested
  if (plot) {
    if (!is.null(output_plot_file)) {
      png(output_plot_file, width = 1920, height = 1080, res = 150)
    }
    oxcAAR::calcurve_plot(calibrated_c14_dates, dates_sigma_ranges = sigma, uncal_range = FALSE, cal_range = TRUE)
    if (!is.null(output_plot_file)) {
      dev.off()
    }
  }

  # Transform the calibrated results to tidy format
  tidy_calibrated_c14_dates <- oxcAAR::get_tidy_oxcalresult(calibrated_c14_dates)

  # Initialize a data frame to store the results
  results_df <- data.frame(
    names = names,
    dates = dates,
    Standard_deviation = standard_deviations,
    Kal_bp_start = NA_real_,
    Kal_bp_end = NA_real_,
    probability = NA_real_
  )

  n_samples <- length(tidy_calibrated_c14_dates$name)

  for (i in 1:n_samples) {

    sigma_df <- tidy_calibrated_c14_dates$sigma_ranges[[i]][[sigma]]

    max_prob_row <- which.max(sigma_df$probability)

    start_value <- sigma_df$start[max_prob_row]
    end_value <- sigma_df$end[max_prob_row]
    prob_value <- sigma_df$probability[max_prob_row]

    if (start_value >= 0) {
      start_bp <- 1950 - start_value
    } else {
      start_bp <- 1950 + abs(start_value)
    }

    if (end_value >= 0) {
      end_bp <- 1950 - end_value
    } else {
      end_bp <- 1950 + abs(end_value)
    }

    results_df$Kal_bp_start[i] <- start_bp
    results_df$Kal_bp_end[i] <- end_bp
    results_df$probability[i] <- prob_value
  }

  if (!is.null(output_csv_file)) {
    write.csv(results_df, output_csv_file, row.names = FALSE)
  }

  # Return the results data frame
  return(results_df)
}
