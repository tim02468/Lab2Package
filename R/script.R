#' Title
#' This function produce a boxplot from given dataframe and given type of payment.
#'
#' @param df A data include hospital-specific charges for the more than 3,000 U.S. hospitals that receive Medicare Inpatient Prospective Payment System (IPPS) payments. See https://data.cms.gov/Medicare-Inpatient/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3 for more information.
#' @param payment Type of payment to plot on a boxplot. Select either "Average.Medicare.Payments", "Average.Covered.Charges" or "Average.Total.Payments".
#'
#' @return g A boxplot.
#' @export
#'
#' @examples
make_boxplot <- function(df, payment) {
  g <- ggplot(df, aes(x = DRG.Definition, y = get(payment))) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(g)
}


#' Title
#' This function return a basic statistic from the given dataframe.
#'
#' @param df A data include hospital-specific charges for the more than 3,000 U.S. hospitals that receive Medicare Inpatient Prospective Payment System (IPPS) payments. See https://data.cms.gov/Medicare-Inpatient/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3 for more information.
#' @param type_of_stat Type of stat. Select from either "mean" for mean, "std" for standard deviation or "median" for median.
#'
#' @return res A value from the given type of statistic.
#' @export
#'
#' @examples
summary_stat <- function(df, type_of_stat) {
  res <- list(mean = mean(df$Average.Medicare.Payments), std = sd(df$Average.Medicare.Payments), median = median(df$Average.Medicare.Payments))
  return(res[names(res) == type_of_stat])
}

