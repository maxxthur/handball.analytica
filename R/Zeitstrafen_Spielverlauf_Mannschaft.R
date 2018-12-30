#' Title
#'
#' @param Mannschaft
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
Zeitstrafen_Spielminute_Mannschaft <- function(Mannschaft, Data) {
  Mannschaft_Zeitstrafen <- Data[[4]] %>% dplyr::filter(Verein==Mannschaft)
  Spielminuten_Zeitstrafen <- as.numeric(substr(Mannschaft_Zeitstrafen$Spielminute, 1, 2))
  hist(Spielminuten_Zeitstrafen, breaks=6, main=paste("Zeitstrafen ", Mannschaft), col="red", xlab="Spielminute")
}
