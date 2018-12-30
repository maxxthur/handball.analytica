#' Title
#'
#' @param Mannschaft
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
GelbeKarten_Spielminute_Mannschaft <- function(Mannschaft, Data) {
  Mannschaft_GK <- Data[[5]] %>% dplyr::filter(Verein==Mannschaft)
  Spielminuten_GK <- as.numeric(substr(Mannschaft_GK$Spielminute, 1, 2))
  hist(Spielminuten_GK, breaks=6, main=paste("Gelbe Karten ", Mannschaft), col="yellow", xlab="Spielminute")
}
