#' Title
#'
#' @param Mannschaft
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
Tore_Spielminute_Mannschaft <- function(Mannschaft, Data) {
  Mannschaft_Tore <- Data[[3]] %>% dplyr::filter(Verein==Mannschaft)
  Spielminuten_Tore <- as.numeric(substr(Mannschaft_Tore$Spielminute, 1, 2))
  hist(Spielminuten_Tore, breaks=6, main=paste("Tore", Mannschaft), col="green", xlab="Spielminute")
}
