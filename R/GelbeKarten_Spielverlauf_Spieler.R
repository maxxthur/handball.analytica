#' Title
#'
#' @param Name
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
GelbeKarten_Spielverlauf_Spieler <- function(Name, Data) {
  Spieler_GK <- Data[[5]] %>% dplyr::filter(Spieler==Name)
  Spielminuten_GK <- as.numeric(substr(Spieler_GK$Spielminute, 1, 2))
  hist(Spielminuten_GK, breaks = 6, main=paste("Gelbe Karten ", Name), col="yellow", xlab="Spielminute")
}
