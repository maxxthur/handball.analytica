#' Title
#'
#' @param Name
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
Zeitstrafen_Spielverlauf_Spieler <- function(Name, Data) {
  Spieler_Zeitstrafen <- Data[[4]] %>% dplyr::filter(Spieler==Name)
  Spielminuten_Zeitstrafen <- as.numeric(substr(Spieler_Zeitstrafen$Spielminute, 1, 2))
  hist(Spielminuten_Zeitstrafen, breaks = 6, main=paste("Zeitstrafen ", Name), col="red", xlab="Spielminute")
}
