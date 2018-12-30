#' Title
#'
#' @param Spieler
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
Tore_Spielminute_Spieler <- function(Name, Data) {
 Spieler_Tore <- Data[[3]] %>% dplyr::filter(Spieler==Name)
 Spielminuten_Tore <- as.numeric(substr(Spieler_Tore$Spielminute, 1, 2))
 hist(Spielminuten_Tore, breaks=6, main=paste("Tore", Name), col="green", xlab="Spielminute")
}


