#' Title
#'
#' @param Team
#' @param Data
#' @param Matches
#'
#' @return
#' @export
#'
#' @examples
Kader <- function(Team, Data, Matches) {
  TeamData <- TeamData <- Data[[1]] %>% dplyr::filter(Mannschaft == Team)
  Kader_Voll <- TeamData %>% dplyr::group_by(Name, Mannschaft) %>% dplyr::summarise(Tore = sum(Tore), Zeitstrafen = sum(`2min`), V = sum(V)) %>% dplyr::select("Name", "Tore", "Zeitstrafen", "V")
  Spieltage <- unique(TeamData$Datum)
  Kader_Rest <- list()
  for (i in ((length(Spieltage) - Matches):length(Spieltage))) {
    TeamData_Spieltag <- TeamData %>% dplyr::filter(Datum == Spieltage[i])
    Kader_Spieltag <- TeamData_Spieltag %>% dplyr::select("#", "Name", "Tore", "2min", "V")
    Kader_Rest[[i - (length(Spieltage) - Matches - 1)]] <- Kader_Spieltag
  }
  Meiste_Tore <- Kader_Voll %>% dplyr::arrange(dplyr::desc(Tore)) %>% .[1:3,1:2] %>% dplyr::mutate(Abh.=round(Tore/sum(Kader_Voll$Tore), 3))
  Meiste_Zeitstrafen <-  Kader_Voll %>% dplyr::arrange(dplyr::desc(Zeitstrafen)) %>% .[1:3,c(1,3)]
  Meiste_Gelbe <- Kader_Voll %>% dplyr::arrange(dplyr::desc(V)) %>% .[1:3,c(1,4)] %>% dplyr::mutate(Gelbe=V) %>% dplyr::select("Name", "Gelbe")

  list(Ganzer_Kader = Kader_Voll, Spieltagskader = Kader_Rest, Meiste_Tore=Meiste_Tore, Meiste_Zeitstrafen=Meiste_Zeitstrafen, Meiste_Gelbe=Meiste_Gelbe)
} # hier überlegen, ob es Sinn macht nicht fehlende Spieler ausgeben zu lassen und nicht den Spieltagskader, zusätzlich könnte man noch die erzielten Tore ausgeben um formstarke Spieler zu identifizieren
