#' Title
#'
#' @param Team
#' @param Data
#' @param ordering
#'
#' @return
#' @export
#'
#' @examples
Kader <- function(Team, Data) {
  TeamData <- TeamData <- Data[[1]] %>%
    dplyr::filter(Mannschaft == Team & !(`#` %in% c("OA", "OB", "OC", "OD", "OE", "OF")))

  Kader_Voll <- TeamData %>%
    dplyr::group_by(Name, Mannschaft) %>%
    dplyr::summarise(Tore = sum(Tore) , Zeitstrafen = sum(`2min`), V = sum(V)) %>%
    dplyr::mutate("% Tore"=round((Tore/sum(.$Tore))*100,2)) %>%
    dplyr::select("Name", "Tore","% Tore", "Zeitstrafen", "V")

  Tore_Mannschaft <- Data[[1]] %>% dplyr::filter(Mannschaft==Team) %>% dplyr::group_by(Datum) %>% dplyr::summarise(Tore=sum(Tore))

  Abhängigkeit <- data.table::data.table()

  for(i in (1:length(Kader_Voll$Name))) {
    Tore_Spieler <- Data[[1]] %>% dplyr::filter(Mannschaft==Team, Name==Kader_Voll$Name[i] & !(`#` %in% c("OA", "OB", "OC", "OD", "OE", "OF")))
    Tore_M_S <- cbind(Tore_Mannschaft %>% dplyr::filter(Datum %in% Tore_Spieler$Datum), Tore_S=Tore_Spieler$Tore)
    Korrelation <- cor(Tore_M_S$Tore, Tore_M_S$Tore_S)
    Abh <- round(Kader_Voll$`% Tore`[i]*(1-Korrelation), 3)
    Abhängigkeit <- rbind(Abhängigkeit,cbind(Name=Kader_Voll$Name[i], Abhängigkeit=Abh))
  }

  Output <- dplyr::left_join(Kader_Voll, Abhängigkeit)
  ordering = if (order=="Tore") {
    Output$Tore
  } else if (order=="Abhängigkeit") {
    Output$Abhängigkeit
  } else if (order=="Zeitstrafen") {
    Output$Zeitstrafen
  } else if (order=="Gelbe Karten") {
    Output$V
  } else {Output$Tore}

  Output
}



