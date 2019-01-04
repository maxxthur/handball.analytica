
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
Performance_last <- function(Team, Data) {
  Results <- data.table::data.table()
  TeamData <- Data[[2]] %>% dplyr::filter(Heim == "DJK Grün-Weiß Nottuln" | Gast == "DJK Grün-Weiß Nottuln")
  Tore_Mannschaft <- Data[[1]] %>% dplyr::filter(Mannschaft=="DJK Grün-Weiß Nottuln") %>% dplyr::group_by(Datum, Name) %>% dplyr::summarise(Tore=sum(Tore)) %>% dplyr::ungroup() %>% dplyr::mutate(Datum=as.character(Datum))
  for (i in ((nrow(TeamData) - 2):nrow(TeamData))) {
    if (TeamData$Heim[i] == "DJK Grün-Weiß Nottuln") {
      if (TeamData$Tore_Heim[i] > TeamData$Tore_Gast[i]) {
        Results <- rbind(Results, cbind(as.character(TeamData$Datum[i]),"Sieg", "Heim", TeamData$Gast[i], TeamData$Tore[i]))
      } else if (TeamData$Tore_Heim[i] == TeamData$Tore_Gast[i]) {
        Results <- rbind(Results, cbind(as.character(TeamData$Datum[i]),"Unentschieden", "Heim", TeamData$Gast[i], TeamData$Tore[i]))
      } else {
        Results <- rbind(Results, cbind(as.character(TeamData$Datum[i]),"Niederlage", "Heim", TeamData$Gast[i], TeamData$Tore[i]))
      }
    } else {
      if (TeamData$Tore_Gast[i] > TeamData$Tore_Heim[i]) {
        Results <- rbind(Results, cbind(as.character(TeamData$Datum[i]), "Sieg", "Auswärts", TeamData$Heim[i], TeamData$Tore[i]))
      } else if (TeamData$Tore_Gast[i] == TeamData$Tore_Heim[i]) {
        Results <- rbind(Results, cbind(as.character(TeamData$Datum[i]),"Unentschieden", "Auswärts", TeamData$Heim[i], TeamData$Tore[i]))
      } else {
        Results <- rbind(Results, cbind(as.character(TeamData$Datum[i]),"Niederlage", "Auswärts", TeamData$Heim[i], TeamData$Tore[i]))
      }
    }
  }
  Beste <- Tore_Mannschaft %>%
    dplyr::group_by(Datum, Name) %>%
    dplyr::summarise(Tore=sum(Tore)) %>%
     dplyr::filter(Datum %in% Results$V1) %>%
     dplyr::group_by(Name) %>%
     dplyr::summarise("durchschn. Tore"=mean(Tore)) %>%
     dplyr::arrange(desc(`durchschn. Tore`)) %>%
     .[1:3,]

  Results <- cbind(Results, Beste)
  colnames(Results) <- c("Datum", "S/N", "Ort", "Gegner", "Ergebnis", "Beste Torschützen", "durchschn. Tore")
  Results
}
