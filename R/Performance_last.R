
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
Performance_last <- function(Team, Data, Matches) {
  Results <- data.table::data.table()
  TeamData <- Data[[2]] %>% dplyr::filter(Heim == Team | Gast == Team)
  for (i in ((nrow(TeamData) - Matches):nrow(TeamData))) {
    if (TeamData$Heim[i] == Team) {
      if (TeamData$Tore_Heim[i] > TeamData$Tore_Gast[i]) {
        Results <- rbind(Results, cbind("Sieg", "Heim", TeamData$Gast[i], TeamData$Tore[i]))
      } else if (TeamData$Tore_Heim[i] == TeamData$Tore_Gast[i]) {
        Results <- rbind(Results, cbind("Unentschieden", "Heim", TeamData$Gast[i], TeamData$Tore[i]))
      } else {
        Results <- rbind(Results, cbind("Niederlage", "Heim", TeamData$Gast[i], TeamData$Tore[i]))
      }
    } else {
      if (TeamData$Tore_Gast[i] > TeamData$Tore_Heim[i]) {
        Results <- rbind(Results, cbind("Sieg", "Auswärts", TeamData$Heim[i], TeamData$Tore[i]))
      } else if (TeamData$Tore_Gast[i] == TeamData$Tore_Heim[i]) {
        Results <- rbind(Results, cbind("Unentschieden", "Auswärts", TeamData$Heim[i], TeamData$Tore[i]))
      } else {
        Results <- rbind(Results, cbind("Niederlage", "Auswärts", TeamData$Heim[i], TeamData$Tore[i]))
      }
    }
  }
  colnames(Results) <- c("S/N", "Ort", "Gegner", "Ergebnis")
  Results
}
