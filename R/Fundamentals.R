#' Title
#'
#' @param Team
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
Fundamentals_Team <- function(Team, Data) {
  Mean_Tore <- mean(
    Data[[1]] %>%
                      dplyr::filter(Mannschaft==Team) %>%
                      dplyr::group_by(Datum) %>%
                      dplyr::summarise(Tore=sum(Tore)) %>%
                      .$Tore
    )

  Mean_Gegentore <- mean(
    Data[[1]] %>%
    dplyr::filter(Gegner==Team) %>%
    dplyr::group_by(Datum) %>%
    dplyr::summarise(Gegentore=sum(Tore)) %>%
      .$Gegentore
    )

  Mean_Gelbe <- mean(
    Data[[1]] %>%
    dplyr::filter(Mannschaft==Team) %>%
    dplyr::group_by(Datum) %>%
    dplyr::summarise(Gelbe=sum(V)) %>%
    .$Gelbe
  )

  Mean_Zeitstrafen <- mean(
    Data[[1]] %>%
    dplyr::filter(Mannschaft==Team) %>%
    dplyr::group_by(Datum) %>%
    dplyr::summarise(Zeitstrafen=sum(`2min`)) %>%
    .$Zeitstrafen
  )

  Output <- data.table::data.table("durschn. Tore"=Mean_Tore, "durchschn. Gegentore"=Mean_Gegentore, "Zeitstrafen/Spiel"=Mean_Zeitstrafen, "Gelbe Karten/Spiel"= Mean_Gelbe)
  Output
  }


