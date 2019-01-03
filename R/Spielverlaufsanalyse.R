#' Title
#'
#' @param Name
#' @param Team
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
Tore_Spielminute_Spieler <- function(Name, Team, Data) {
  Tore_Spieler <- Data[[3]] %>%
    dplyr::filter(Spieler==Name) %>%
    dplyr::mutate(Spielabschnitt=as.numeric(substr(Spielminute, 1,1))+1) %>%
    dplyr::group_by(Datum) %>% dplyr::count(Spielabschnitt)

  Tore_Mannschaft <- Data[[1]] %>%
    dplyr::filter(Mannschaft==Team) %>%
    dplyr::group_by(Datum) %>%
    dplyr::summarise(Tore=sum(Tore))

  Mean_Tore_Mannschaft <- mean(Tore_Mannschaft$Tore)
  VC_Tore_Mannschaft <- sd(Tore_Mannschaft$Tore)/mean(Tore_Mannschaft$Tore)

  Enddaten <- data.table::data.table("1"=0, "2"=0, "3"=0, "4"=0, "5"=0, "6"=0)

  for(i in (1:length(unique(Tore_Spieler$Datum)))) {
    Gegn <- as.character(unique(Data[[1]] %>%
                                  dplyr::filter(Datum==unique(Tore_Spieler$Datum)[i] & Mannschaft==Team) %>%
                                  .$Gegner))

    Mean_Gegentore_Gegner <- mean(Data[[1]] %>%
                                    dplyr::filter(Gegner==Gegn) %>%
                                    dplyr::group_by(Datum)  %>%
                                    dplyr::summarise(Gegentore=sum(Tore)) %>%
                                    .$Gegentore)

    VC_Gegentore_Gegner <- sd(Data[[1]] %>%
                                dplyr::filter(Gegner==Gegn) %>%
                                dplyr::group_by(Datum)  %>%
                                dplyr::summarise(Gegentore=sum(Tore)) %>%
                                .$Gegentore)/Mean_Gegentore_Gegner


    Tore_erzielt_Spieler <- Tore_Spieler %>% dplyr::filter(Datum==unique(Tore_Spieler$Datum)[i])
    Tore_erzielt_Mannschaft <- Tore_Mannschaft %>% dplyr::filter(Datum==unique(Tore_Spieler$Datum)[i])
    Tore_erzielt_Spieler$n <- (Tore_erzielt_Spieler$n/Tore_erzielt_Mannschaft$Tore)*((Mean_Tore_Mannschaft+Mean_Gegentore_Gegner)*(VC_Gegentore_Gegner/VC_Tore_Mannschaft)/2)

    for(j in (1:nrow(Tore_erzielt_Spieler))) {
      if(Tore_erzielt_Spieler$Spielabschnitt[j]==1) {
        Enddaten$`1` <- Enddaten$`1`+Tore_erzielt_Spieler[j,]$n
      } else if(Tore_erzielt_Spieler$Spielabschnitt[j]==2) {
        Enddaten$`2` <- Enddaten$`2`+Tore_erzielt_Spieler[j,]$n
      } else if(Tore_erzielt_Spieler$Spielabschnitt[j]==3) {
        Enddaten$`3` <- Enddaten$`3`+Tore_erzielt_Spieler[j,]$n
      } else if(Tore_erzielt_Spieler$Spielabschnitt[j]==4) {
        Enddaten$`4` <- Enddaten$`4`+Tore_erzielt_Spieler[j,]$n
      } else if(Tore_erzielt_Spieler$Spielabschnitt[j]==5) {
        Enddaten$`5` <- Enddaten$`5`+Tore_erzielt_Spieler[j,]$n
      } else {
        Enddaten$`6` <- Enddaten$`6`+Tore_erzielt_Spieler[j,]$n
      }
    }
  }
  Enddaten <- as.data.frame(t(Enddaten))
   ggplot2::ggplot(Enddaten, ggplot2::aes(x=rownames(Enddaten), y=Enddaten$V1)) +
     ggplot2::geom_col(fill="dark green") +
     ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))+
     ggplot2::xlab("Spielabschnitt") +
     ggplot2::ylab("Torgefährlichkeit") +
     ggplot2::ggtitle(label=paste("Torgefährlichkeit ", Name))
}


