#' Title
#'
#' @param Mannschaft
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
Zeitstrafen_Spielminute_Mannschaft <- function(Mannschaft, Data) {
  Mannschaft_Zeitstrafen <- Data[[4]] %>%
    dplyr::filter(Verein==Mannschaft) %>%
    dplyr::mutate(Spielabschnitt=as.numeric(substr(Spielminute, 1, 1))+1)

  Enddaten <- data.table::data.table("1"=0, "2"=0, "3"=0, "4"=0, "5"=0, "6"=0)

  for(i in (1:nrow(Mannschaft_Zeitstrafen))) {
    if(Mannschaft_Zeitstrafen$Spielabschnitt[i]==1) {
      Enddaten$`1` <- Enddaten$`1`+1
    } else if (Mannschaft_Zeitstrafen$Spielabschnitt[i]==2) {
      Enddaten$`2` <- Enddaten$`2`+1
    } else if (Mannschaft_Zeitstrafen$Spielabschnitt[i]==3) {
      Enddaten$`3` <- Enddaten$`3`+1
    } else if (Mannschaft_Zeitstrafen$Spielabschnitt[i]==4) {
      Enddaten$`4` <- Enddaten$`4`+1
    } else if (Mannschaft_Zeitstrafen$Spielabschnitt[i]==5) {
      Enddaten$`5` <- Enddaten$`5`+1
    } else {
      Enddaten$`6` <- Enddaten$`6`+1
    }
  }

  Enddaten <- as.data.frame(t(Enddaten))
  ggplot2::ggplot(Enddaten, ggplot2::aes(x=rownames(Enddaten), y=Enddaten$V1)) +
    ggplot2::geom_col(fill="dark red") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))+
    ggplot2::xlab("Spielabschnitt") +
    ggplot2::ylab("Zeitstrafen") +
    ggplot2::ggtitle(label=paste("2 Minuten ", Mannschaft))
}
