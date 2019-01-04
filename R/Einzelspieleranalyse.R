#' Title
#'
#' @param Player
#' @param Team
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
Einzelspieleranalyse <- function(Player, Team, Data){
  Mean_Tore <- mean(
    Data[[1]] %>%
      dplyr::filter(Name==Player) %>%
      dplyr::group_by(Datum) %>%
      dplyr::summarise(Tore=sum(Tore)) %>%
      .$Tore
  )

  Gelbe_Gesamt <- sum(
    Data[[1]] %>%
      dplyr::filter(Name==Player) %>%
      dplyr::group_by(Datum) %>%
      dplyr::summarise(Gelbe=V) %>%
      .$Gelbe
  )

  Zeitstrafen_Gesamt <- sum(
    Data[[1]] %>%
      dplyr::filter(Name==Player) %>%
      dplyr::group_by(Datum) %>%
      dplyr::summarise(Zeitstrafen=`2min`) %>%
      .$Zeitstrafen
  )

  Fundamentals <- data.table::data.table("durchsch. Tore"=Mean_Tore, "Zeitstrafen/Saison"=Zeitstrafen_Gesamt, "Gelbe Karten/Saison"=Gelbe_Gesamt)

  Tore_Spiele <- Data[[1]] %>%
    dplyr::filter(Name==Player) %>%
    dplyr::group_by(Datum, Gegner) %>%
    dplyr::summarise(Tore=Tore) %>%
    ggplot2::ggplot(ggplot2::aes(x=Gegner, y=Tore)) +
    ggplot2::geom_col(fill="dark green") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))+
    ggplot2::xlab("Gegner") +
    ggplot2::ylab("Tore") +
    ggplot2::ggtitle(label=paste("Tore ", Player))

  Zeitstrafen_Spiele <- Data[[1]] %>%
    dplyr::filter(Name==Player) %>%
    dplyr::group_by(Datum, Gegner) %>%
    dplyr::summarise(Zeitstrafen=`2min`) %>%
    ggplot2::ggplot(ggplot2::aes(x=Gegner, y=Zeitstrafen)) +
    ggplot2::geom_col(fill="dark red") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))+
    ggplot2::xlab("Gegner") +
    ggplot2::ylab("Zeitstrafen") +
    ggplot2::ggtitle(label=paste("Zeitstrafen ", Player))

  Gelbe_Spiele <- Data[[1]] %>%
    dplyr::filter(Name==Player) %>%
    dplyr::group_by(Datum, Gegner) %>%
    dplyr::summarise(Gelbe=V) %>%
    ggplot2::ggplot(ggplot2::aes(x=Gegner, y=Gelbe)) +
    ggplot2::geom_col(fill="orange") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))+
    ggplot2::xlab("Gegner") +
    ggplot2::ylab("Gelbe Karten") +
    ggplot2::ggtitle(label=paste("Gelbe Karten ", Player))

  Torgefahr <- handball.analytica::Tore_Spielminute_Spieler(Player, Team, Data)
  Zeitstrafen <- handball.analytica::Zeitstrafen_Spielverlauf_Spieler(Player, Data)
  Gelbe <- handball.analytica::GelbeKarten_Spielverlauf_Spieler(Player, Data)

  plot1 <- ggpubr::ggarrange(Torgefahr, Zeitstrafen, Gelbe, ncol=3, nrow=1)
  plot <- ggpubr::ggarrange(Tore_Spiele, Zeitstrafen_Spiele, Gelbe_Spiele, plot1, ncol=1, nrow=4)
  list(plot, Fundamentals)
}
