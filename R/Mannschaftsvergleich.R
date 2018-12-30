#' Title
#'
#' @param Team
#' @param Data
#'
#' @return
#' @export
#'
#' @examples

Mannschaftsvergleich <- function(Team, Data) {
  Gegner <- Data[[1]] %>% dplyr::filter(Mannschaft==Team)
  Nottuln <- Data[[1]] %>% dplyr::filter(Mannschaft=="DJK Grün-Weiß Nottuln")
  Performance_Gegner <- Data[[2]] %>% dplyr::filter(Heim==Team | Gast==Team)
  Performance_Nottuln <- Data[[2]] %>% dplyr::filter(Heim=="DJK Grün-Weiß Nottuln" | Gast=="DJK Grün-Weiß Nottuln")
  Anzahl_Spiele_Gegner <- nrow(Performance_Gegner)
  Anzahl_Spiele_Nottuln <- nrow(Performance_Nottuln)
  Beste3_Nottuln <- Nottuln %>% dplyr::group_by(Name, Mannschaft) %>% dplyr::summarise(Tore = sum(Tore)) %>% dplyr::arrange(desc(Tore)) %>% .[1:3,]
  Alle_Nottuln <- Nottuln %>% dplyr::group_by(Name, Mannschaft) %>% dplyr::summarise(Tore = sum(Tore)) %>% dplyr::arrange(desc(Tore))
  Beste3_Gegner <- Gegner %>% dplyr::group_by(Name, Mannschaft) %>% dplyr::summarise(Tore = sum(Tore)) %>% dplyr::arrange(desc(Tore)) %>% .[1:3,]
  Alle_Gegner <- Gegner  %>% dplyr::group_by(Name, Mannschaft) %>% dplyr::summarise(Tore = sum(Tore)) %>% dplyr::arrange(desc(Tore))


  Gegner_Tore <- sum(sum(Performance_Gegner$Tore_Heim[Performance_Gegner$Heim==Team]), sum(Performance_Gegner$Tore_Gast[Performance_Gegner$Gast==Team]))/Anzahl_Spiele_Gegner
  Gegner_Gegentore <- (sum(Performance_Gegner$Tore_Gast[Performance_Gegner$Heim==Team])+sum(Performance_Gegner$Tore_Heim[Performance_Gegner$Gast==Team]))/Anzahl_Spiele_Gegner
  Gegner_Zeitstrafen <- sum(Gegner$`2min`)/Anzahl_Spiele_Gegner
  Gegner_Gelbe <- sum(Gegner$V)/Anzahl_Spiele_Gegner
  Gegner_Konzentration <- sum(Beste3_Gegner$Tore)/sum(Alle_Gegner$Tore)


  Nottuln_Tore <- sum(Nottuln$Tore)/Anzahl_Spiele_Nottuln
  Nottuln_Gegentore <- (sum(Performance_Nottuln$Tore_Gast[Performance_Nottuln$Heim=="DJK Grün-Weiß Nottuln"])+sum(Performance_Nottuln$Tore_Heim[Performance_Nottuln$Gast=="DJK Grün-Weiß Nottuln"]))/Anzahl_Spiele_Nottuln
  Nottuln_Zeitstrafen <- sum(Nottuln$`2min`)/Anzahl_Spiele_Nottuln
  Nottuln_Gelbe <- sum(Nottuln$V)/Anzahl_Spiele_Nottuln
  Nottuln_Konzentration <- sum(Beste3_Nottuln$Tore)/sum(Alle_Nottuln$Tore)

  Nottuln <- data.frame(Nottuln_Tore, Nottuln_Gegentore, Nottuln_Zeitstrafen, Nottuln_Gelbe, Nottuln_Konzentration)
  Gegner <- data.frame(Gegner_Tore, Gegner_Gegentore, Gegner_Zeitstrafen, Gegner_Gelbe, Gegner_Konzentration)

  colnames(Nottuln) <- c("Tore", "Gegentore", "Zeitstrafen", "Gelbe Karten", "Konzentration")
  colnames(Gegner) <- c("Tore", "Gegentore", "Zeitstrafen", "Gelbe Karten", "Konzentration")

  Stats <- rbind(Nottuln, Gegner)
  rownames(Stats) <- c("Nottuln", "Gegner")

  a <- ggplot2::ggplot(Stats, ggplot2::aes(y=Tore, x=rownames(Stats))) +
    ggplot2::geom_bar(position="dodge", stat="identity", width = 0.5, fill=c("red", "green")) + ggplot2::xlab("") + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                                                                                                                        panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  b <- ggplot2::ggplot(Stats, ggplot2::aes(y=Gegentore, x=rownames(Stats))) +
    ggplot2::geom_bar(position="dodge", stat="identity", width = 0.5, fill=c("red", "green")) + ggplot2::xlab("") + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                                                                                                                          panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  c <- ggplot2::ggplot(Stats, ggplot2::aes(y=Zeitstrafen, x=rownames(Stats))) +
    ggplot2::geom_bar(position="dodge", stat="identity", width = 0.5, fill=c("red", "green")) + ggplot2::xlab("") + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                                                                                                                          panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  d <- ggplot2::ggplot(Stats, ggplot2::aes(y=`Gelbe Karten`, x=rownames(Stats))) +
    ggplot2::geom_bar(position="dodge", stat="identity", width = 0.5, fill=c("red", "green")) + ggplot2::xlab("") + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                                                                                                                          panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  e <- ggplot2::ggplot(Stats, ggplot2::aes(y=Konzentration, x=rownames(Stats))) +
    ggplot2::geom_bar(position="dodge", stat="identity", width = 0.5, fill=c("red", "green")) + ggplot2::xlab("") + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                                                                                                                                   panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  Output <- ggpubr::ggarrange(a,b,c,d,e, ncol=5, nrow=1)
  Output <- ggpubr::annotate_figure(Output, top="Mannschaften im Vergleich")
  Output
}

