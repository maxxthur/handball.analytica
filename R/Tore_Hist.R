#' Title
#'
#' @param Team
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
Tore_Hist <- function(Team, Data) {
  Gegner  <- Data[[2]] %>% dplyr::filter(Heim==Team | Gast==Team)
  Nottuln <- Data[[2]] %>% dplyr::filter(Heim=="DJK Grün-Weiß Nottuln" | Gast=="DJK Grün-Weiß Nottuln")

  Tore_Gegner <- data.frame(Tore=c(), Gegner=c())
for(i in (1:nrow(Gegner))) {
    if(Gegner$Heim[i]==Team) {
    Tore_Gegner <- rbind(Tore_Gegner,cbind(Gegner$Tore_Heim[i], Gegner$Gast[i]))
    } else {Tore_Gegner <- rbind(Tore_Gegner,cbind(Gegner$Tore_Gast[i], Gegner$Heim[i]))}
}

  Gegentore_Gegner <- data.frame(Gegentore=c(), Gegner=c())
  for(j in (1:nrow(Gegner))) {
    if(Gegner$Heim[j]==Team) {
      Gegentore_Gegner <- rbind(Gegentore_Gegner,cbind(Gegner$Tore_Gast[j], Gegner$Gast[j]))
    } else {Gegentore_Gegner <- rbind(Gegentore_Gegner,cbind(Gegner$Tore_Heim[j], Gegner$Heim[j]))}
  }

  Tore_Nottuln <- data.frame(Tore=c(), Gegner=c())
  for(i in (1:nrow(Nottuln))) {
    if(Nottuln$Heim[i]=="DJK Grün-Weiß Nottuln") {
      Tore_Nottuln <- rbind(Tore_Nottuln,cbind(Nottuln$Tore_Heim[i], Nottuln$Gast[i]))
    } else {Tore_Nottuln <- rbind(Tore_Nottuln,cbind(Nottuln$Tore_Gast[i], Nottuln$Heim[i]))}
  }

  Gegentore_Nottuln <- data.frame(Gegentore=c(), Gegner=c())
  for(j in (1:nrow(Nottuln))) {
    if(Nottuln$Heim[j]==Team) {
      Gegentore_Nottuln <- rbind(Gegentore_Nottuln,cbind(Nottuln$Tore_Gast[j], Nottuln$Gast[j]))
    } else {Gegentore_Nottuln <- rbind(Gegentore_Nottuln,cbind(Nottuln$Tore_Heim[j], Nottuln$Heim[j]))}
  }

  #Tore_Nottuln <- xts::xts(Tore_Nottuln, order.by = Nottuln$Datum)
  #Gegentore_Nottuln <- xts::xts(Gegentore_Nottuln, order.by = Nottuln$Datum)
  #Tore_Gegner <- xts::xts(Tore_Gegner, order.by = Gegner$Datum)
  #Gegentore_Gegner <- xts::xts(Gegentore_Nottuln, order.by = Gegner$Datum)


}
