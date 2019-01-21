#' Title
#'
#' @param liga
#' @param Old_URL
#' @param Old_Data
#'
#' @return
#' @export
#'
#' @examples
refresh_sis <- function(liga = "001519505501509501000000000000000002007", Old_URL, Old_Data) {
  URL_Alle_Spiele <- paste0("http://www.sis-handball.de/default.aspx?view=AlleSpiele&Liga=", liga)
  Dataset <- data.table::data.table()
  AllURLs <- rvest::html_attr(rvest::html_nodes(xml2::read_html(URL_Alle_Spiele), "a"), "href")
  URLs_Liveticker <- AllURLs[grep("^http://liveticker.sis-handball.org/game/", AllURLs)]
  URLs_Liveticker <- URLs_Liveticker[URLs_Liveticker != Old_URL]
  Tordatenbank <- data.table::data.table()
  Strafendatenbank <- data.table::data.table()
  GelbeKartenDatenbank <- data.table::data.table()
  for (i in (1:length(URLs_Liveticker))) {
    home <- URLs_Liveticker[i] %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//*[@id="teams"]/div[1]') %>%
      rvest::html_nodes(xpath = '//*[@id="home"]') %>%
      rvest::html_table() %>%
      .[[1]]

    hometeam <- URLs_Liveticker[i] %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//*[@id="teams"]/div[1]/h3') %>%
      rvest::html_text() %>%
      .[[1]]

    guest <- URLs_Liveticker[i] %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//*[@id="teams"]/div[2]') %>%
      rvest::html_nodes(xpath = '//*[@id="guest"]') %>%
      rvest::html_table() %>%
      .[[1]]

    guestteam <- URLs_Liveticker[i] %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//*[@id="teams"]/div[2]/h3') %>%
      rvest::html_text() %>%
      .[[1]]

    date <- URLs_Liveticker[i] %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = "/html/body/div[1]/div[3]/table[1]/tbody/tr/td[1]") %>%
      rvest::html_text() %>%
      .[[1]]

    date <- as.Date(paste(substr(date, 7, 10), "-", substr(date, 4, 5), "-", substr(date, 1, 2), sep = ""))

    homecomplete <- cbind(Datum = rep(date, each = nrow(home)), Mannschaft = rep(hometeam, each = nrow(home)), Gegner = rep(guestteam, each = nrow(home)), home)
    guestcomplete <- cbind(Datum = rep(date, each = nrow(guest)), Mannschaft = rep(guestteam, each = nrow(guest)), Gegner = rep(hometeam, each = nrow(guest)), guest)
    Gamecomplete <- rbind(homecomplete, guestcomplete)
    Dataset <- rbind(Dataset, Gamecomplete) #hier überprüfen

    Spielverlauf <- URLs_Liveticker[i] %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//*[@id="events"]') %>%
      rvest::html_table() %>%
      .[[1]]

    Spielverlauf_Tore <- Spielverlauf[grep("Tor ", Spielverlauf$X3),]
    Verein <- sub("\\).*", "", sub(".*\\(", "", Spielverlauf_Tore$X3)) #extrahiert den Verein
    Spieler <- substr(sub("[[:digit:]]+","", sub("Tor nach 7-Meter durch ","", sub("\\(.*", "", sub(".*\\Tor durch ", "", Spielverlauf_Tore$X3)))), 2, (nchar(sub("[[:digit:]]+","", sub("\\(.*", "", sub(".*\\Tor durch ", "", Spielverlauf_Tore$X3))))-1))
    Spielstand <- sub("\\).*", "", sub(".*\\(", "", Spielverlauf_Tore$X1)) #extrahiert den Spielstand
    Spielminute <- substr(Spielverlauf_Tore$X1, 1, 5)

    Torverlauf <- data.frame(Spielminute, Spielstand, Verein, Spieler)
    Torverlauf <- cbind(Datum = rep(date, each = nrow(Torverlauf)), Torverlauf)
    Torverlauf$Spieler <- as.character(Spieler)

    Tordatenbank <- rbind(Tordatenbank, Torverlauf)

    Spielverlauf_2min <- Spielverlauf[grep("2 Minuten Strafe", Spielverlauf$X3),]
    Verein <- sub("\\).*", "", sub(".*\\(", "", Spielverlauf_2min$X3)) #extrahiert den Verein
    Spieler <- substr(sub("[[:digit:]]+","", sub("2 Minuten Strafe gegen ","", sub("\\(.*", "", Spielverlauf_2min$X3))), 2, (nchar(sub("[[:digit:]]+","", sub("\\(.*", "", sub(".*\\'2 Minuten Strafe gegen '", "", Spielverlauf_2min$X3))))-1))
    Spieler <- substr(Spieler, 1, (nchar(Spieler)-1))
    Spielminute <- substr(Spielverlauf_2min$X1, 1, 5)
    Verlauf_2min <- data.frame(Spielminute, Verein, Spieler)
    Verlauf_2min <- cbind(Datum = rep(date, each = nrow(Verlauf_2min)), Verlauf_2min)
    Verlauf_2min$Spieler <- as.character(Spieler)
    Strafendatenbank <- rbind(Strafendatenbank, Verlauf_2min)

    Spielverlauf_GelbeKarten <- Spielverlauf[grep("Verwarnung gegen", Spielverlauf$X3),]
    Verein <- sub("\\).*", "", sub(".*\\(", "", Spielverlauf_GelbeKarten$X3)) #extrahiert den Verein
    Spieler <- substr(sub("[[:digit:]]+","", sub("Verwarnung gegen ","", sub("\\(.*", "", Spielverlauf_GelbeKarten$X3))), 2, (nchar(sub("[[:digit:]]+","", sub("\\(.*", "", sub(".*\\'Verwarnung gegen '", "", Spielverlauf_GelbeKarten$X3))))-1))
    Spieler <- substr(Spieler, 1, (nchar(Spieler)-1))
    Spielminute <- substr(Spielverlauf_GelbeKarten$X1, 1, 5)
    Verlauf_GK <- data.frame(Spielminute, Verein, Spieler)
    Verlauf_GK <- cbind(Datum = rep(date, each = nrow(Verlauf_GK)), Verlauf_GK)
    Verlauf_GK$Spieler <- as.character(Spieler)

    GelbeKartenDatenbank <- rbind(GelbeKartenDatenbank, Verlauf_GK)
  }


  Results <- URL_Alle_Spiele %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="ctl00_ContentPlaceHolder1_Panel1"]/div[4]/table') %>%
    rvest::html_table() %>%
    .[[1]] %>%
    .[, c(2, 4, 6, 7)] %>%
    dplyr::mutate(Tore_Heim = as.numeric(substr(.[, 4], 1, 2)), Tore_Gast = as.numeric(substr(.[, 4], 4, 5))) %>%
    dplyr::mutate(Datum = as.Date(paste("20", substr(Datum, 7, 8), "-", substr(Datum, 4, 5), "-", substr(Datum, 1, 2), sep = "")))

  Dataset$V[Dataset$V == "x"] <- 1
  Dataset$V[Dataset$V == ""] <- 0
  Dataset$V <- as.integer(Dataset$V)
  Data <- list(Dataset, Results, Tordatenbank, Strafendatenbank, GelbeKartenDatenbank, URLs_Liveticker)
  Data <- list(rbind(Old_Data[[1]],Data[[1]]), Data[[2]], rbind(Old_Data[[3]], Data[[3]]), rbind(Old_Data[[4]], Data[[4]]), rbind(Old_Data[[5]], Data[[5]]), c(Old_URL, URLs_Liveticker)) # hier überprüfen
  saveRDS(Data, file="data.rds")
  Data
}
