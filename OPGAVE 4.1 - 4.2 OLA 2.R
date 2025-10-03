library(readxl)
install.packages("zoo")
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

#Opgave 4.1

#Vi starter med at indlæse Excel-filen Forv5.xlsx i R, 
#så vi kan arbejde med DST’s forbrugertillidsindikator

forv5 <- read_excel("Forv5.xlsx")


#2 Med mutate() laver vi en ny kolonne, der indeholder en rigtig dato. 
#Det gør vi ved at tage år og måned fra variablen Måned og samle dem til en fuld dato. 
#Derefter bruger vi funktionen as.yearqtr() fra pakken zoo
#til at omdanne datoen til et kvartal.

forv5 <- forv5 %>%
  mutate(
    Dato = as.Date(paste0(substr(Måned, 1, 4), "-", substr(Måned, 6, 7), "-01")),
    Kvartal = as.yearqtr(Dato)
  )

#3) Her grupperer vi datasættet efter kvartal med group_by(Kvartal). 
#Derefter bruger vi summarise() til at beregne gennemsnittet af Forbrugertillidsindikatoren 
#for hvert kvartal.Resultatet bliver en ny tabel 
#(forbrugK), som viser ét gennemsnitstal pr. kvartal.

forbrugK <- forv5 %>%
  group_by(Kvartal) %>%
  summarise(
    ForbT = mean(Forbrugertillidsindikatoren)
  )

#Til sidst laver vi en graf med ggplot(), 
#hvor x-aksen viser kvartalerne og y-aksen gennemsnittet af forbrugertilliden. 
#Den blå linje viser udviklingen over tid, mens den røde stiplet linje markerer 
#nulpunktet mellem optimisme og pessimisme. Titel

ggplot(forbrugK, aes(as.Date(Kvartal), ForbT)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "DST’s Forbrugertillidsindikator i kvartaler",
    x = "År",
    y = "Netto balance i pct"
  ) 


#Opgave 4.2

#Her gemmer vi navnet på den kolonne i forv5, som vi vil analysere. 
#col er altså en tekststreng med det kolonnenavn fra forv5 som vi skal bruge

col <- "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"

#Vi laver et udsnit af forv5 og gemmer det som forvsub. 
#Kun rækker, hvor Kvartal ligger mellem 2000 Q1 og 2025 Q3, kommer med.

forvsub <- subset(forv5, Kvartal >= "2000 Q1" & Kvartal <= "2025 Q2")

# Nu skal vi Beregne gennemsnittet af den valgte kolonne i perioden 2000Q1-2024Q3.

# Det giver et samlet tal, som viser det gennemsnitlige niveau for hele perioden.
mean(forvsub[[col]])
min(forvsub[[col]])
max(forvsub[[col]])


#Vi har undersøgt variablen “Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket”
#i perioden 2000 Q1 til 2025 Q2. Gennemsnittet blev –10,4, hvilket viser, 
#at forbrugerne i gennemsnit har været mere pessimistiske end optimistiske 
#omkring større forbrugsgoder i denne periode.



ggplot(forvsub, aes(x = Kvartal, y = .data[[col]])) +
  geom_line(color = "blue", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "Anskaffelse af større forbrugsgoder",
    subtitle = "2000 Q1 - 2025 Q2",
    x = "År",
    y = "Nettotal"
  )




