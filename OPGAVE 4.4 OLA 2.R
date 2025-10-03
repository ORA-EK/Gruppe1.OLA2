library(readxl)
install.packages("zoo")
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)



#Opgave 4.4

#Vi starter med at indlæse Excel-filen Forv5.xlsx i R, 
#så vi kan arbejde med DST’s forbrugertillidsindikator

forv5 <- read_excel("Forv5.xlsx")


#2 Med mutate() laver vi en ny kolonne, der indeholder en rigtig dato. 
#Det gør vi ved at tage år og måned fra variablen Måned og samle dem til en fuld dato. 
#Derefter bruger vi funktionen as.yearqtr()
#til at omdanne datoen til et kvartal.

forv5 <- forv5 %>%
  mutate(
    Dato = as.Date(paste0(substr(Måned, 1, 4), "-", substr(Måned, 6, 7), "-01")),
    Kvartal = as.yearqtr(Dato)
  )

#3) Her grupperer vi datasættet efter kvartal med group_by(Kvartal). 
#Derefter bruger vi summarise() til at beregne gennemsnittet af Forbrugertillidsindikatoren 
#for hvert kvartal.Resultatet bliver en ny tabel 
#(forbrug_vartal), som viser ét gennemsnitstal pr. kvartal.

forbrug_kvartal <- forv5 %>%
  group_by(Kvartal) %>%
  summarise(
    Forbrugertillidsindikatoren = mean(Forbrugertillidsindikatoren)
  )
forbrug_kvartal2 <- subset(forbrug_kvartal, Kvartal >= "2000 Q1" & Kvartal <= "2025 Q2")



#Vi starter med at indlæse Excel-filen Forbdi.xlsx i R, 
#så vi kan arbejde med DI’s forbrugertillidsindikator

forbdi <- read_excel("forbdi.xlsx")

#2 Med mutate() laver vi en ny kolonne, der indeholder en rigtig dato. 
#Det gør vi ved at tage år og måned fra variablen Måned og samle dem til en fuld dato. 
#til at omdanne datoen til et kvartal.

forbdi <- forbdi %>%
  mutate(
    Dato = as.Date(paste0(substr(Måned, 1, 4), "-", substr(Måned, 6, 7), "-01")),
    Kvartal = as.yearqtr(Dato)
  )


#DI forbrugertillidsindikatoren i kvartal
forbdi_kvartal <- forbdi %>%
  group_by(Kvartal) %>%
  summarise(across(-c(Måned, Dato), mean, na.rm = TRUE))


#Henter vi de 15 forbrugsgrupper 

forb_grupper <- read_excel("forb_grupper.xlsx")

#log() bruges for at lave vækstrater.
#diff(..., lag=4) betyder forskellen fra samme kvartal året før 
#*100 gør det til procent.
forb_fv <- c(diff(log(forb_grupper$`Fødevarer mv.`),lag=4)*100)


#Her tager vi de fire kolonner med DI’s forbrugertillidsspørgsmål spørgsmål ud af datasættet "forbdi_kvartal."
spg1<- forbdi_kvartal[[2]]
spg2 <- forbdi_kvartal[[3]]
spg3 <- forbdi_kvartal[[4]]  
spg4 <- forbdi_kvartal[[5]] 

#Her laver vi gennemsnittet af de fire spørgsmål, så man får et samlet DI-forbrugertillidsindikator.
di_samlet <- c(spg1+spg2+spg3+spg4)/4

dataf <- forb_grupper[,-1]   # alle kolonner undtagen Kvartal

#apply(..., 2, ...) kører funktionen kolonne for kolonne.
#rep(NA, 4) tilføjer 4 tomme rækker i starten, så længden passer 
#fordi man mister de første 4 kvartaler, når man laver lag=4.
dataf_pct <- apply(dataf, 2, function(x) c(rep(NA, 4), diff(log(x), lag = 4) * 100))

# fjerner NA'er 
dataf_pct <- dataf_pct[complete.cases(dataf_pct), ]

#Konverterer matrixen til et regulært dataframe med kolonnenavne.
df_pct <- data.frame(dataf_pct)


#Her tager vi hver af de 15 forbrugsgrupper og kører en lineær regression, hvor
#y afhængig variabel = den årlige procentvise ændring i forbruget for gruppen fra "df_pct"
#x uafhængig variabel = DI’s samlede forbrugertillidsindikator "di_samlet"
#Med summary() ser man for hver model, om der er en signifikant sammenhæng -
#hældningskoefficienten (β), og hvor meget variation (R²) indikatoren kan forklare i forbruget.

di1 <- lm(df_pct$Fødevarer.mv.~di_samlet)
summary(di1)

di2 <- lm(df_pct$Drikkevarer.og.tobak.mv.~di_samlet)
summary(di2)

di3 <- lm(df_pct$Beklædning.og.fodtøj~di_samlet)
summary(di3)

di4 <- lm(df_pct$Boligbenyttelse~di_samlet)
summary(di4)

di5 <- lm(df_pct$Elektricitet..fjernvarme.og.andet.brændsel~di_samlet)
summary(di5)

di6 <- lm(df_pct$Boligudstyr..husholdningstjenester.mv.~di_samlet)
summary(di6)

di7 <- lm(df_pct$Medicin..lægeudgifter.o.l.~di_samlet)
summary(di7)

di8 <- lm(df_pct$Køb.af.køretøjer~di_samlet)
summary(di8)

di9 <-  lm(df_pct$Drift.af.køretøjer.og.transporttjenester~di_samlet)
summary(di9)

di10 <- lm(df_pct$Information.og.kommunikation~di_samlet)


di11 <- lm(df_pct$Fritid..sport.og.kultur~di_samlet)

di12 <- lm(df_pct$Undervisning~di_samlet)

di13 <- lm(df_pct$Restauranter.og.hoteller~di_samlet)

di14 <- lm(df_pct$Forsikring.og.finansielle.tjenester~di_samlet)

di15 <- lm(df_pct$Andre.varer.og.tjenester~di_samlet)

#Her laver vi præcis samme type regressioner som med DI, 
#men nu er det DST’s forbrugertillidsindikator, der bruges som forklaringsvariabel
#y afhængig variabel = den årlige procentvise ændring i forbruget for hver af de 15 forbrugsgrupper "df_pct".
#x uafhængig variabel = DST’s forbrugertillidsindikator (forbrug_kvartal2$Forbrugertillidsindikatoren).

dst1 <- lm(df_pct$Fødevarer.mv.~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst1)

dst2 <- lm(df_pct$Drikkevarer.og.tobak.mv.~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst2)

dst3 <- lm(df_pct$Beklædning.og.fodtøj~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst3)

dst4 <- lm(df_pct$Boligbenyttelse~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst4)

dst5 <- lm(df_pct$Elektricitet..fjernvarme.og.andet.brændsel~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst5)

dst6 <- lm(df_pct$Boligudstyr..husholdningstjenester.mv.~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst6)

dst7 <- lm(df_pct$Medicin..lægeudgifter.o.l.~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst7)

dst8 <- lm(df_pct$Køb.af.køretøjer~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst8)

dst9 <- lm(df_pct$Drift.af.køretøjer.og.transporttjenester~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst9)

dst10 <- lm(df_pct$Information.og.kommunikation~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst10)

dst11 <- lm(df_pct$Fritid..sport.og.kultur~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst11)

dst12 <- lm(df_pct$Undervisning~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst12)

dst13 <- lm(df_pct$Restauranter.og.hoteller~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst13)

dst14 <- lm(df_pct$Forsikring.og.finansielle.tjenester~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst14)

dst15 <- lm(df_pct$Andre.varer.og.tjenester~forbrug_kvartal2$Forbrugertillidsindikatoren)
summary(dst15)
















