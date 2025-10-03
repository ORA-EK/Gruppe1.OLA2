library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#opgave 4.3

# Vi indlæser Excel-filen ind og sikrer
#at første kolonne hedder 'År'.

df <- read_excel("forbu2.xlsx", sheet = "Ark1")
names(df)[1] <- "År"

# 2) Long-format
df_long <- df %>%
  pivot_longer(-År, names_to = "Gruppe", values_to = "Forbrug")


#Først bruges filter(År == 2024) til at vælge kun de rækker, hvor året er 2024. 
#Derefter anvendes arrange(desc(Forbrug)), som sorterer grupperne efter størst forbrug,
#så den højeste værdi kommer øverst. Til sidst bruges slice(1),
#der tager den første række i det sorterede datasæt. 
#Resultatet er, at vi finder den forbrugsgruppe, som danskerne brugte flest penge på i 2024.

størst2024 <- df_long %>%
  filter(År == 2024) %>%
  arrange(desc(Forbrug)) %>%
  slice(1)

#Vi definere start og slutåret som variabler
årstart <- 2020
årslut  <- 2024

ændring_pct <- df_long %>%
  filter(År %in% c(årstart, årslut)) %>% #Her vælger vi kun de rækker fra datasættet, hvor året er startåret 2020 eller slutåret 2024.
  pivot_wider(names_from = År, values_from = Forbrug) %>% #Vi omdanner datasættet fra langt format hvor hvert år er en række til bredt format, så man får en kolonne med forbrug i startåret og en kolonne med forbrug i slutåret.
  mutate( #Her laver vi ny kolonne forskel, der viser den absolutte ændring i forbrug:
    forskel = .data[[as.character(årslut)]] - .data[[as.character(årstart)]], 
    forskel_pct = (forskel / .data[[as.character(årstart)]]) * 100 ##Her beregner vi den procentvise ændring i forbruget i forhold til startåret.
  ) %>%
  arrange(desc(forskel)) #Til sidst sorterer vi tabellen, så de forbrugsgrupper med den største absolutte stigning kommer øverst.


# 6) GRAF: Ændring 2020 → 2024 i procent med tal på søjlerne
ggplot(ændring_pct, aes(x = reorder(Gruppe, forskel_pct), y = forskel_pct)) +
  geom_col(fill = "limegreen") +
  coord_flip() +
  geom_text(
    aes(label = paste0(format(round(forskel_pct, 1), big.mark = ".", decimal.mark = ","), "%")),
    hjust = ifelse(ændring_pct$forskel_pct >= 0, -0.1, 1.1),
    size = 4
  ) +
  labs(
    title = "Ændring i husholdningernes forbrug (i %)",
    subtitle = paste("Fra", årstart, "til", årslut),
    x = "Forbrugsgruppe",
    y = "Ændring i pct."
  ) +
  scale_y_continuous(labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), "%")) +
  expand_limits(y = c(min(ændring_pct$forskel_pct) * 1.15,
                      max(ændring_pct$forskel_pct) * 1.15)) +
  theme_minimal(base_size = 13)

