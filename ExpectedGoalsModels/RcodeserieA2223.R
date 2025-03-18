# Installa e carica le librerie necessarie
install.packages(c("worldfootballR", "dplyr"))
library(worldfootballR)
library(dplyr)

# Lista per memorizzare i dati di tutti i match
all_matches_list <- list()

# Costruisci la lista di URL dei match
start_match_id <- 18582
end_match_id <- 18961

match_urls <- sprintf("https://understat.com/match/%d", start_match_id:end_match_id)

# Iterazione su ogni URL del match
for (url in match_urls) {
  match_data <- understat_match_shots(match_url = url)
  match_data$name <- url  # eventualmente un nome piu descrittivo
  all_matches_list[[url]] <- match_data
}

# Unisci i dati di tutti i match in un unico dataframe
all_matches_df <- bind_rows(all_matches_list)

# Calcola la distanza dalla porta utilizzando la nuova formula
all_matches_df$distanza_porta <- sqrt((abs(all_matches_df$X - 1) * 105)^2 + (abs(all_matches_df$Y - 0.5) * 65)^2)

# Aggiungi la variabile aggregata per lastAction
frequenza_lastAction <- table(all_matches_df$lastAction)
categorie_selezionate <- names(frequenza_lastAction[frequenza_lastAction >= 160])
all_matches_df <- all_matches_df %>% filter(lastAction %in% categorie_selezionate)
all_matches_df$lastAction_aggregated <- ifelse(all_matches_df$lastAction == "Standard", "Standard", "Altro")

# Crea la variabile di risposta goal
all_matches_df$goal <- 0
all_matches_df$goal[all_matches_df$result == "Goal"] <- 1

# Crea il modello completo con tutte le esplicative
modello_completo <- glm(goal ~ home_away + situation + shotType + lastAction_aggregated + distanza_porta + home_team + away_team, 
                        family = "binomial", data = all_matches_df)

# Applica la stepwise selection al modello completo
modello_selezionato <- step(modello_completo, direction = "both")

# Visualizza il modello completo
summary(modello_completo)

# Visualizza il modello selezionato
summary(modello_selezionato)



# Tabella di conteggio per la variabile goal
table(all_matches_df$goal)
