#Reste à faire : 
# fichier rmarkdown


# ===========================================================================
# === CHAPITRE 1 : Lire fichier =============================================
# ===========================================================================

library(readxl)
library(ggplot2)
library(forcats)
library(magick)

dfbase <- readxl::read_excel("TEST Finale PlayOff Etoile Lavaloise vs Sporting Paris.xlsx")
couleurs_data <- read_excel("Logos + couleurs.xltm")

# ===========================================================================
# === CHAPITRE 2 : Décaler colonnes 2 3 4 d'1 plus haut =====================
# ===========================================================================

df1 <- dfbase


# Remplace le nom de la 2 colonne par un nouveau nom
names(df1)[2] <- "Temps départ"

# Remplace le nom de la 3 colonne par un nouveau nom
names(df1)[3] <- "Possession"

# Remplace le nom de la 4 colonne par un nouveau nom
names(df1)[4] <- "Temps match"

# Supprime les valeurs de la ligne 1 dans les colonnes 2, 3 et 4
df1[1, c("Temps départ", "Possession", "Temps match")] <- NA

# ===========================================================================
# === CHAPITRE 3 : Convertir données en format hh:mm:ss =====================
# ===========================================================================

# Convertit la colonne 4 en format numérique
df1$`Temps match` <- as.numeric(df1$`Temps match`)

# Convertit les valeurs de la colonne 4 au format POSIXlt
df1$`Temps match` <- as.POSIXlt(df1$`Temps match` * 86400, origin = "1970-01-01", tz = "UTC")

# Formatte les valeurs pour afficher seulement l'heure
df1$`Temps match` <- format(df1$`Temps match`, format = "%H:%M:%S")


library(hms)

# Convertit les colonnes 7 à la dernière en format hms
df1[, 7:ncol(df1)] <- lapply(df1[, 7:ncol(df1)], function(x) hms::as_hms(x))

# Supprimer la colonne 2
df1 <- subset(df1, select = -c(2))

# ===========================================================================
# === CHAPITRE 4 : Déclaration des variables ================================
# ===========================================================================


#Déclarer variables
EquipeDomicile <- df1[3, 1]
EquipeExterieur <- df1[5, 1]
Nom_EquipeDomicile <- as.character(EquipeDomicile[1, 1])
Nom_EquipeExterieur <- as.character(EquipeExterieur[1, 1])

#=====================================================================================
#COULEURS de chaque équipe
#=====================================================================================

#DETECTION COULEUR DE CHAQUE EQUIPE
# Récupérer les couleurs des équipes domicile et extérieur
Couleur_Equipe_DOM <- couleurs_data$HEXcouleurs[couleurs_data$Equipes == Nom_EquipeDomicile]
Couleur_Equipe_EXT <- couleurs_data$HEXcouleurs[couleurs_data$Equipes == Nom_EquipeExterieur]

#SI EQUIPE NON PRESENTE DANS LE FICHIER, RENVOYER DES COULEURS PAR DEFAUT
# Vérifier si les équipes domicile et extérieur sont présentes dans les données de couleur
if (length(Couleur_Equipe_DOM) == 0 & length(Couleur_Equipe_EXT) == 0) {
  Couleur_Equipe_DOM <- "#FF0000"  # Définir la couleur par défaut si Nom_EquipeDomicile n'est pas trouvé
  Couleur_Equipe_EXT <- "#0000FF"  # Définir la couleur par défaut si Nom_EquipeExterieur n'est pas trouvé
} else if (length(Couleur_Equipe_DOM) == 0) {
  Couleur_Equipe_DOM <- "#FF0000"  # Définir la couleur par défaut si Nom_EquipeDomicile n'est pas trouvé
} else if (length(Couleur_Equipe_EXT) == 0) {
  Couleur_Equipe_EXT <- "#0000FF"  # Définir la couleur par défaut si Nom_EquipeExterieur n'est pas trouvé
}

#OBSERVER LA LUMINOSITE DES COULEURS POUR ECRIRE EN CLAIR OU EN FONCE
# Récupérer les luminosités des couleurs des équipes domicile et extérieur
Luminosite_Equipe_DOM <- couleurs_data$Luminosité[couleurs_data$Equipes == Nom_EquipeDomicile]
Luminosite_Equipe_EXT <- couleurs_data$Luminosité[couleurs_data$Equipes == Nom_EquipeExterieur]

# Définir les couleurs du texte en fonction de la luminosité des couleurs des équipes
if (length(Luminosite_Equipe_DOM) == 0) {
  Luminosite_Equipe_DOM <- "Bright"  # Supposer que la luminosité est "Bright" si elle n'est pas définie
}
if (length(Luminosite_Equipe_EXT) == 0) {
  Luminosite_Equipe_EXT <- "Bright"  # Supposer que la luminosité est "Bright" si elle n'est pas définie
}

# Définir les couleurs du texte en fonction de la luminosité des couleurs des équipes
if ((Luminosite_Equipe_DOM == "Bright" & Luminosite_Equipe_EXT == "Bright") |
    (Luminosite_Equipe_EXT == "Bright" & Luminosite_Equipe_DOM == "Bright")) {
  couleur_texte <- "black"
} else if ((Luminosite_Equipe_DOM == "Bright" & Luminosite_Equipe_EXT == "Dark") |
           (Luminosite_Equipe_EXT == "Dark" & Luminosite_Equipe_DOM == "Bright")) {
  couleur_texte <- "white"
} else if ((Luminosite_Equipe_DOM == "Dark" & Luminosite_Equipe_EXT == "Dark") |
           (Luminosite_Equipe_EXT == "Dark" & Luminosite_Equipe_DOM == "Dark")) {
  couleur_texte <- "white"
}



# ===========================================================================
# === CHAPITRE 5 : Traitements ==============================================
# ===========================================================================


# === Sous chapitre 1 : Scores ==============================================

# Initialise scores ScoreEquipeDOM et ScoreEquipeEXT à zéro
ScoreEquipeDOM <- 0
ScoreEquipeEXT <- 0

# Parcours chaque en tête des colonnes du dataframe df1
for (col_name in colnames(df1)) {
  
  # Vérifie si le nom de chaque en tête de colonne commence par "Buteur" ou "TirAmenantCSC"
  if (startsWith(col_name, "Buteur") || startsWith(col_name, "TirAmenantCSC")) {
    
    # Vérifie si la colonne contient au moins une valeur non manquante
    if (any(!is.na(df1[[col_name]]))) {
      
      # Trouve les indices des lignes non vides dans la colonne actuelle
      non_na_indices <- which(!is.na(df1[[col_name]]))
      
      # Si oui, trouve la dernière ligne non vide
      lastRow <- max(non_na_indices)
      
      # Boucle pour parcourir les lignes de chaque colonne à partir de la première ligne
      for (row_index in 1:lastRow) {
        # Compare club du buteur avec EquipeDomicile ou EquipeExterieur
        if (!is.na(df1[row_index, col_name]) && !is.na(df1[row_index, "Club"])) {
          if (df1[row_index, "Club"] == EquipeDomicile) {
            # Si but EquipeDomicile = +1 au score de l'EquipeDomicile
            ScoreEquipeDOM <- ScoreEquipeDOM + 1
          } else if (df1[row_index, "Club"] == EquipeExterieur) {
            # Si but EquipeDomicile = +1 au score de l'EquipeExterieur
            ScoreEquipeEXT <- ScoreEquipeEXT + 1
          }
        }
      }
    }
  }
}


# === Sous chapitre 2 : Recherches simples ==============================================

# Définir une fonction pour calculer les scores en fonction du mot de recherche
calculer_scores1 <- function(df1, equipe_dom, equipe_ext, mot_recherche) {
  # Initialise les scores à zéro
  score_dom <- 0
  score_ext <- 0
  
  # Parcours chaque en-tête des colonnes du dataframe df1
  for (col_name in colnames(df1)) {
    # Vérifie si le nom de chaque en-tête de colonne commence par le mot de recherche
    if (startsWith(col_name, mot_recherche)) {
      # Si occurrence trouvée, trouve dernière ligne non vide de la colonne correspondante
      lastRow <- max(which(!is.na(df1[[col_name]])))
      
      # Vérifie si la colonne observée contient des données
      if (!is.null(lastRow)) {
        # Boucle pour parcourir les lignes de chaque colonne à partir de la première ligne
        for (row_index in 1:lastRow) {
          # Compare le club avec EquipeDomicile ou EquipeExterieur
          if (!is.na(df1[row_index, col_name]) && !is.na(df1[row_index, "Club"])) {
            if (df1[row_index, "Club"] == equipe_dom) {
              # Si le club correspond à EquipeDomicile, incrémente le score de domicile
              score_dom <- score_dom + 1
            } else if (df1[row_index, "Club"] == equipe_ext) {
              # Si le club correspond à EquipeExterieur, incrémente le score de l'extérieur
              score_ext <- score_ext + 1
            }
          }
        }
      }
    }
  }
  
  
  
  # Retourne les scores calculés
  return(c(score_dom, score_ext))
}

# Liste des mots de recherche
mots_recherches <- c(
  "Arrêt", 
  "Carton J", 
  "Carton R", 
  "Dribble raté", 
  "Dribble réussi", 
  "Faute C concédée",
  "Faute NC concédée",
  "Tir contré",
  "Tir sur montant",
  "Tir non cadré",
  "Dégagement",
  "Intercepté",
  "Perte dans les pieds",
  "Interception",
  "Récupération dans les pieds",
  "Relance courte réussie",
  "Relance moyenne réussie",
  "Relance longue réussie",
  "Relance courte raté",
  "Relance moyenne raté",
  "Relance longue raté",
  "Buteur Trans",
  "Buteur att placée",
  "Buteur CPA Corner",
  "Buteur CPA Touche",
  "Buteur CPA Coup franc direct",
  "Buteur CPA Coup franc indirect",
  "Buteur MontéeGB",
  "Buteur PowerPlay",
  "Buteur ContrePowerPlay",
  "TirAmenantCSC",
  "Buteur 4c3",
  "Buteur CPA 10m",
  "Buteur CPA Penalty")

# Parcours pour chaque mot de recherche
for (i in 1:length(mots_recherches)) {
  # Initialise les noms des variables pour les scores
  nom_variable_dom <- paste("NB", gsub(" ", "_", mots_recherches[i]), "DOM", sep = "_")
  nom_variable_ext <- paste("NB", gsub(" ", "_", mots_recherches[i]), "EXT", sep = "_")
  
  # Calculer les scores pour le mot de recherche actuel
  scores <- calculer_scores1(df1, EquipeDomicile, EquipeExterieur, mots_recherches[i])
  
  # Assigner les scores calculés aux variables correspondantes
  assign(nom_variable_dom, scores[1])
  assign(nom_variable_ext, scores[2])
}


# === Sous chapitre 3 : Recherches doubles ==============================================

# Définir une fonction pour calculer les scores en fonction des deux mots de recherche
calculer_scores2 <- function(df1, equipe_dom, equipe_ext, mot_recherche1, mot_recherche2) {
  # Initialise les scores à zéro
  score_dom <- 0
  score_ext <- 0
  
  # Parcours chaque en tête des colonnes du dataframe df1
  for (col_name in colnames(df1)) {
    # Vérifie si le nom de chaque en tête de colonne contient les deux mots de recherche
    if (grepl(paste(mot_recherche1, mot_recherche2, sep = "|"), col_name)) {
      # Vérifie si la colonne contient des données non manquantes
      if (any(!is.na(df1[[col_name]]))) {
        # Si occurrence trouvée, trouve dernière ligne non vide de la colonne correspondante
        lastRow <- max(which(!is.na(df1[[col_name]])))
        
        # Boucle pour parcourir les lignes de chaque colonne à partir de la première ligne
        for (row_index in 1:lastRow) {
          # Compare le club avec EquipeDomicile ou EquipeExterieur
          if (!is.na(df1[row_index, col_name]) && !is.na(df1[row_index, "Club"])) {
            if (df1[row_index, "Club"] == equipe_dom) {
              # Si le club correspond à EquipeDomicile, incrémente le score de domicile
              score_dom <- score_dom + 1
            } else if (df1[row_index, "Club"] == equipe_ext) {
              # Si le club correspond à EquipeExterieur, incrémente le score de l'extérieur
              score_ext <- score_ext + 1
            }
          }
        }
      }
    }
  }
  
  # Retourne les scores calculés
  return(c(score_dom, score_ext))
}

# Liste des paires de mots de recherche
paires_mots_recherches <- list(
  c("Récupération", "Interception"),
  c("Tir", "Buteur"),
  c("Tir cadré", "Buteur"),
  c("Entrée", "Entrée Solo")
)

# Parcours pour chaque paire de mots de recherche
for (pair in paires_mots_recherches) {
  # Concatène les mots de la paire pour former un nom unique
  nom_recherche <- paste(pair, collapse = "_")
  
  # Initialise les noms des variables pour les scores
  nom_variable_dom <- paste("NB", gsub(" ", "_", nom_recherche), "DOM", sep = "_")
  nom_variable_ext <- paste("NB", gsub(" ", "_", nom_recherche), "EXT", sep = "_")
  
  # Calculer les scores pour la paire de mots de recherche actuelle
  scores <- calculer_scores2(df1, EquipeDomicile, EquipeExterieur, pair[1], pair[2])
  
  # Assigner les scores calculés aux variables correspondantes
  assign(nom_variable_dom, scores[1])
  assign(nom_variable_ext, scores[2])
}

#Changer les noms des variables doubles
renommer_variable <- function(ancien_nom, nouveau_nom) {
  assign(nouveau_nom, get(ancien_nom), envir = .GlobalEnv)
  rm(list = ancien_nom, envir = .GlobalEnv)
}

#Renommer variables
#1
renommer_variable("NB_Récupération_Interception_DOM", "NB_Ballon_Gagné_DOM")

#2
renommer_variable("NB_Récupération_Interception_EXT", "NB_Ballon_Gagné_EXT")

#3
renommer_variable("NB_Tir_Buteur_DOM", "NB_Tir_DOM")

#4
renommer_variable("NB_Tir_Buteur_EXT", "NB_Tir_EXT")

#5
renommer_variable("NB_Tir_cadré_Buteur_DOM", "NB_Tir_Cadré_DOM")

#6
renommer_variable("NB_Tir_cadré_Buteur_EXT", "NB_Tir_Cadré_EXT")

#7
renommer_variable("NB_Entrée_Entrée_Solo_DOM", "NB_Changements_DOM")

#8
renommer_variable("NB_Entrée_Entrée_Solo_EXT", "NB_Changements_EXT")


# === Sous chapitre 4 : Recherches triples ==============================================

# Définir une fonction pour calculer les scores en fonction des trois mots de recherche
calculer_scores_triples <- function(df1, equipe_dom, equipe_ext, mot_recherche1, mot_recherche2, mot_recherche3) {
  # Initialise les scores à zéro
  score_dom <- 0
  score_ext <- 0
  
  # Parcours chaque en-tête des colonnes du dataframe df1
  for (col_name in colnames(df1)) {
    # Vérifie si le nom de chaque en-tête de colonne contient les trois mots de recherche
    if (grepl(paste(mot_recherche1, mot_recherche2, mot_recherche3, sep = "|"), col_name)) {
      # Vérifie si la colonne contient des données non manquantes
      if (any(!is.na(df1[[col_name]]))) {
        # Si occurrence trouvée, trouve dernière ligne non vide de la colonne correspondante
        lastRow <- max(which(!is.na(df1[[col_name]])))
        
        # Boucle pour parcourir les lignes de chaque colonne à partir de la première ligne
        for (row_index in 1:lastRow) {
          # Compare le club avec EquipeDomicile ou EquipeExterieur
          if (!is.na(df1[row_index, col_name]) && !is.na(df1[row_index, "Club"])) {
            if (df1[row_index, "Club"] == equipe_dom) {
              # Si le club correspond à EquipeDomicile, incrémente le score de domicile
              score_dom <- score_dom + 1
            } else if (df1[row_index, "Club"] == equipe_ext) {
              # Si le club correspond à EquipeExterieur, incrémente le score de l'extérieur
              score_ext <- score_ext + 1
            }
          }
        }
      }
    }
  }
  
  # Retourne les scores calculés
  return(c(score_dom, score_ext))
}

# Liste des triplets de mots de recherche
triplets_mots_recherches <- list(
  c("Dégagement", "Intercepté", "Perte dans les pieds")
)

# Parcours pour chaque triplet de mots de recherche
for (triplet in triplets_mots_recherches) {
  # Concatène les mots du triplet pour former un nom unique
  nom_recherche <- paste(triplet, collapse = "_")
  
  # Initialise les noms des variables pour les scores
  nom_variable_dom <- paste("NB", gsub(" ", "_", nom_recherche), "DOM", sep = "_")
  nom_variable_ext <- paste("NB", gsub(" ", "_", nom_recherche), "EXT", sep = "_")
  
  # Calculer les scores pour le triplet de mots de recherche actuel
  scores <- calculer_scores_triples(df1, EquipeDomicile, EquipeExterieur, triplet[1], triplet[2], triplet[3])
  
  # Assigner les scores calculés aux variables correspondantes
  assign(nom_variable_dom, scores[1])
  assign(nom_variable_ext, scores[2])
}

#Renommer variables
#1
renommer_variable("NB_Dégagement_Intercepté_Perte_dans_les_pieds_DOM", "NB_Ballon_Perdu_DOM")

#2
renommer_variable("NB_Dégagement_Intercepté_Perte_dans_les_pieds_EXT", "NB_Ballon_Perdu_EXT")


# === Sous chapitre 6 : Possession ==============================================

df2 <- df1

# Convertir les valeurs de la colonne 3 (Temps match) en secondes
df2$`Temps match` <- as.character(df2$`Temps match`)
temps <- strsplit(df2$`Temps match`, ":")
df2$`Temps match` <- sapply(temps, function(x) as.numeric(x[1]) * 3600 + as.numeric(x[2]) * 60 + as.numeric(x[3]))

# Trouver la dernière ligne avec des données dans la colonne 2 (Equipe) du dataframe df2
lastRow <- max(which(!is.na(df2[, 2])))

# Initialiser les totaux
totalBleu <- 0
totalRouge <- 0

# Parcourir les lignes et additionner les valeurs de la colonne 3 (Temps match) 
# en fonction de la valeur de la colonne 2 (Equipe)
for (i in 3:lastRow) {
  if (df2[i, 2] == "B") {
    totalBleu <- totalBleu + df2[i, 3]
  } else if (df2[i, 2] == "R") {
    totalRouge <- totalRouge + df2[i, 3]
  }
}

# Calculer les pourcentages
if (totalBleu + totalRouge != 0) {
  pourcentageBleu <- totalBleu / (totalBleu + totalRouge)
  pourcentageRouge <- totalRouge / (totalBleu + totalRouge)
} else {
  pourcentageBleu <- 0
  pourcentageRouge <- 0
}

# Arrondir les pourcentages et les stocker dans les variables PossessionEXT et PossessionDOM
PossessionEXT <- round(pourcentageBleu * 100)
PossessionDOM <- round(pourcentageRouge * 100)


# === Sous chapitre 7 : Temps de jeu ==============================================

df3 <- df1

#Entrée

# Fonction pour convertir une valeur hh:mm:ss en secondes
convert_to_seconds_entrée <- function(time_string) {
  time_components <- as.numeric(unlist(strsplit(time_string, ":")))
  seconds <- sum(c(3600, 60, 1) * time_components, na.rm = TRUE)
  return(seconds)
}

# Initialiser une variable pour stocker les temps d'entrée
TempsEntrée <- list()

# Initialiser la variable LigneJoueurEntrée
LigneJoueurEntrée <- 1

# Tant que LigneJoueurEntrée est inférieur ou égal au nombre total de lignes dans la colonne 5 du dataframe
while (LigneJoueurEntrée <= nrow(df3) && !is.na(df3[LigneJoueurEntrée, 5])) {
  # Initialiser la variable TempsJoueurEntrée
  TempsJoueurEntrée <- 0
  
  # Parcourir toutes les en-têtes de colonne à partir de la colonne 6
  for (col_index in 6:ncol(df3)) {
    # Si l'en-tête commence par "Entrée" ou "Entrée Solo" et que la valeur de LigneJoueurEntrée dans cette colonne n'est pas NA
    if (grepl("^Entrée", names(df3)[col_index]) && !is.na(df3[LigneJoueurEntrée, col_index])) {
      # Convertir la valeur en chaîne de caractères
      value_str <- as.character(df3[LigneJoueurEntrée, col_index])
      # Ajouter la valeur à TempsJoueurEntrée
      TempsJoueurEntrée <- TempsJoueurEntrée + as.numeric(value_str)
    }
  }
  
  # Stocker le temps d'entrée dans la liste TempsEntrée avec une clé unique
  TempsEntrée[[paste0("TempsEntrée", df3[LigneJoueurEntrée, 5])]] <- TempsJoueurEntrée
  
  # Passer à la ligne suivante
  LigneJoueurEntrée <- LigneJoueurEntrée + 1
}


#Sortie

# Fonction pour convertir une valeur hh:mm:ss en secondes
convert_to_seconds_sortie <- function(time_string) {
  time_components <- as.numeric(unlist(strsplit(time_string, ":")))
  seconds <- sum(c(3600, 60, 1) * time_components, na.rm = TRUE)
  return(seconds)
}

# Initialiser une variable pour stocker les temps de sortie
TempsSortie <- list()

# Initialiser la variable LigneJoueurSortie
LigneJoueurSortie <- 1

# Tant que LigneJoueurSortie est inférieur ou égal au nombre total de lignes dans la colonne 5 du dataframe
while (LigneJoueurSortie <= nrow(df3) && !is.na(df3[LigneJoueurSortie, 5])) {
  # Initialiser la variable TempsJoueurSortie
  TempsJoueurSortie <- 0
  
  # Parcourir toutes les en-têtes de colonne à partir de la colonne 6
  for (col_index in 6:ncol(df3)) {
    # Si l'en-tête commence par "Sortie" ou "Sortie Solo" et que la valeur de LigneJoueurSortie dans cette colonne n'est pas NA
    if (grepl("^Sortie", names(df3)[col_index]) && !is.na(df3[LigneJoueurSortie, col_index])) {
      # Convertir la valeur en chaîne de caractères
      value_str <- as.character(df3[LigneJoueurSortie, col_index])
      # Ajouter la valeur à TempsJoueurSortie
      TempsJoueurSortie <- TempsJoueurSortie + as.numeric(value_str)
    }
  }
  
  # Stocker le temps de sortie dans la liste TempsSortie avec une clé unique
  TempsSortie[[paste0("TempsSortie", df3[LigneJoueurSortie, 5])]] <- TempsJoueurSortie
  
  # Passer à la ligne suivante
  LigneJoueurSortie <- LigneJoueurSortie + 1
}


# Fonction pour convertir les secondes en format mm:ss
seconds_to_mms <- function(seconds) {
  minutes <- floor(seconds / 60)
  seconds <- seconds %% 60
  return(sprintf("%02d:%02d", minutes, seconds))
}

# Initialiser une liste pour stocker les temps de jeu de chaque joueur
TempsJeu <- list()

# Boucle sur les lignes du dataframe
for (i in 1:nrow(df3)) {
  # Récupérer le nom du joueur à partir de la colonne 5 du dataframe
  nom_joueur <- df3[i, 5]
  
  # Récupérer la valeur de la colonne 4 (nom de l'équipe du joueur)
  equipe_joueur <- df3[i, 4]
  
  # Construire les clés uniques pour accéder aux temps d'entrée et de sortie
  clef_entree <- paste0("TempsEntrée", nom_joueur)
  clef_sortie <- paste0("TempsSortie", nom_joueur)
  
  # Vérifier si les clés existent dans les listes
  if (clef_entree %in% names(TempsEntrée) && clef_sortie %in% names(TempsSortie)) {
    # Calculer le temps de jeu en soustrayant le temps de sortie du temps d'entrée
    temps_jeu <- TempsSortie[[clef_sortie]] - TempsEntrée[[clef_entree]]
    
    # Convertir le temps de jeu en format mm:ss
    temps_jeu_mms <- seconds_to_mms(temps_jeu)
    
    # Stocker le temps de jeu dans la liste TempsJeu avec une clé unique incluant le nom de l'équipe du joueur
    if (equipe_joueur == EquipeDomicile) {
      TempsJeu[[paste0("TempsJeu_", EquipeDomicile, "_", nom_joueur)]] <- temps_jeu_mms
    } else {
      TempsJeu[[paste0("TempsJeu_", EquipeExterieur, "_", nom_joueur)]] <- temps_jeu_mms
    }
  }
}

# === Sous chapitre 8 : Stocker noms joueurs par équipe ==============================================

# Noms joueurs domicile
Nom_joueurs_DOM <- character()

# Parcourir les lignes du DataFrame
for (i in 1:nrow(df1)) {
  # Vérifier si le nom du joueur n'est pas manquant
  if (!is.na(df1[i, 5])) {
    # Vérifier si le club de la ligne correspond à EquipeDomicile
    if (df1[i, 4] == EquipeDomicile) {
      # Ajouter le nom du joueur au vecteur
      Nom_joueurs_DOM <- c(Nom_joueurs_DOM, df1[i, 5])
    }
  }
}

# Noms joueurs extérieur
Nom_joueurs_EXT <- character()

# Parcourir les lignes du DataFrame
for (i in 1:nrow(df1)) {
  # Vérifier si le nom du joueur n'est pas manquant
  if (!is.na(df1[i, 5])) {
    # Vérifier si le club de la ligne correspond à EquipeExterieur
    if (df1[i, 4] == EquipeExterieur) {
      # Ajouter le nom du joueur au vecteur
      Nom_joueurs_EXT <- c(Nom_joueurs_EXT, df1[i, 5])
    }
  }
}

#Tronquer noms
# Fonction pour tronquer les noms qui dépassent une certaine longueur
tronquer_nom <- function(nom, longueur_max) {
  if (nchar(nom) > longueur_max) {
    return(substr(nom, 1, longueur_max))
  } else {
    return(nom)
  }
}

#Noms pour la liste déroulante
# Assigner les noms des joueurs domicile aux variables correspondantes
for (i in 1:min(length(Nom_joueurs_DOM), 12)) {
  assign(paste0("Nom_DOM_", i), Nom_joueurs_DOM[i])
}

# Assigner les noms des joueurs extérieur aux variables correspondantes
for (i in 1:min(length(Nom_joueurs_EXT), 12)) {
  assign(paste0("Nom_EXT_", i), Nom_joueurs_EXT[i])
}

# === Sous chapitre 9 : Additions de variables ==============================================

#Relances réussies
NB_Relance_réussie_DOM <- NB_Relance_courte_réussie_DOM + NB_Relance_moyenne_réussie_DOM + NB_Relance_longue_réussie_DOM
NB_Relance_réussie_EXT <- NB_Relance_courte_réussie_EXT + NB_Relance_moyenne_réussie_EXT + NB_Relance_longue_réussie_EXT

#Relances ratés
NB_Relance_raté_DOM <- NB_Relance_courte_raté_DOM + NB_Relance_moyenne_raté_DOM + NB_Relance_longue_raté_DOM
NB_Relance_raté_EXT <- NB_Relance_courte_raté_EXT + NB_Relance_moyenne_raté_EXT + NB_Relance_longue_raté_EXT

#Possession
NB_Possession_DOM <- as.numeric(PossessionDOM)
NB_Possession_EXT <- as.numeric(PossessionEXT)


# === Sous chapitre 10 : Traitements pour tableau stats individuelles ==============================================


#Stats individuelles simples

# Définir une fonction pour calculer les scores en fonction du mot de recherche
calculer_scores_indiv <- function(df1, joueurs_dom, joueurs_ext, mots_recherche) {
  # Initialise les scores à zéro
  scores_dom <- rep(0, length(joueurs_dom))
  scores_ext <- rep(0, length(joueurs_ext))
  
  # Parcours chaque en-tête des colonnes du dataframe df1
  for (col_name in colnames(df1)) {
    # Vérifie si le nom de chaque en-tête de colonne commence par le mot de recherche
    if (startsWith(col_name, mots_recherche)) {
      # Si occurrence trouvée, trouve dernière ligne non vide de la colonne correspondante
      lastRow <- max(which(!is.na(df1[[col_name]])))
      
      # Vérifie si la colonne observée contient des données
      if (!is.null(lastRow)) {
        # Boucle pour parcourir les lignes de chaque colonne à partir de la première ligne
        for (row_index in 1:lastRow) {
          # Compare le Nom avec les noms des joueurs DOM
          if (!is.na(df1[row_index, col_name]) && !is.na(df1[row_index, "Nom"])) {
            for (i in 1:length(joueurs_dom)) {
              if (df1[row_index, "Nom"] == joueurs_dom[i]) {
                scores_dom[i] <- scores_dom[i] + 1
              }
            }
          }
          
          # Compare le Nom avec les noms des joueurs EXT
          if (!is.na(df1[row_index, col_name]) && !is.na(df1[row_index, "Nom"])) {
            for (i in 1:length(joueurs_ext)) {
              if (df1[row_index, "Nom"] == joueurs_ext[i]) {
                scores_ext[i] <- scores_ext[i] + 1
              }
            }
          }
        }
      }
    }
  }
  
  # Retourne les scores calculés
  return(list(DOM = scores_dom, EXT = scores_ext))
}

# Liste des mots de recherche
mots_recherches <- c(
  "Arrêt", 
  "Carton J", 
  "Carton R", 
  "Dribble raté", 
  "Dribble réussi", 
  "Faute C concédée",
  "Faute C subie",
  "Faute NC concédée",
  "Faute NC subie",
  "Tir",
  "Tir sur montant",
  "Tir cadré",
  "Tir contré",
  "Tir sur montant",
  "Tir non cadré",
  "Dégagement",
  "Intercepté",
  "Perte dans les pieds",
  "Interception",
  "Récupération dans les pieds",
  "Relance courte réussie",
  "Relance moyenne réussie",
  "Relance longue réussie",
  "Relance courte raté",
  "Relance moyenne raté",
  "Relance longue raté",
  "Buteur",
  "Buteur Trans",
  "Buteur att placée",
  "Buteur CPA Corner",
  "Buteur CPA Touche",
  "Buteur CPA Coup franc direct",
  "Buteur CPA Coup franc indirect",
  "Buteur MontéeGB",
  "Buteur PowerPlay",
  "Buteur ContrePowerPlay",
  "TirAmenantCSC",
  "Buteur 4c3",
  "Buteur CPA 10m",
  "Buteur CPA Penalty",
  "Enrée",
  "Entrée Solo",
  "Sortie",
  "Sortie Solo", 
  "Passeur D",
  "Passeur D att placée",
  "Passeur D Trans",
  "Passeur D 4c3",
  "Passeur D PowerPlay",
  "Passeur D ContrePowerPlay",
  "Passeur D MontéeGB",
  "Passeur D CPA",
  "Passeur D CPA Corner", 
  "Passeur D CPA Touche",
  "Passeur D CPA Coup franc direct",
  "Passeur D CPA Coup franc indirect",
  "Présent But concédé",
  "Présent But marqué")

# Parcours pour chaque mot de recherche
for (mot_recherche in mots_recherches) {
  scores <- calculer_scores_indiv(df1, 
                                  c(Nom_DOM_1, Nom_DOM_2, Nom_DOM_3, Nom_DOM_4, Nom_DOM_5, Nom_DOM_6, Nom_DOM_7, Nom_DOM_8, Nom_DOM_9, Nom_DOM_10, Nom_DOM_11, Nom_DOM_12), 
                                  c(Nom_EXT_1, Nom_EXT_2, Nom_EXT_3, Nom_EXT_4, Nom_EXT_5, Nom_EXT_6, Nom_EXT_7, Nom_EXT_8, Nom_EXT_9, Nom_EXT_10, Nom_EXT_11, Nom_EXT_12), 
                                  mot_recherche)
  
  for (i in 1:length(scores$DOM)) {
    assign(paste("Indiv_NB", gsub(" ", "_", mot_recherche), paste("Nom_DOM", i, sep = "_"), sep = "_"), scores$DOM[i])
  }
  
  for (i in 1:length(scores$EXT)) {
    assign(paste("Indiv_NB", gsub(" ", "_", mot_recherche), paste("Nom_EXT", i, sep = "_"), sep = "_"), scores$EXT[i])
  }
}

#Addition de variable pour en former des nouvelles

#Ballon perdu
#DOM
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables <- length(ls(pattern = "^Indiv_NB_Dégagement_Nom_DOM_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Ballon_perdu_Nom_DOM_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Dégagement_Nom_DOM_", x)) +
    get(paste0("Indiv_NB_Intercepté_Nom_DOM_", x)) +
    get(paste0("Indiv_NB_Perte_dans_les_pieds_Nom_DOM_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}

#EXT
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables2 <- length(ls(pattern = "^Indiv_NB_Dégagement_Nom_DOM_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables2) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Ballon_perdu_Nom_EXT_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Dégagement_Nom_EXT_", x)) +
    get(paste0("Indiv_NB_Intercepté_Nom_EXT_", x)) +
    get(paste0("Indiv_NB_Perte_dans_les_pieds_Nom_EXT_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}


#Ballon gagné
#DOM
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables <- length(ls(pattern = "^Indiv_NB_Récupération_dans_les_pieds_Nom_DOM_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Ballon_gagné_Nom_DOM_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Récupération_dans_les_pieds_Nom_DOM_", x)) +
    get(paste0("Indiv_NB_Interception_Nom_DOM_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}

#EXT
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables <- length(ls(pattern = "^Indiv_NB_Récupération_dans_les_pieds_Nom_EXT_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Ballon_gagné_Nom_EXT_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Récupération_dans_les_pieds_Nom_EXT_", x)) +
    get(paste0("Indiv_NB_Interception_Nom_EXT_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}


#Ballon donné
#DOM
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables <- length(ls(pattern = "^Indiv_NB_Intercepté_Nom_DOM_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Ballon_donné_Nom_DOM_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Intercepté_Nom_DOM_", x)) +
    get(paste0("Indiv_NB_Perte_dans_les_pieds_Nom_DOM_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}

#EXT
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables <- length(ls(pattern = "^Indiv_NB_Intercepté_Nom_EXT_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Ballon_donné_Nom_EXT_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Intercepté_Nom_EXT_", x)) +
    get(paste0("Indiv_NB_Perte_dans_les_pieds_Nom_EXT_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}


#Tirs cadrés avec buts
#DOM
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables <- length(ls(pattern = "^Indiv_NB_Tir_cadré_Nom_DOM_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Tir_cadré_contenant_tirs_des_buts_Nom_DOM_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Tir_cadré_Nom_DOM_", x)) +
    get(paste0("Indiv_NB_Buteur_Nom_DOM_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}

#EXT
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables <- length(ls(pattern = "^Indiv_NB_Tir_cadré_Nom_EXT_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Tir_cadré_contenant_tirs_des_buts_Nom_EXT_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Tir_cadré_Nom_EXT_", x)) +
    get(paste0("Indiv_NB_Buteur_Nom_EXT_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}

#Tirs normaux avec buts
#DOM
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables <- length(ls(pattern = "^Indiv_NB_Tir_contré_Nom_DOM_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Tir_tout_compris_Nom_DOM_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Tir_cadré_contenant_tirs_des_buts_Nom_DOM_", x)) +
    get(paste0("Indiv_NB_Tir_contré_Nom_DOM_", x)) +
    get(paste0("Indiv_NB_Tir_non_cadré_Nom_DOM_", x)) +
    get(paste0("Indiv_NB_Tir_sur_montant_Nom_DOM_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}

#EXT
#compter le nombre de joueurs pour savoir combien de variable additionner.
nombre_de_variables <- length(ls(pattern = "^Indiv_NB_Tir_contré_Nom_EXT_"))

# Parcourir chaque valeur de x de 1 à 12
for (x in 1:nombre_de_variables) {
  # Créer le nom de la nouvelle variable
  nom_variable_nouvelle <- paste0("Indiv_NB_Tir_tout_compris_Nom_EXT_", x)
  
  # Calculer la somme des variables correspondantes pour chaque type d'action
  somme <- get(paste0("Indiv_NB_Tir_cadré_contenant_tirs_des_buts_Nom_EXT_", x)) +
    get(paste0("Indiv_NB_Tir_contré_Nom_EXT_", x)) +
    get(paste0("Indiv_NB_Tir_non_cadré_Nom_EXT_", x)) +
    get(paste0("Indiv_NB_Tir_sur_montant_Nom_EXT_", x))
  
  # Assigner la somme à la nouvelle variable
  assign(nom_variable_nouvelle, somme)
}


#Calcul titulaires MT1 et MT2
# Créer une liste pour stocker les noms des joueurs
liste_noms_joueurs_Titulaires_MT1 <- vector("character", length = 10)

# Compter le nombre de "Entrée Solo" trouvés
nombre_entree_solo <- 0

# Parcourir les en-têtes des colonnes d'action
for (col_index in 6:ncol(df1)) {
  # Vérifier si l'en-tête commence par "Entrée Solo"
  if (startsWith(colnames(df1)[col_index], "Entrée Solo")) {
    # Parcourir les lignes de la colonne en question
    for (row_index in 1:nrow(df1)) {
      # Vérifier si la valeur dans la colonne est non nulle
      if (!is.na(df1[row_index, col_index])) {
        # Récupérer le nom du joueur dans la colonne "Nom"
        nom_joueur <- df1[row_index, "Nom"]
        
        # Ajouter le nom du joueur à la liste
        liste_noms_joueurs_Titulaires_MT1[nombre_entree_solo + 1] <- nom_joueur
        
        # Incrémenter le compteur de "Entrée Solo" trouvés
        nombre_entree_solo <- nombre_entree_solo + 1
        
        # Sortir de la boucle si on a trouvé les 10 premiers "Entrée Solo"
        if (nombre_entree_solo == 10) {
          break
        }
      }
    }
    
    # Sortir de la boucle principale si on a trouvé les 10 premiers "Entrée Solo"
    if (nombre_entree_solo == 10) {
      break
    }
  }
}


# Trouver le numéro de colonne contenant "MT"
col_MT <- grep("^MT", colnames(df1))

# Vérifier si "MT" a été trouvé
if (length(col_MT) == 0) {
  stop("Aucune colonne avec l'en-tête 'MT' n'a été trouvée dans le dataframe.")
}

# Créer une liste pour stocker les noms des joueurs
liste_noms_joueurs_Titulaires_MT2 <- vector("character", length = 10)

# Compter le nombre de "Entrée Solo" trouvés
nombre_entree_solo <- 0

# Parcourir les en-têtes des colonnes d'action
for (col_index in (col_MT + 1):ncol(df1)) {
  # Vérifier si l'en-tête commence par "Entrée Solo"
  if (startsWith(colnames(df1)[col_index], "Entrée Solo")) {
    # Parcourir les lignes de la colonne en question
    for (row_index in 1:nrow(df1)) {
      # Vérifier si la valeur dans la colonne est non nulle
      if (!is.na(df1[row_index, col_index])) {
        # Récupérer le nom du joueur dans la colonne "Nom"
        nom_joueur <- df1[row_index, "Nom"]
        
        # Ajouter le nom du joueur à la liste
        liste_noms_joueurs_Titulaires_MT2[nombre_entree_solo + 1] <- nom_joueur
        
        # Incrémenter le compteur de "Entrée Solo" trouvés
        nombre_entree_solo <- nombre_entree_solo + 1
        
        # Sortir de la boucle si on a trouvé les 10 premiers "Entrée Solo"
        if (nombre_entree_solo == 10) {
          break
        }
      }
    }
    
    # Sortir de la boucle principale si on a trouvé les 10 premiers "Entrée Solo"
    if (nombre_entree_solo == 10) {
      break
    }
  }
}


#Tableau stats individuelles
# Convertir les variables Nom_joueurs_EXT et Nom_joueurs_DOM en vecteurs de caractères
NomCharacter_joueurs_EXT <- unlist(Nom_joueurs_EXT)
NomCharacter_joueurs_DOM <- unlist(Nom_joueurs_DOM)

# Création d'une liste unique de tous les noms de joueurs
tous_les_noms <- unique(c(NomCharacter_joueurs_DOM, NomCharacter_joueurs_EXT))

# Création d'un vecteur pour les équipes correspondant à chaque nom de joueur
equipes <- c(rep(Nom_EquipeDomicile, length(NomCharacter_joueurs_DOM)), rep(Nom_EquipeExterieur, length(NomCharacter_joueurs_EXT)))

# Création du dataframe avec les noms de joueurs uniques et les équipes correspondantes
df_stats_joueurs <- data.frame(
  Nom_joueur = rep(tous_les_noms, each = 1),
  Equipe = rep(equipes, each = 1),
  matrix(NA, ncol = 27, nrow = length(tous_les_noms))
)

# Ajout des noms des colonnes
colnames(df_stats_joueurs)[3:28] <- c("Temps_de_jeu", "But", "Passe_décisive", "Tir", "Tir_sur_montants", "Tir_cadré", "Tir_contré",
                                      "Tir_non_cadré", "Ballon_perdu", "Ballon_donné", "Ballon_gagné",
                                      "Dégagement", "Perte_dans_les_pieds", "Intercepté",
                                      "Récupération_dans_les_pieds", "Interception", "Présence_but_concédé",
                                      "Présence_but_inscrit", "Dribble_réussi", "Dribble_raté", "Faute_commise",
                                      "Faute_subie", "Titularisation_sur_les_MT", "Nombre_de_changement",
                                      "Carton_jaune", "Carton_rouge")

# Convertir la liste nommée en vecteur
temps_jeu_vector <- unlist(TempsJeu)

# Extraire les noms de joueurs à partir des noms attribués à chaque élément de la liste
noms_joueurs <- gsub(pattern = "^TempsJeu_", replacement = "", names(TempsJeu))

# Créer un dataframe avec les noms de joueurs et leurs temps de jeu
df_temps_jeu <- data.frame(Nom_joueur = noms_joueurs, Temps_de_jeu = temps_jeu_vector, stringsAsFactors = FALSE)

# Supprimer le préfixe des noms de joueurs dans df_temps_jeu
df_temps_jeu$Nom_joueur <- gsub(paste0("^", Nom_EquipeDomicile, "_"), "", df_temps_jeu$Nom_joueur)
df_temps_jeu$Nom_joueur <- gsub(paste0("^", Nom_EquipeExterieur, "_"), "", df_temps_jeu$Nom_joueur)

# Fusionner les dataframes sur la colonne Nom_joueur
df_stats_joueurs <- merge(df_stats_joueurs, df_temps_jeu[, c("Nom_joueur", "Temps_de_jeu")], by = "Nom_joueur", all.x = TRUE)

# Réorganiser les colonnes pour placer "Temps_de_jeu" en troisième position
df_stats_joueurs <- df_stats_joueurs[, c(1, 2, 30, 4:29)]

# Supprimer la 29ème colonne
df_stats_joueurs <- subset(df_stats_joueurs, select = -29)

# Renommer la colonne "Temps_de_jeu" correctement
colnames(df_stats_joueurs)[3] <- "Temps_de_jeu"



# Création d'une nouvelle colonne pour stocker les noms de variable correspondants aux noms de joueurs
df_stats_joueurs$Nom_variable <- NA

# Parcourir chaque ligne du dataframe df_stats_joueurs
for (i in 1:nrow(df_stats_joueurs)) {
  # Récupérer le nom du joueur dans la colonne 1
  nom_joueur <- df_stats_joueurs[i, 1]
  
  # Vérifier si le nom du joueur est dans Nom_joueurs_DOM
  if (nom_joueur %in% sapply(Nom_joueurs_DOM, `[`, 1)) {
    # Trouver la position du joueur dans Nom_joueurs_DOM
    position <- which(sapply(Nom_joueurs_DOM, `[`, 1) == nom_joueur)
    # Écrire le résultat dans la colonne 29
    df_stats_joueurs[i, 29] <- paste0("Nom_DOM_", position)
  }
  
  # Vérifier si le nom du joueur est dans Nom_joueurs_EXT
  if (nom_joueur %in% sapply(Nom_joueurs_EXT, `[`, 1)) {
    # Trouver la position du joueur dans Nom_joueurs_EXT
    position <- which(sapply(Nom_joueurs_EXT, `[`, 1) == nom_joueur)
    # Écrire le résultat dans la colonne 29
    df_stats_joueurs[i, 29] <- paste0("Nom_EXT_", position)
  }
}



# Liste des types d'action
types_action <- c("Buteur",
                  "Passeur_D",
                  "Tir_tout_compris",
                  "Tir_sur_montant",
                  "Tir_cadré_contenant_tirs_des_buts",
                  "Tir_contré",
                  "Tir_non_cadré",
                  "Ballon_perdu",
                  "Ballon_donné",
                  "Ballon_gagné",
                  "Dégagement",
                  "Perte_dans_les_pieds",
                  "Intercepté",
                  "Récupération_dans_les_pieds",
                  "Interception",
                  "Présent_But_concédé",
                  "Présent_But_marqué",
                  "Dribble_réussi",
                  "Dribble_raté",
                  "Faute_C_concédée",
                  "Faute_C_subie",
                  "Sortie",
                  "Carton_J",
                  "Carton_R"
) # Ajoutez tous les types d'action nécessaires

# Liste des colonnes de destination pour chaque type d'action
colonnes_destination <- c("But",
                          "Passe_décisive",
                          "Tir",
                          "Tir_sur_montants",
                          "Tir_cadré",
                          "Tir_contré",
                          "Tir_non_cadré",
                          "Ballon_perdu",
                          "Ballon_donné",
                          "Ballon_gagné",
                          "Dégagement",
                          "Perte_dans_les_pieds",
                          "Intercepté",
                          "Récupération_dans_les_pieds",
                          "Interception",
                          "Présence_but_concédé",
                          "Présence_but_inscrit",
                          "Dribble_réussi",
                          "Dribble_raté",
                          "Faute_commise",
                          "Faute_subie",
                          "Nombre_de_changement",
                          "Carton_jaune",
                          "Carton_rouge"
) # Ajoutez les noms des colonnes correspondantes

# Parcours de chaque type d'action
for (i in 1:length(types_action)) {
  type <- types_action[i]
  colonne <- colonnes_destination[i]
  
  # Parcours du dataframe df_stats_joueurs
  for (i in 1:nrow(df_stats_joueurs)) {
    # Extraire le numéro de joueur et l'équipe du nom de la variable dans la colonne Nom_variable
    nom_variable <- df_stats_joueurs[i, "Nom_variable"]
    equipe <- substr(nom_variable, nchar(nom_variable)-2, nchar(nom_variable))
    numero_joueur <- as.numeric(substr(nom_variable, nchar(nom_variable), nchar(nom_variable)))
    
    # Sélectionner le nom de la variable correspondant à l'équipe et au numéro de joueur
    nom_variable_indiv <- paste0("Indiv_NB_", type, "_", nom_variable)
    
    # Vérifier si le vecteur Indiv_NB_Typed'action_Nom_EQUIPE_x existe
    if (exists(nom_variable_indiv)) {
      # Sélectionner la valeur du vecteur Indiv_NB_Typed'action_Nom_EQUIPE_x
      valeur <- get(nom_variable_indiv)
      
      # Écrire la valeur dans la colonne spécifiée de la ligne dans df_stats_joueurs
      df_stats_joueurs[i, colonne] <- valeur
    }
  }
}


# Création de la colonne pour stocker les informations sur la présence des joueurs dans les listes
df_stats_joueurs$Presence_listes <- 0

# Parcours de chaque ligne du dataframe df_stats_joueurs
for (i in 1:nrow(df_stats_joueurs)) {
  # Récupération du nom du joueur dans la colonne 1
  nom_joueur <- df_stats_joueurs[i, 1]
  
  # Vérification de la présence dans la première liste
  if (nom_joueur %in% liste_noms_joueurs_Titulaires_MT1) {
    df_stats_joueurs[i, "Presence_listes"] <- df_stats_joueurs[i, "Presence_listes"] + 1
  }
  
  # Vérification de la présence dans la deuxième liste
  if (nom_joueur %in% liste_noms_joueurs_Titulaires_MT2) {
    df_stats_joueurs[i, "Presence_listes"] <- df_stats_joueurs[i, "Presence_listes"] + 1
  }
}

# Remplacer les valeurs supérieures à 2 par 2
df_stats_joueurs$Presence_listes[df_stats_joueurs$Presence_listes > 2] <- 2

# Copier les valeurs de la colonne 30 dans la colonne 25
df_stats_joueurs$Titularisation_sur_les_MT <- df_stats_joueurs$Presence_liste

#Supprimer la colonne Presence_liste
df_stats_joueurs <- subset(df_stats_joueurs, select = -30)



#Tableau de stats indiv avec filtre sur le nom du joueur
# Sélectionner le premier nom de la colonne 1 (Nom_joueur)
Sélection_actuelle <- "Nom_DOM_1"
motif_recherche <- paste0("^", Sélection_actuelle, "$")

filtre_par_numéro <- grep(motif_recherche , df_stats_joueurs$Nom_variable)

nom_filtre <- df_stats_joueurs[filtre_par_numéro, 1]

# Appliquer le filtre
df_stats_joueurs_1 <- df_stats_joueurs[df_stats_joueurs$Nom_joueur == nom_filtre, ]

#Supprimer colonnes inutiles
df_stats_joueurs_1 <- subset(df_stats_joueurs_1, select = -2)
df_stats_joueurs_1 <- subset(df_stats_joueurs_1, select = -28)

#Transposer le tableau
df_stats_joueurs_1 <- t(df_stats_joueurs_1)


# === Sous chapitre 11 : Nécessaires pour R markdown ==============================================

#Chemins pour logos

# Créer le chemin complet de l'image de l'équipe domicile en enlevant les espaces
chemin_image_dom <- paste("C:/Users/Fabien/Documents/Futsal/Stats/Logos/", gsub(" ", "", EquipeDomicile), ".png", sep = "")

# Créer le chemin complet de l'image de l'équipe extérieure en enlevant les espaces
chemin_image_ext <- paste("C:/Users/Fabien/Documents/Futsal/Stats/Logos/", gsub(" ", "", EquipeExterieur), ".png", sep = "")

# Vérifier si le fichier image de l'équipe domicile existe, sinon utiliser l'image neutre
if (!file.exists(chemin_image_dom)) {
  chemin_image_dom <- "C:/Users/Fabien/Documents/Futsal/Stats/Logos/Neutre.jpg"
}

# Vérifier si le fichier image de l'équipe extérieure existe, sinon utiliser l'image neutre
if (!file.exists(chemin_image_ext)) {
  chemin_image_ext <- "C:/Users/Fabien/Documents/Futsal/Stats/Logos/Neutre.jpg"
}

# Chargez l'image depuis un fichier en mode binaire
image_dom <- readBin(chemin_image_dom, "raw", file.size(chemin_image_dom))
image_ext <- readBin(chemin_image_ext, "raw", file.size(chemin_image_ext))

# Convertissez les images en Base64
image_dom_base64 <- base64enc::base64encode(image_dom)
image_ext_base64 <- base64enc::base64encode(image_ext)


#Pourcentage pour tableau récap

#Variables pour les pourcentages du tableau récap 
# Calcul du total et du pourcentage pour la première paire de variables
total1 <- ScoreEquipeDOM + ScoreEquipeEXT
pourcentage1_DOM <- (ScoreEquipeDOM / total1) * 100
pourcentage1_EXT <- (ScoreEquipeEXT / total1) * 100

# Calcul du total et du pourcentage pour la deuxième paire de variables
total2 <- NB_Possession_DOM + NB_Possession_EXT
pourcentage2_DOM <- (NB_Possession_DOM / total2) * 100
pourcentage2_EXT <- (NB_Possession_EXT / total2) * 100

# Calcul du total et du pourcentage pour la troisième paire de variables
total3 <- NB_Tir_DOM + NB_Tir_EXT
pourcentage3_DOM <- (NB_Tir_DOM / total3) * 100
pourcentage3_EXT <- (NB_Tir_EXT / total3) * 100

# Calcul du total et du pourcentage pour la quatrième paire de variables
total4 <- NB_Tir_Cadré_DOM + NB_Tir_Cadré_EXT
pourcentage4_DOM <- (NB_Tir_Cadré_DOM / total4) * 100
pourcentage4_EXT <- (NB_Tir_Cadré_EXT / total4) * 100

# Calcul du total et du pourcentage pour la cinquième paire de variables
total5 <- NB_Tir_non_cadré_DOM + NB_Tir_non_cadré_EXT
pourcentage5_DOM <- (NB_Tir_non_cadré_DOM / total5) * 100
pourcentage5_EXT <- (NB_Tir_non_cadré_EXT / total5) * 100

# Calcul du total et du pourcentage pour la sixième paire de variables
total6 <- NB_Tir_contré_DOM + NB_Tir_contré_EXT
pourcentage6_DOM <- (NB_Tir_contré_DOM / total6) * 100
pourcentage6_EXT <- (NB_Tir_contré_EXT / total6) * 100

# Calcul du total et du pourcentage pour la septième paire de variables
total7 <- NB_Tir_sur_montant_DOM + NB_Tir_sur_montant_EXT
pourcentage7_DOM <- (NB_Tir_sur_montant_DOM / total7) * 100
pourcentage7_EXT <- (NB_Tir_sur_montant_EXT / total7) * 100

# Calcul du total et du pourcentage pour la huitième paire de variables
total8 <- NB_Arrêt_DOM + NB_Arrêt_EXT
pourcentage8_DOM <- (NB_Arrêt_DOM / total8) * 100
pourcentage8_EXT <- (NB_Arrêt_EXT / total8) * 100

# Calcul du total et du pourcentage pour la neuvième paire de variables
total9 <- NB_Faute_C_concédée_DOM + NB_Faute_C_concédée_EXT
pourcentage9_DOM <- (NB_Faute_C_concédée_DOM / total9) * 100
pourcentage9_EXT <- (NB_Faute_C_concédée_EXT / total9) * 100

# Calcul du total et du pourcentage pour la dixième paire de variables
total10 <- NB_Carton_J_DOM + NB_Carton_J_EXT
pourcentage10_DOM <- (NB_Carton_J_DOM / total10) * 100
pourcentage10_EXT <- (NB_Carton_J_EXT / total10) * 100

# Calcul du total et du pourcentage pour la onzième paire de variables
total11 <- NB_Carton_R_DOM + NB_Carton_R_EXT
pourcentage11_DOM <- (NB_Carton_R_DOM / total11) * 100
pourcentage11_EXT <- (NB_Carton_R_EXT / total11) * 100

# Calcul du total et du pourcentage pour la douzième paire de variables
total12 <- NB_Ballon_Gagné_DOM + NB_Ballon_Gagné_EXT
pourcentage12_DOM <- (NB_Ballon_Gagné_DOM / total12) * 100
pourcentage12_EXT <- (NB_Ballon_Gagné_EXT / total12) * 100

# Calcul du total et du pourcentage pour la treizième paire de variables
total13 <- NB_Ballon_Perdu_DOM + NB_Ballon_Perdu_EXT
pourcentage13_DOM <- (NB_Ballon_Perdu_DOM / total13) * 100
pourcentage13_EXT <- (NB_Ballon_Perdu_EXT / total13) * 100

# Calcul du total et du pourcentage pour la quatorzième paire de variables
total14 <- NB_Dribble_réussi_DOM + NB_Dribble_réussi_EXT
pourcentage14_DOM <- (NB_Dribble_réussi_DOM / total14) * 100
pourcentage14_EXT <- (NB_Dribble_réussi_EXT / total14) * 100

# Calcul du total et du pourcentage pour la quinzième paire de variables
total15 <- NB_Dribble_raté_DOM + NB_Dribble_raté_EXT
pourcentage15_DOM <- (NB_Dribble_raté_DOM / total15) * 100
pourcentage15_EXT <- (NB_Dribble_raté_EXT / total15) * 100

# Calcul du total et du pourcentage pour la seizième paire de variables
total16 <- NB_Relance_réussie_DOM + NB_Relance_réussie_EXT
pourcentage16_DOM <- (NB_Relance_réussie_DOM / total16) * 100
pourcentage16_EXT <- (NB_Relance_réussie_EXT / total16) * 100

# Calcul du total et du pourcentage pour la dix-septième paire de variables
total17 <- NB_Relance_raté_DOM + NB_Relance_raté_EXT
pourcentage17_DOM <- (NB_Relance_raté_DOM / total17) * 100
pourcentage17_EXT <- (NB_Relance_raté_EXT / total17) * 100


# ===========================================================================
# === CHAPITRE 6 : Fichier RMARKDOWN ==============================================
# ===========================================================================


# Créer le contenu du fichier Rmarkdown avec le score du match et les images des équipes
contenu_rmarkdown <- paste("
---
title: ''
output:
  html_document:
    css: styles.css
    includes:
      in_header: header.html
---

<style>
  /* Appliquer un espacement de 10 pixels entre les lignes du tableau */
  #tableau td, #tableau th, #tableau2 td, #tableau2 th {
    padding-top: 10px;
    padding-bottom: 0px;
  }
  /* Ajouter une bordure en bas de chaque ligne des tableaux */
  table tr {
    border-bottom: 1px solid black;
  }
</style>

<div style='text-align: center;'>
  <h1 style='font-size: 40px;'>Rapport de match</h1>
  <div style='font-size: 88px;'>", ScoreEquipeDOM, " - ", ScoreEquipeEXT, "</div>
  <div style='float: left; margin-top: -130px;'><img src='data:image/png;base64,", image_dom_base64, "' width='100'></div>
  <div style='float: right; margin-top: -130px;'><img src='data:image/png;base64,", image_ext_base64, "' width='100'></div>
  <div style='clear: both;'></div>
  
  <div style='height: 20px;'></div> <!-- Réduire l'espace vertical -->
  
  <table id='tableau' style='font-size: 13px;'>
    <colgroup>
      <col style='width: 20px;'>
      <col style='width: 210px;'>
      <col>
    </colgroup>"
)

# Créer une liste des temps de jeu pour chaque joueur de l'équipe domicile
temps_jeu_liste <- sapply(Nom_joueurs_DOM, function(nom_joueur) {
  clef_temps_jeu <- paste0("TempsJeu_", EquipeDomicile, "_", nom_joueur)
  TempsJeu[[clef_temps_jeu]]
})

# Obtenir l'ordre de tri des indices pour l'équipe domicile
indices_tri <- order(temps_jeu_liste, decreasing = TRUE)

# Réorganiser les noms des joueurs et les temps de jeu en fonction de ces indices pour l'équipe domicile
Nom_joueurs_DOM_tri <- Nom_joueurs_DOM[indices_tri]
temps_jeu_DOM_tri <- temps_jeu_liste[indices_tri]

# Tronquer les noms des joueurs pour éviter les retours à la ligne dans le tableau
Nom_joueurs_DOM_tri <- sapply(Nom_joueurs_DOM_tri, tronquer_nom, longueur_max = 25)

# Ajouter les lignes triées pour chaque joueur avec son numéro, nom et temps de jeu pour l'équipe domicile
for (i in 1:length(Nom_joueurs_DOM_tri)) {
  # Récupérer le nom du joueur et son temps de jeu triés pour l'équipe domicile
  nom_joueur <- Nom_joueurs_DOM_tri[i]
  temps_jeu <- temps_jeu_DOM_tri[i]
  
  # Ajouter la ligne au tableau pour l'équipe domicile
  contenu_rmarkdown <- paste(contenu_rmarkdown, "
    <tr>
      <td style='text-align: center;'>", i, "</td>
      <td style='text-align: left;'>", nom_joueur, "</td>
      <td>", temps_jeu, "</td>
    </tr>"
  )
}

# Ajouter la fin du premier tableau et le début du deuxième tableau à l'extrémité droite
contenu_rmarkdown <- paste(contenu_rmarkdown, "
  </table>
  <table id='tableau2' style='font-size: 13px; float: right; margin-top: -355px;'>
    <colgroup>
      <col style='width: 20px;'>
      <col style='width: 210px;'>
      <col>
    </colgroup>"
)

# Créer une liste des temps de jeu pour chaque joueur de l'équipe extérieure
temps_jeu_liste_ext <- sapply(Nom_joueurs_EXT, function(nom_joueur) {
  clef_temps_jeu <- paste0("TempsJeu_", EquipeExterieur, "_", nom_joueur)
  TempsJeu[[clef_temps_jeu]]
})

# Obtenir l'ordre de tri des indices pour l'équipe extérieure
indices_tri_ext <- order(temps_jeu_liste_ext, decreasing = TRUE)

# Réorganiser les noms des joueurs et les temps de jeu en fonction de ces indices pour l'équipe extérieure
Nom_joueurs_EXT_tri <- Nom_joueurs_EXT[indices_tri_ext]
temps_jeu_EXT_tri <- temps_jeu_liste_ext[indices_tri_ext]

# Tronquer les noms des joueurs pour éviter les retours à la ligne dans le tableau
Nom_joueurs_EXT_tri <- sapply(Nom_joueurs_EXT_tri, tronquer_nom, longueur_max = 25)

# Ajouter les lignes triées pour chaque joueur avec son numéro, nom et temps de jeu pour l'équipe extérieure
for (i in 1:length(Nom_joueurs_EXT_tri)) {
  # Récupérer le nom du joueur et son temps de jeu triés pour l'équipe extérieure
  nom_joueur_ext <- Nom_joueurs_EXT_tri[i]
  temps_jeu_ext <- temps_jeu_EXT_tri[i]
  
  # Ajouter la ligne au deuxième tableau pour l'équipe extérieure
  contenu_rmarkdown <- paste(contenu_rmarkdown, "
    <tr>
      <td style='text-align: center;'>", i, "</td>
      <td style='text-align: left;'>", nom_joueur_ext, "</td>
      <td>", temps_jeu_ext, "</td>
    </tr>"
  )
}


#Ajouter Tableau 3
contenu_rmarkdown <- paste(contenu_rmarkdown, "

<table id='tableau3' style='font-size: 13px; border-collapse: collapse; margin-left: 285px ; font-weight: bold; color: couleur_texte; position: relative; top: -338px;'>
  <colgroup>
    <col style='width: 20px;'>
    <col style='width: 300px;'>
    <col style='width: 20px;'>
  </colgroup>
  <tbody>
    <tr>
      <td style='border: 1px solid black;'>", ScoreEquipeDOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage1_DOM > pourcentage1_EXT, "right", "left"), ", ", ifelse(pourcentage1_DOM > pourcentage1_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage1_DOM, pourcentage1_EXT), "%"), ", ", ifelse(pourcentage1_DOM > pourcentage1_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage1_DOM, pourcentage1_EXT), "%"), ");'>Buts</td>
      <td style='border: 1px solid black;'>", ScoreEquipeEXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Possession_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage2_DOM > pourcentage2_EXT, "right", "left"), ", ", ifelse(pourcentage2_DOM > pourcentage2_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage2_DOM, pourcentage2_EXT), "%"), ", ", ifelse(pourcentage2_DOM > pourcentage2_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage2_DOM, pourcentage2_EXT), "%"), ");'>Possession</td>
      <td style='border: 1px solid black;'>", NB_Possession_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Tir_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage3_DOM > pourcentage3_EXT, "right", "left"), ", ", ifelse(pourcentage3_DOM > pourcentage3_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage3_DOM, pourcentage3_EXT), "%"), ", ", ifelse(pourcentage3_DOM > pourcentage3_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage3_DOM, pourcentage3_EXT), "%"), ");'>Tirs</td>
      <td style='border: 1px solid black;'>", NB_Tir_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Tir_Cadré_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage4_DOM > pourcentage4_EXT, "right", "left"), ", ", ifelse(pourcentage4_DOM > pourcentage4_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage4_DOM, pourcentage4_EXT), "%"), ", ", ifelse(pourcentage4_DOM > pourcentage4_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage4_DOM, pourcentage4_EXT), "%"), ");'>Cadrés</td>
      <td style='border: 1px solid black;'>", NB_Tir_Cadré_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Tir_non_cadré_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage5_DOM > pourcentage5_EXT, "right", "left"), ", ", ifelse(pourcentage5_DOM > pourcentage5_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage5_DOM, pourcentage5_EXT), "%"), ", ", ifelse(pourcentage5_DOM > pourcentage5_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage5_DOM, pourcentage5_EXT), "%"), ");'>Non cadrés</td>
      <td style='border: 1px solid black;'>", NB_Tir_non_cadré_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Tir_contré_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage6_DOM > pourcentage6_EXT, "right", "left"), ", ", ifelse(pourcentage6_DOM > pourcentage6_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage6_DOM, pourcentage6_EXT), "%"), ", ", ifelse(pourcentage6_DOM > pourcentage6_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage6_DOM, pourcentage6_EXT), "%"), ");'>Contrés</td>
      <td style='border: 1px solid black;'>", NB_Tir_contré_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Tir_sur_montant_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage7_DOM > pourcentage7_EXT, "right", "left"), ", ", ifelse(pourcentage7_DOM > pourcentage7_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage7_DOM, pourcentage7_EXT), "%"), ", ", ifelse(pourcentage7_DOM > pourcentage7_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage7_DOM, pourcentage7_EXT), "%"), ");'>Montants</td>
      <td style='border: 1px solid black;'>", NB_Tir_sur_montant_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Arrêt_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage8_DOM > pourcentage8_EXT, "right", "left"), ", ", ifelse(pourcentage8_DOM > pourcentage8_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage8_DOM, pourcentage8_EXT), "%"), ", ", ifelse(pourcentage8_DOM > pourcentage8_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage8_DOM, pourcentage8_EXT), "%"), ");'>Arrêts</td>
      <td style='border: 1px solid black;'>", NB_Arrêt_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Faute_C_concédée_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage9_DOM > pourcentage9_EXT, "right", "left"), ", ", ifelse(pourcentage9_DOM > pourcentage9_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage9_DOM, pourcentage9_EXT), "%"), ", ", ifelse(pourcentage9_DOM > pourcentage9_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage9_DOM, pourcentage9_EXT), "%"), ");'>Fautes</td>
      <td style='border: 1px solid black;'>", NB_Faute_C_concédée_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Carton_J_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage10_DOM > pourcentage10_EXT, "right", "left"), ", ", ifelse(pourcentage10_DOM > pourcentage10_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage10_DOM, pourcentage10_EXT), "%"), ", ", ifelse(pourcentage10_DOM > pourcentage10_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage10_DOM, pourcentage10_EXT), "%"), ");'>Cartons jaune</td>
      <td style='border: 1px solid black;'>", NB_Carton_J_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Carton_R_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage11_DOM > pourcentage11_EXT, "right", "left"), ", ", ifelse(pourcentage11_DOM > pourcentage11_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage11_DOM, pourcentage11_EXT), "%"), ", ", ifelse(pourcentage11_DOM > pourcentage11_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage11_DOM, pourcentage11_EXT), "%"), ");'>Cartons rouge</td>
      <td style='border: 1px solid black;'>", NB_Carton_R_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Ballon_Gagné_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage12_DOM > pourcentage12_EXT, "right", "left"), ", ", ifelse(pourcentage12_DOM > pourcentage12_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage12_DOM, pourcentage12_EXT), "%"), ", ", ifelse(pourcentage12_DOM > pourcentage12_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage12_DOM, pourcentage12_EXT), "%"), ");'>Ballons gagnés</td>
      <td style='border: 1px solid black;'>", NB_Ballon_Gagné_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Ballon_Perdu_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage13_DOM > pourcentage13_EXT, "right", "left"), ", ", ifelse(pourcentage13_DOM > pourcentage13_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage13_DOM, pourcentage13_EXT), "%"), ", ", ifelse(pourcentage13_DOM > pourcentage13_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage13_DOM, pourcentage13_EXT), "%"), ");'>Ballons perdus</td>
      <td style='border: 1px solid black;'>", NB_Ballon_Perdu_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Dribble_réussi_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage14_DOM > pourcentage14_EXT, "right", "left"), ", ", ifelse(pourcentage14_DOM > pourcentage14_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage14_DOM, pourcentage14_EXT), "%"), ", ", ifelse(pourcentage14_DOM > pourcentage14_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage14_DOM, pourcentage14_EXT), "%"), ");'>Dribbles réussis</td>
      <td style='border: 1px solid black;'>", NB_Dribble_réussi_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Dribble_raté_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage15_DOM > pourcentage15_EXT, "right", "left"), ", ", ifelse(pourcentage15_DOM > pourcentage15_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage15_DOM, pourcentage15_EXT), "%"), ", ", ifelse(pourcentage15_DOM > pourcentage15_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage15_DOM, pourcentage15_EXT), "%"), ");'>Dribbles ratés</td>
      <td style='border: 1px solid black;'>", NB_Dribble_raté_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Relance_réussie_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage16_DOM > pourcentage16_EXT, "right", "left"), ", ", ifelse(pourcentage16_DOM > pourcentage16_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage16_DOM, pourcentage16_EXT), "%"), ", ", ifelse(pourcentage16_DOM > pourcentage16_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage16_DOM, pourcentage16_EXT), "%"), ");'>Relances réussies</td>
      <td style='border: 1px solid black;'>", NB_Relance_réussie_EXT, "</td>
    </tr>
    <tr>
      <td style='border: 1px solid black;'>", NB_Relance_raté_DOM, "</td>
      <td style='border: 1px solid black; background: linear-gradient(to ", ifelse(pourcentage17_DOM > pourcentage17_EXT, "right", "left"), ", ", ifelse(pourcentage17_DOM > pourcentage17_EXT, Couleur_Equipe_DOM, Couleur_Equipe_EXT), paste0(pmax(pourcentage17_DOM, pourcentage17_EXT), "%"), ", ", ifelse(pourcentage17_DOM > pourcentage17_EXT, Couleur_Equipe_EXT, Couleur_Equipe_DOM), paste0(pmin(pourcentage17_DOM, pourcentage17_EXT), "%"), ");'>Relances ratées</td>
      <td style='border: 1px solid black;'>", NB_Relance_raté_EXT, "</td>
    </tr>
  </tbody>
</table>
</div>
")



# Écrire le contenu dans un fichier Rmarkdown
writeLines(contenu_rmarkdown, "rapport_de_match.Rmd")

# Écrire le contenu du fichier CSS
contenu_css <- "
div {
  text-align: center;
}
div > h1 {
  font-family: 'Aptos';
  font-size: 40px; /* Nouvelle taille du titre */
}

div > div {
  font-size: 88px; /* Taille du score */
}

"

# Écrire le contenu dans un fichier CSS
writeLines(contenu_css, "styles.css")


#FIN FICHIER RMARKDOWN


# Convertir le fichier Rmarkdown en HTML
rmarkdown::render("rapport_de_match.Rmd")

# Ouvrir le fichier HTML dans le navigateur par défaut
browseURL("rapport_de_match.html")


