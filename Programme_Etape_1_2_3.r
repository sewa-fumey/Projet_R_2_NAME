
"""Première étape : Chargement et nettoyage des données médicales """

library(tidyverse)
library(janitor)

# 📁 Chargement des données
# Remplace "donnees_medicales.csv" par ton propre nom de fichier si besoin
df <- read.csv("donnees_medicales.csv")

# 🧽 Nettoyage des noms de colonnes
df <- clean_names(df)

# 🧐 Vérification rapide
str(df)
summary(df)

# 🔄 Transformation des colonnes
df$gender <- factor(df$gender, levels = c(0, 1), labels = c("Femme", "Homme"))
df$result <- factor(df$result, levels = c("negative", "positive"), labels = c("Sain", "Pathologique"))

# 🧹 Détection des doublons (au cas où)
df <- df %>% distinct()

# 📊 Détection des valeurs aberrantes pour CK-MB
boxplot(df$ck_mb, main = "Boxplot CK-MB", col = "lightblue")

# 🧾 Filtrage des valeurs CK-MB extrêmes (facultatif, à ajuster)
df <- df %>% filter(ck_mb < 50)

# 📈 Résumé statistique par diagnostic
df %>%
  group_by(result) %>%
  summarise(
    Age_Moyen = round(mean(age), 1),
    Frequence_Cardiaque_Moyenne = round(mean(heart_rate), 1),
    Glycemie_Moyenne = round(mean(blood_sugar), 1),
    Nombre = n()
  )

# 📉 Visualisation de la distribution de l'âge selon le diagnostic
ggplot(df, aes(x = age, fill = result)) +
  geom_histogram(bins = 10, position = "dodge", alpha = 0.8) +
  labs(title = "Distribution de l'âge selon le diagnostic", x = "Âge", y = "Nombre de patients") +
  theme_minimal()



# 📦 Visualisation des taux de CK-MB selon le diagnostic
ggplot(df, aes(x = result, y = ck_mb, fill = result)) +
  geom_boxplot() +
  labs(title = "CK-MB selon le diagnostic", x = "Diagnostic", y = "CK-MB (ng/mL)") +
  theme_minimal()