# Template und Beispiel fuer eine Vorhersagemodellierung

# Paket laden
library(mosaic)

# Daten einlesen
# Achtung: An Stelle der richtigen Daten der Sonstigen Beteiligung werden hier die Trinkgelddaten verwendet
# Ggf. Dateipfad anpassen

Training <- read.csv2("../data/tips_train.csv")
Test <- read.csv2("../data/tips_test.csv")

# Modelliert werden soll "smoker==Yes".
# Dazu wird die Variable als factor definiert, wobei
# "No" y=0 und "Yes" y=1 entsprechen soll. 

Training <- Training %>%
  mutate(smoker = factor(smoker, levels = c("No","Yes")))

# Kontrolle:
levels(Training$smoker)

# Modell: Logistische Regression auf Basis von Rechnungshöhe und Tageszeit
erg.logit <- glm(smoker ~ total_bill + time, family = "binomial", data = Training)
summary(erg.logit)

# Anwendung auf Testdaten
# Ausgegeben wird die geschätzte Wahrscheinlichkeit von smoker
erg.predict <- predict(erg.logit, newdata = Test, type = "response")

# Zuordung zu smoker="Yes", wenn P>0.5 ist:
erg.predict <- ifelse(erg.predict > 0.5, "Yes", "No")

# Export der Prognose
write.csv2(erg.predict, "Prognose_MeinName.csv")
