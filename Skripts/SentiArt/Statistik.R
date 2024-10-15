library(dplyr)
library(ggplot2)
library(scales)

df_cleaned <- read.csv('C:/Users/jwieb/OneDrive - uni-bielefeld.de/Bachelorarbeit/output.csv', sep = ";")

gruppe_schauerkorpus <- df_cleaned %>% filter(subcorpus == "Schauerkorpus") %>% pull(fear_z)
gruppe_vergleichskorpus <- df_cleaned %>% filter(subcorpus == "Vergleichskorpus") %>% pull(fear_z)

# Student's t-Test durchführen (equal_var = TRUE)
t_test_result <- t.test(gruppe_schauerkorpus, gruppe_vergleichskorpus, var.equal = TRUE)

# Ergebnisse des t-Tests
print(t_test_result)

# Mittelwerte und Standardabweichungen berechnen
mean_schauerkorpus <- mean(gruppe_schauerkorpus, na.rm = TRUE)
std_schauerkorpus <- sd(gruppe_schauerkorpus, na.rm = TRUE)
n_schauerkorpus <- length(gruppe_schauerkorpus[!is.na(gruppe_schauerkorpus)])

mean_vergleichskorpus <- mean(gruppe_vergleichskorpus, na.rm = TRUE)
std_vergleichskorpus <- sd(gruppe_vergleichskorpus, na.rm = TRUE)
n_vergleichskorpus <- length(gruppe_vergleichskorpus[!is.na(gruppe_vergleichskorpus)])

# Confidence Interval berechnen
compute_confidence_interval <- function(mean, sd, n, confidence = 0.95) {
  error_margin <- sd * qt((1 + confidence) / 2, df = n - 1) / sqrt(n)
  return(c(mean - error_margin, mean + error_margin))
}

# Die Anzahl der Freiheitsgrade für den klassischen t-Test ist n1 + n2 - 2
df_total <- n_schauerkorpus + n_vergleichskorpus - 2

# Berechnung der Confidence Intervals für den klassischen t-Test
ci_schauerkorpus <- compute_confidence_interval(mean_schauerkorpus, std_schauerkorpus, n_schauerkorpus)
ci_vergleichskorpus <- compute_confidence_interval(mean_vergleichskorpus, std_vergleichskorpus, n_vergleichskorpus)

# Ergebnisse der Confidence Intervals ausgeben
cat("Schauerkorpus Mean:", mean_schauerkorpus, "95% CI:", ci_schauerkorpus, "\n")
cat("Vergleichskorpus Mean:", mean_vergleichskorpus, "95% CI:", ci_vergleichskorpus, "\n")

# Optional: Visualisieren der Ergebnisse
means <- c(mean_schauerkorpus, mean_vergleichskorpus)
cis <- rbind(ci_schauerkorpus, ci_vergleichskorpus)
groups <- c("Schauerkorpus", "Vergleichskorpus")

df_means_cis <- data.frame(Group = groups, Mean = means, CI_Lower = cis[,1], CI_Upper = cis[,2])

ggplot(df_means_cis, aes(x = Group, y = Mean, ymin = CI_Lower, ymax = CI_Upper)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  ylab("fear_z") +
  ggtitle("Durchschnittliche 'fear_z'-Werte in SentiArt mit 95% Konfidenzintervallen") +
  theme_minimal()

# Wilcoxon-Rangsummentest (Mann-Whitney-U-Test)
wilcox.test(fear_z ~ subcorpus, data = df_cleaned)

