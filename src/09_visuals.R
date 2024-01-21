library(ggplot2)
library(dplyr)


source("src/utils/data_preprocessing.R")




haeufigkeit <- as.data.frame(table(data$target))
haeufigkeit$proz <- round(haeufigkeit$Freq*100/sum(haeufigkeit$Freq), 1)

haeufigkeitSort <- haeufigkeit[order(-haeufigkeit$Freq), ]



## Pie Chart - Target -> Imbalanced Data
g1new <- ggplot(data, aes(x = factor(1))) + 
  geom_bar(stat="count", aes(fill = target)) +
  coord_polar("y") + 
  labs(title = "Anteil und Häufigkeit der (nicht) Wechselwilligen", 
       subtitle = "Tortendiagramm - Target", 
       fill = "//Legend title") +
  geom_text(data = haeufigkeitSort, aes(x  = 1.1, y = Freq, label=paste((proz), "%", "\n", (Freq))), 
            color="black", position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(
#        text = element_text(size = 18), 
        plot.title = element_text(size = 20), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14)
        ) +
  scale_fill_manual(values = c("1" = "#E40010", "0" = "#00B32C"), 
                    labels = c("0 - Nicht wechselwillig", "1 - wechselwillig"))
g1new


## Bar Chart - Education Level & Target -> No Difference
g2 <- ggplot(data, aes(x = company_size)) + 
  geom_bar(aes(fill=target), position = position_dodge2(preserve = "single")) +
  labs(title = "Wechselwilligkeit je Education Level", 
       subtitle = "Säulendiagramm - Education Level & Target", 
       x = "Education Level", y = "Häufigkeit") + 
  theme_light() + 
  theme(axis.text = element_text(colour="black", size=11)) + 
  scale_fill_manual(values = c("0" = "#B73377", "1" = "#2DA9D9"))
g2


## Density Plot - CDI & Target -> Big Differences
g4 <- ggplot(data, aes(x = city_development_index)) + 
  geom_density(aes(fill=target), alpha = 0.6) + 
  labs(title = "Wechselwilligkeit nach CDI (City Development Index)", 
       subtitle = "Kerndichteschätzer - CDI & Target", 
       x = "CDI", y = "Dichte") + 
#  scale_x_continuous(breaks=seq(20, 120, 20), lim = c(0, 140)) +
  theme_light() + 
  theme(axis.text = element_text(colour="black", size=11)) + 
  scale_fill_manual(values = c("0" = "#E40010", "1" = "#00B32C"))
g4



## Density Plot - CDI & Target -> Big Differences
data_1 <- data
data_1$experience <- as.character(data_1$experience)
data_1$experience[data_1$experience == "<1"] <- "0"
data_1$experience[data_1$experience == ">20"] <- "21"
data_1$experience <- as.numeric(data_1$experience)

data_1$company_size <- as.character(data_1$company_size)
data_1$company_size[data_1$company_size == "<10"] <- "5"
data_1$company_size[data_1$company_size == "10/49"] <- "30"
data_1$company_size[data_1$company_size == "100-500"] <- "300"
data_1$company_size[data_1$company_size == "1000-4999"] <- "3000"
data_1$company_size[data_1$company_size == "50-99"] <- "75"
data_1$company_size[data_1$company_size == "500-999"] <- "750"
data_1$company_size[data_1$company_size == "5000-9999"] <- "7500"
data_1$company_size[data_1$company_size == "10000+"] <- "15000"
data_1$company_size <- as.numeric(data_1$company_size)


## Density Plot - Experience & Target -> Differences
g5 <- ggplot(data, aes(x = experience)) + 
  geom_density(aes(fill=target), alpha = 0.6) + 
  labs(title = "Wechselwilligkeit nach Experience", 
       subtitle = "Kerndichteschätzer - Experience & Target", 
       x = "Experience", y = "Dichte") + 
  #  scale_x_continuous(breaks=seq(20, 120, 20), lim = c(0, 140)) +
  theme_light() + 
  theme(axis.text = element_text(colour="black", size=11)) + 
  scale_fill_manual(values = c("0" = "#E40010", "1" = "#00B32C"))
g5


## Density Plot - Training_hours & Target -> Big Differences  ???
g6 <- ggplot(data, aes(x = training_hours)) + 
  geom_density(aes(fill=target), alpha = 0.6) + 
  labs(title = "Wechselwilligkeit nach Trainingsstunden", 
       subtitle = "Kerndichteschätzer - Training_hours & Target", 
       x = "Trainingsstunden", y = "Dichte") + 
  #  scale_x_continuous(breaks=seq(20, 120, 20), lim = c(0, 140)) +
  theme_light() + 
  theme(axis.text = element_text(colour="black", size=11)) + 
  scale_fill_manual(values = c("0" = "#E40010", "1" = "#00B32C"))
g6


data_0 <- data[data$target == "0", ]
data_1 <- data[data$target == "1", ]

summary(data_0)
summary(data_1)


## NAs
library(naniar)
vis_miss(data)


## Correlation Matrix
#library(corrplot)
#corrplot(data_1, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 45)

#library(ggcorrplot)
#model.matrix(~0+., data_1) %>% 
#  cor(use="pairwise.complete.obs") %>% 
#  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)


## - Company Size & Experience

## Evtl. mit Tabelle 
# Für jede Kombination Anteil als Zahl und Einfärbung
# Inspiration: https://www.kaggle.com/code/joshuaswords/awesome-hr-data-visualization-prediction



given_order <- c("none", "small", "medium", "large")
data_1 <- data
data_1$company_size <- forcats::fct_relevel(data_1$company_size, given_order)

ggplot(data_1, aes(x = company_size)) +
  geom_bar(aes(fill = target), position = position_dodge2(preserve = "single")) +
  labs(title = "Wechselwilligkeit nach Unternehmensgröße",
       subtitle = "Säulendiagramm - Company_Size & Target",
       x = "Unternehmensgröße",
       y = "Häufigkeit") +
  theme_light() +
  theme(
    axis.text = element_text(colour = "black", size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("0" = "#E40010", "1" = "#00B32C"))



## Density Plot - CDI & Target -> Big Differences
g4 <- ggplot(data, aes(x = city_development_index)) + 
  geom_density(aes(fill=target), alpha = 0.6) + 
  labs(title = "Wechselwilligkeit nach CDI (City Development Index)", 
       subtitle = "Kerndichteschätzer - CDI & Target", 
       x = "CDI", y = "Dichte") + 
  #  scale_x_continuous(breaks=seq(20, 120, 20), lim = c(0, 140)) +
  theme_light() +
  theme(
    axis.text = element_text(colour = "black", size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("0" = "#E40010", "1" = "#00B32C"))
g4











## Possible Plots - No Target

  ### Bar Count Education Level
### Missing Data (Bar Chart oder Count als Bars)
### Company Size & Experience
# Bar / Density CDI
# Density training hours 
# Welche Dinge sonst interessant?


## Possible Plots - Correlation / Impact

### Correlation Matrix
  ### Bar Education Level (Bars next to eachother)
  ### CDI Density (colored by target)
  ### Experience Density (colored by target) ?
  ### Training Hours Density (colored by target)
### oder alle Kombinationen (entweder als Bar oder Density)


## TBD

# Color Coding
# Naming
# Andere Art von Plots (Corr.Matrix, Pie Chart, Bar Chart, Density, Table)
