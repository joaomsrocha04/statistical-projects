crime <- read.csv("crimeus.csv")


names(crime)
library(ggplot2)
ggplot(crime, aes(x = Weapon, y= State [2:48,]) +
  geom_point() + 
  labs(
    title = "Distribuição dos Tipos de Crime nos EUA",
    x = "Tipo de Crime",
    y = "Estado da Ocorrência do Crime"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




crime_clean <- subset(crime, Victim.Age > 0 & Victim.Age <= 100)
ggplot(crime_clean, aes(x = Crime.Type, y = Victim.Age, fill = Crime.Type)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Distribuição da Idade das Vítimas por Tipo de Crime",
    x = "Tipo de Crime",
    y = "Idade da Vítima"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


crime <- subset(crime, crime$Perpetrator.Age>0)

set.seed(124)
amostra <- crime[sample(1:nrow(crime), 2000), ]  

ggplot(crime, aes(x = Victim.Age, y = Perpetrator.Age)) +
  geom_point(alpha = 0.4, color = "#4575b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relação entre Idade da Vítima e do Perpetrador",
    x = "Idade da Vítima",
    y = "Idade do Perpetrador"
  ) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_minimal()

library(dplyr)
crime %>%
  count(Relationship, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(Relationship, n), y = n)) +
  geom_col(fill = "#4575b4") +
  coord_flip() +
  labs(
    title = "Most Common Victim–Offender Relationships",
    x = "Relationship",
    y = "Number of Cases"
  ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 30, 10, 10))


library(tidyverse)
library(scales)

crime %>%
  count(Relationship, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(Relationship, n), y = n)) +
  geom_col(fill = "#4575b4") +  # cor fixa
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 4, color = "black") +
  coord_flip() +
  labs(
    title = "Top 10 Victim–Offender Relationships",
    subtitle = "Number of cases in descending order",
    x = "Relationship",
    y = "Number of Cases"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))



crime %>%
  filter(!is.na(Victim.Age) & Victim.Age <= 100) %>%
  ggplot(aes(x = reorder(Relationship, Victim.Age, FUN = median), y = Victim.Age, fill = Relationship)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(
    title = "Victim Age Distribution by Relationship",
    x = "Relationship",
    y = "Victim Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_brewer(palette = "Set3")



library(viridis)

crime %>%
  filter(!is.na(Victim.Age) & Victim.Age <= 100) %>%
  ggplot(aes(x = State, y = Victim.Age, fill = State)) +
  geom_boxplot() +
  labs(
    title = "Victim Age Distribution by State",
    x = "State",
    y = "Victim Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_viridis_d(option = "turbo")  



crime %>%
  count(State, Relationship) %>%
  ggplot(aes(x=Relationship, y=State, size=n, fill=n)) +
  geom_point(shape=21, color="black", alpha=0.7) +
  scale_size_continuous(range=c(3,15)) +
  scale_fill_viridis_c() +
  labs(title="Cases by Relationship and State", x="Relationship", y="State", size="Cases", fill="Cases") +
  theme_minimal(base_size=12) +
  theme(axis.text.x = element_text(angle=45, hjust=1))


crime %>%
  filter(!is.na(Victim.Age) & Victim.Age <= 100) %>%
  group_by(Relationship) %>%
  summarise(mean_age = mean(Victim.Age), .groups="drop") %>%
  slice_max(mean_age, n=10) %>%
  ggplot(aes(x=reorder(Relationship, mean_age), y=mean_age)) +
  geom_segment(aes(x=Relationship, xend=Relationship, y=0, yend=mean_age), color="#4575b4") +
  geom_point(color="#d73027", size=6) +
  coord_flip() +
  labs(title="Average Victim Age by Relationship", x="Relationship", y="Mean Age") +
  theme_minimal(base_size=12)

install.packages("remotes")      
remotes::install_github("hrbrmstr/waffle")

library(waffle)

crime %>%
  count(State, Relationship) %>%
  group_by(State) %>%
  mutate(prop = n / sum(n)) %>%
  filter(prop > 0.1) %>% 
  ggplot(aes(x=State, y=prop, fill=Relationship)) +
  geom_col() +
  labs(title="Major Relationships per State (Proportion >10%)", x="State", y="Proportion") +
  coord_flip() +
  theme_minimal(base_size=12)


crime %>%
  filter(!is.na(Victim.Age) & Victim.Age <= 100) %>%
  group_by(State, Relationship) %>%
  summarise(mean_age = mean(Victim.Age), n = n(), .groups="drop") %>%
  ggplot(aes(x=Relationship, y=mean_age, color=State, size=n)) +
  geom_point(alpha=0.8) +
  theme_minimal(base_size=12) +
  labs(title="Mean Victim Age by Relationship & State (Bubble Size = Cases)", x="Relationship", y="Mean Age", color="State", size="Cases") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


library(treemapify)

crime %>%
  count(State, Relationship) %>%
  ggplot(aes(area=n, fill=Relationship, label=paste(Relationship, n), subgroup=State)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place="centre", grow=TRUE, alpha=0.5) +
  geom_treemap_text(color="white", place="centre") +
  labs(title="Treemap of Relationships by State") +
  theme(legend.position="none")


crime %>%
  count(Relationship) %>%
  ggplot(aes(x=reorder(Relationship, n), y=n, fill=n)) +
  geom_col() +
  coord_polar(start=0) +
  labs(title="Circular Barplot of Relationships", x="", y="Cases") +
  theme_minimal() +
  scale_fill_viridis_c()


library(ggbeeswarm)

crime %>%
  filter(!is.na(Victim.Age) & Victim.Age <=100) %>%
  ggplot(aes(x=Relationship, y=Victim.Age, color=Relationship)) +
  geom_quasirandom(alpha=0.6) +
  coord_flip() +
  labs(title="Beeswarm Plot of Victim Age by Relationship", x="Relationship", y="Victim Age") +
  theme_minimal(base_size=12)








 
