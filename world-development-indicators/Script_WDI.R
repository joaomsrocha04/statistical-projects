# Captura todo o output de texto
sink("output.txt", split = TRUE)

# Captura todos os plots num PDF
pdf("plots.pdf", width = 11, height = 8.5)


# ANÁLISE MULTIVARIADA - WORLD DEVELOPMENT INDICATORS
# Métodos Estatísticos em Data Mining


# LIBRARIES
library(readxl)
library(cluster)
library(clusterCrit)
library(MASS)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(scales)
library(mclust)
library(GGally)
library(factoextra)
library(ellipse)
library(moments)    # skewness e kurtosis
library(tidyr)      # pivot_longer
library(rpart)      # CART
library(rpart.plot) # visualização da árvore
library(class)      # KNN
library(e1071)      # SVM



# IMPORTAR DADOS
dados <- read_excel("WBD.xlsx")


# SEPARAR VARIÁVEIS CATEGÓRICAS
dados_num <- dados[, !(names(dados) %in% c("Country", "Income group"))]
dados_num <- data.frame(lapply(dados_num, as.numeric))



# PRÉ-PROCESSAMENTO

# ANÁLISE EXPLORATÓRIA DOS ZEROS

# Trata temporariamente todos os zeros como NA para identificar
# variáveis com missings codificados como 0.

dados_temp <- dados_num
dados_temp[dados_temp == 0] <- NA

sort(colSums(is.na(dados_temp)), decreasing = TRUE)

par(mar = c(12, 4, 4, 2))
barplot(sort(colSums(is.na(dados_temp)), decreasing = TRUE),
        las = 2,
        main = "Zeros por variável (tratados como NA)")
par(mar = c(5, 4, 4, 2))




# ZEROS IMPOSSÍVEIS -> NA

vars_zero_impossivel <- c(
  "Access.to.electricity....of.population.",
  "GDP.per.capita..current.US..",
  "Individuals.using.the.Internet....of.population.",
  "Mobile.cellular.subscriptions..per.100.people.",
  "School.enrollment..primary....gross.",
  "Gross.capital.formation....of.GDP.",
  "Inflation..consumer.prices..annual...",
  "Exports.of.goods.and.services....of.GDP.",
  "Imports.of.goods.and.services....of.GDP.",
  "Agriculture..forestry..and.fishing..value.added....of.GDP.",
  "Industry..including.construction...value.added....of.GDP.",
  "Services..value.added....of.GDP.",
  "Total.greenhouse.gas.emissions.excluding.LULUCF.per.capita..t.CO2e.capita."
)

vars_zero_impossivel <- intersect(vars_zero_impossivel, names(dados_num))

for (v in vars_zero_impossivel) {
  dados_num[[v]][dados_num[[v]] == 0] <- NA
}




# MISSINGS

colSums(is.na(dados_num))

par(mar = c(12, 4, 4, 2))
barplot(colSums(is.na(dados_num)),
        las = 2,
        main = "Número de missings por variável")
par(mar = c(5, 4, 4, 2))





# IMPUTAÇÃO PELA MEDIANA

for (i in seq_along(dados_num)) {
  med <- median(dados_num[[i]], na.rm = TRUE)
  dados_num[[i]][is.na(dados_num[[i]])] <- med
}




# REMOVER VARIÁVEL REDUNDANTE
dados_num$Trade....of.GDP. <- NULL






# ANÁLISE UNIVARIADA

# Tabela resumo global 
tabela_univ <- data.frame(
  Media        = sapply(dados_num, mean),
  Media_trim5  = sapply(dados_num, mean, trim = 0.05),
  Mediana      = sapply(dados_num, median),
  Q1           = sapply(dados_num, quantile, 0.25),
  Q3           = sapply(dados_num, quantile, 0.75),
  Amplitude    = sapply(dados_num, function(x) diff(range(x))),
  IQR          = sapply(dados_num, IQR),
  Desv_Pad     = sapply(dados_num, sd),
  CV_pct       = sapply(dados_num, function(x) sd(x) / mean(x) * 100),
  Skewness     = sapply(dados_num, skewness),
  Kurtosis     = sapply(dados_num, kurtosis)
)

# CV não aplicável para variáveis que mudam de sinal
tabela_univ[vars_sem_cv, "CV_pct"] <- NA

cat("=== TABELA UNIVARIADA (todos os indicadores) ===\n")
print(round(tabela_univ, 3))

# Exportar para CSV
write.csv(round(tabela_univ, 3), "tabela_univariada.csv")


# --- Boxplots das 4 variáveis-chave ---
par(mfrow = c(2, 2))
boxplot(dados_num$GDP.per.capita..current.US..,
        main = "GDP per capita (USD)", col = "lightblue")
boxplot(dados_num$Life.expectancy.at.birth..total..years.,
        main = "Life expectancy (years)", col = "lightblue")
boxplot(dados_num$Fertility.rate..total..births.per.woman.,
        main = "Fertility rate", col = "lightblue")
boxplot(dados_num$Individuals.using.the.Internet....of.population.,
        main = "Internet users (%)", col = "lightblue")
par(mfrow = c(1, 1))

# --- Boxplots padronizados (todas as variáveis) ---
par(mar = c(12, 4, 4, 2))
boxplot(scale(dados_num),
        las = 2, cex.axis = 0.7,
        main = "Standardised boxplots (z-score)",
        col = "lightblue",
        outcol = "red", outpch = 19, outcex = 0.6)
par(mar = c(5, 4, 4, 2))
# Nota: outliers extremos em Inflation e Population density.
# GDP per capita com forte assimetria positiva.

# --- Tabela Internet ---
internet_cat <- cut(dados_num$Individuals.using.the.Internet....of.population.,
                    breaks = c(0,40,80,100), labels=c("Low","Medium","High"))
table(internet_cat, dados$`Income group`)
chisq.test(table(internet_cat, dados$`Income group`))


# --- Histogramas das variáveis-chave ---
par(mfrow = c(2, 3))
hist(dados_num$GDP.per.capita..current.US..,
     main = "GDP per capita", xlab = "", col = "lightblue", breaks = 20)
hist(dados_num$Life.expectancy.at.birth..total..years.,
     main = "Life Expectancy", xlab = "", col = "lightblue", breaks = 20)
hist(dados_num$Fertility.rate..total..births.per.woman.,
     main = "Fertility Rate", xlab = "", col = "lightblue", breaks = 20)
hist(dados_num$Individuals.using.the.Internet....of.population.,
     main = "Internet Users (%)", xlab = "", col = "lightblue", breaks = 20)
hist(dados_num$Inflation..consumer.prices..annual...,
     main = "Inflation (%)", xlab = "", col = "lightblue", breaks = 20)
hist(dados_num$Mortality.rate..under.5..per.1.000.live.births.,
     main = "Child Mortality", xlab = "", col = "lightblue", breaks = 20)
par(mfrow = c(1, 1))




# ANÁLISE BIVARIADA

# --- Identificar correlações altas (|r| > 0.8) ---
cor_mat_raw <- cor(dados_num)

cat("\n=== PARES COM |CORRELAÇÃO| > 0.8 ===\n")
cors_altas <- which(abs(cor_mat_raw) > 0.8 & cor_mat_raw != 1,
                    arr.ind = TRUE)
for (i in seq_len(nrow(cors_altas))) {
  r <- cor_mat_raw[cors_altas[i, 1], cors_altas[i, 2]]
  if (cors_altas[i, 1] < cors_altas[i, 2]) {
    cat(colnames(cor_mat_raw)[cors_altas[i, 1]], "vs",
        colnames(cor_mat_raw)[cors_altas[i, 2]], ":",
        round(r, 2), "\n")
  }
}

# --- Heatmap das correlações ---
encurtar <- function(x, n = 22) {
  x <- gsub("\\.+", " ", x)
  x <- gsub("of GDP", "%GDP", x)
  x <- gsub("of population", "%pop", x)
  x <- gsub("of total", "%total", x)
  x <- gsub("annual", "", x)
  x <- gsub("total", "", x, ignore.case = TRUE)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "."), x)
}

nomes_curtos <- encurtar(colnames(cor_mat_raw), n = 22)
nomes_curtos <- make.unique(nomes_curtos, sep = "_")
cor_mat_plot <- cor_mat_raw
colnames(cor_mat_plot) <- rownames(cor_mat_plot) <- nomes_curtos

p_heat <- ggcorrplot(cor_mat_plot,
                     type          = "upper",
                     show.diag     = TRUE,
                     lab           = FALSE,
                     hc.order      = TRUE,
                     outline.color = "white",
                     title         = "Correlation heatmap",
                     ggtheme       = theme_minimal()) +
  scale_fill_gradientn(
    colours = c("red", "orange", "#F3F3B2", "lightgreen", "green4"),
    values  = rescale(c(-1, -0.5, 0, 0.5, 1)),
    limits  = c(-1, 1),
    name    = "Correlation"
  ) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 8),
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    panel.grid   = element_blank()
  )

print(p_heat)
ggsave("heatmap_corr.png", plot = p_heat,
       width = 12, height = 10, dpi = 300)

# --- Boxplots de variáveis-chave por Income Group ---
income_factor_biv <- dados$`Income group`

vars_biv <- c("GDP.per.capita..current.US..",
              "Life.expectancy.at.birth..total..years.",
              "Fertility.rate..total..births.per.woman.",
              "Individuals.using.the.Internet....of.population.")

labels_biv <- c("GDP per capita (USD)",
                "Life expectancy (years)",
                "Fertility rate",
                "Internet users (%)")

par(mfrow = c(2, 2))
for (i in seq_along(vars_biv)) {
  boxplot(dados_num[[vars_biv[i]]] ~ income_factor_biv,
          main  = labels_biv[i],
          xlab  = "",
          ylab  = "",
          col   = c("tomato", "orange", "lightgreen", "steelblue"),
          las   = 2, cex.axis = 0.75)
}
par(mfrow = c(1, 1))

# --- Testes de Kruskal-Wallis (comparação de medianas entre grupos) ---
# Não paramétrico: adequado dada a assimetria das variáveis.
cat("\n=== TESTES DE KRUSKAL-WALLIS (por Income Group) ===\n")
vars_kw <- c("GDP.per.capita..current.US..",
             "Life.expectancy.at.birth..total..years.",
             "Fertility.rate..total..births.per.woman.",
             "Mortality.rate..under.5..per.1.000.live.births.",
             "Individuals.using.the.Internet....of.population.",
             "Access.to.electricity....of.population.")

for (v in vars_kw) {
  kt <- kruskal.test(dados_num[[v]] ~ dados$`Income group`)
  cat(v, ": p-value =", format(kt$p.value, digits = 3), "\n")
}





# ANÁLISE MULTIVARIADA

# NORMALIZAÇÃO 
dados_scaled <- scale(dados_num)



# PCA — ANÁLISE EM COMPONENTES PRINCIPAIS

x <- princomp(dados_num, cor = TRUE, scores = TRUE)
summary(x)

# --- Scree plot ---
plot(x, main = "Variance per component (Scree Plot)")

# --- Critérios para número de componentes a reter ---
variancias <- x$sdev^2
prop_var   <- variancias / sum(variancias)

# 1) Critério de Pearson: 80% da variância acumulada
n_pcs_pearson <- which(cumsum(prop_var) >= 0.8)[1]
cat("\nCritério de Pearson (80%): reter", n_pcs_pearson, "componentes\n")

# 2) Elbow Method
# o cotovelo está entre PC2 e PC3

# 3) Critério de Kaiser: eigenvalues > 1 
n_pcs_kaiser <- sum(variancias > 1)
cat("Critério de Kaiser (eigenvalue > 1): reter", n_pcs_kaiser, "componentes\n")

# Conclusão: Pearson sugere 10 componentes; Kaiser sugere 7.
# Usaremos 10 (Pearson) para clustering e QDA, sendo mais conservador.
n_pcs_80 <- n_pcs_pearson

# --- Scree plot detalhado ---
plot(prop_var,
     type = "b", pch = 19,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Scree Plot")

plot(cumsum(prop_var),
     type = "b", pch = 19,
     ylim = c(0, 1),
     xlab = "Number of Components",
     ylab = "Cumulative Variance",
     main = "Cumulative Variance")
abline(h = 0.8, col = "red", lty = 2)
abline(v = n_pcs_pearson, col = "red",  lty = 2)
abline(v = n_pcs_kaiser,  col = "blue", lty = 2)
legend("bottomright",
       legend = c("80% (Pearson)", "Kaiser"),
       col    = c("red", "blue"),
       lty    = 2, bty = "n")

# --- Scores PC1 vs PC2 ---
plot(x$scores[, 1:2],
     xlab = "PC1 - Human Development (33.07%)",
     ylab = "PC2 - Productive Structure (8.33%)",
     main = "Scores PC1 vs PC2",
     pch = 19, col = "grey60", cex = 0.8)

outliers <- which(abs(x$scores[, 1]) > 5 | abs(x$scores[, 2]) > 4)
text(x$scores[outliers, 1],
     x$scores[outliers, 2],
     labels = dados$Country[outliers],
     cex = 0.55, pos = 3, col = "darkred")
abline(h = 0, v = 0, col = "grey80", lty = 2)

cat("\nOutliers extremos (|PC1| > 5):\n")
print(dados$Country[which(abs(x$scores[, 1]) > 5)])

# --- Loadings ---
sort(x$loadings[, 1], decreasing = TRUE)
sort(x$loadings[, 2], decreasing = TRUE)

# PC1 — "Eixo de Desenvolvimento Humano"
# PC2 — "Eixo de Estrutura Produtiva"


par(mar = c(12, 4, 4, 2))
barplot(sort(abs(x$loadings[, 1]), decreasing = TRUE),
        las = 2,
        main = "Variable importance on PC1 (Human Development)")
barplot(sort(abs(x$loadings[, 2]), decreasing = TRUE),
        las = 2,
        main = "Variable importance on PC2 (Productive Structure)")
par(mar = c(5, 4, 4, 2))

# --- Correlações das variáveis com PC1 e PC2 ---
cor_vars_pc <- cor(dados_num, x$scores[, 1:2])
cat("\nTop correlações com PC1:\n")
print(round(sort(cor_vars_pc[, 1], decreasing = TRUE), 3))
cat("\nTop correlações com PC2:\n")
print(round(sort(cor_vars_pc[, 2], decreasing = TRUE), 3))

# --- Biplot ---
income_factor <- factor(dados$`Income group`,
                        levels = c("Low income",
                                   "Lower middle income",
                                   "Upper middle income",
                                   "High income"))

biplot <- fviz_pca_biplot(
  x,
  geom.ind       = "point",
  fill.ind       = income_factor,
  col.ind        = "black",
  pointshape     = 21,
  pointsize      = 2.5,
  alpha.ind      = 0.8,
  addEllipses    = TRUE,
  ellipse.type   = "confidence",
  ellipse.level  = 0.95,
  ellipse.alpha  = 0.10,
  col.var        = "contrib",
  gradient.cols  = c("grey60", "orange", "red3"),
  select.var     = list(contrib = 7),
  arrowsize      = 0.6,
  labelsize      = 3,
  repel          = TRUE,
  legend.title   = list(fill = "Income Group", color = "Contribution"),
  title          = "Biplot - PCA (PC1 vs PC2)"
) +
  ggsci::scale_fill_jco() +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

print(biplot)
ggsave("biplot_pca.png", plot = biplot, width = 13, height = 9, dpi = 300)

# --- PCA colorido por Income Group ---
income_factor_num <- as.factor(dados$`Income group`)
cores <- c("black", "red", "green3", "blue")

par(mar = c(5, 4, 4, 10), xpd = TRUE)
plot(x$scores[, 1:2],
     col  = cores[as.numeric(income_factor_num)],
     pch  = 19, cex = 0.8,
     xlab = "PC1 - Human Development (33.07%)",
     ylab = "PC2 - Productive Structure (8.33%)",
     main = "PCA by Income Group")

for (g in seq_along(levels(income_factor_num))) {
  idx <- which(income_factor_num == levels(income_factor_num)[g])
  if (length(idx) > 2) {
    centro <- colMeans(x$scores[idx, 1:2])
    cov_g  <- cov(x$scores[idx, 1:2])
    lines(ellipse(cov_g, centre = centro, level = 0.68),
          col = cores[g], lwd = 2)
  }
}
abline(h = 0, v = 0, col = "grey80", lty = 2)
legend("topright", inset = c(-0.35, 0),
       legend = levels(income_factor_num),
       col = cores, pch = 19, bty = "n")
par(mar = c(5, 4, 4, 2), xpd = FALSE)



# CLUSTERING


# K-MEANS — ESCOLHA DE K

# --- Elbow (WSS) ---
set.seed(123)
wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(dados_scaled, centers = k, nstart = 10)$withinss)
}
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of clusters", ylab = "Within Sum of Squares",
     main = "Elbow Method")

# --- Calinski-Harabasz ---
set.seed(123)
ch_values <- rep(NA, 10)
for (k in 2:10) {
  km <- kmeans(dados_scaled, centers = k, nstart = 20)
  ch <- intCriteria(as.matrix(dados_scaled),
                    as.integer(km$cluster),
                    "Calinski_Harabasz")
  ch_values[k] <- ch$calinski_harabasz
}
plot(2:10, ch_values[2:10], type = "b", pch = 19,
     xlab = "Number of clusters", ylab = "Calinski-Harabasz",
     main = "Calinski-Harabasz Criterion")

# K=2 é o "máximo" apenas por ser o mínimo testado.

# --- Silhouette ---
set.seed(123)
sil_values <- rep(NA, 10)
for (k in 2:10) {
  km  <- kmeans(dados_scaled, centers = k, nstart = 20)
  sil <- silhouette(km$cluster, dist(dados_scaled))
  sil_values[k] <- mean(sil[, 3])
}
plot(2:10, sil_values[2:10], type = "b", pch = 19,
     xlab = "Number of clusters", ylab = "Average silhouette width",
     main = "Silhouette Method")

cat("\nK sugerido por Calinski-Harabasz:", which.max(ch_values[2:10]) + 1, "\n")
cat("K sugerido por Silhouette:        ", which.max(sil_values[2:10]) + 1, "\n")
# Ambos sugerem K=2 formalmente. Testamos também K=4
# por corresponder aos 4 Income Groups reais.



# K-MEANS COM K = 2
set.seed(123)
kmeans_2 <- kmeans(dados_scaled, centers = 2, nstart = 25)

plot(x$scores[, 1:2],
     col = kmeans_2$cluster, pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "K-Means with K = 2")
legend("topright", legend = c("Cluster 1", "Cluster 2"),
       col = 1:2, pch = 19, bty = "n")

cat("\nTabela K-Means K=2 vs Income Group:\n")
print(table(kmeans_2$cluster, dados$`Income group`))
# K=2 separa países ricos (High + Upper middle) de pobres
# (Low + Lower middle) — o gradiente principal do PC1.


# K-MEANS COM K = 4
set.seed(123)
kmeans_res <- kmeans(dados_scaled, centers = 4, nstart = 25)

plot(x$scores[, 1:2],
     col = kmeans_res$cluster, pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "K-Means with K = 4")
legend("topright", legend = paste("Cluster", 1:4),
       col = 1:4, pch = 19, bty = "n")

cat("\nTabela K-Means K=4 vs Income Group:\n")
print(table(kmeans_res$cluster, dados$`Income group`))

cat("\nMédias por cluster:\n")
print(round(aggregate(dados_num,
                      by = list(cluster = kmeans_res$cluster),
                      mean), 2))

# --- Silhouette K=4 ---
sil_final <- silhouette(kmeans_res$cluster, dist(dados_scaled))
plot(sil_final, main = "Silhouette K = 4")

ari_km4 <- adjustedRandIndex(kmeans_res$cluster, dados$`Income group`)
cat("\nARI K-Means K=4 vs Income Group:", round(ari_km4, 3), "\n")

# Silhouette médio ~0.13 (fraco, < 0.25): os clusters K=4 não
# são bem separados.



# K-MEANS NAS PCs (redução de dimensionalidade)
# Aplicado nas 10 primeiras PCs: elimina variáveis redundantes
# e reduz ruído.

spc <- x$scores[, 1:n_pcs_80]

set.seed(123)
cl_pca <- kmeans(spc, centers = 4, nstart = 20)

plot(spc[, 1:2],
     col = cl_pca$cluster, pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "K-Means on PCs (K=4)")

ari_km_pca <- adjustedRandIndex(cl_pca$cluster, dados$`Income group`)
cat("ARI K-Means nas PCs vs Income Group:", round(ari_km_pca, 3), "\n")

# --- Comparação visual: Income Group real vs K-Means ---
par(mfrow = c(1, 2))
plot(x$scores[, 1:2],
     col = as.numeric(income_factor_num), pch = 19,
     main = "Income Group (real)", xlab = "PC1", ylab = "PC2")
plot(x$scores[, 1:2],
     col = kmeans_res$cluster, pch = 19,
     main = "K-Means K=4", xlab = "PC1", ylab = "PC2")
par(mfrow = c(1, 1))
# O K-Means isola um cluster muito pequeno com os outliers
# extremos (Singapore, Hong Kong, Macao, Monaco) — economias-cidade
# ultra-densas. É um cluster real mas pequeno demais para ter
# valor substantivo na análise dos Income Groups.

# --- Scatterplot matrix das primeiras 4 PCs ---
GGally::ggpairs(as.data.frame(spc[, 1:min(4, ncol(spc))]))



# MCLUST — MISTURA DE GAUSSIANAS
# Estima o número de clusters e as covariâncias por cluster
# via algoritmo EM (Expectation-Maximization).

mc <- Mclust(dados_scaled, G = 1:9)
summary(mc)
plot(mc, what = "BIC")

ari_mc <- adjustedRandIndex(mc$classification, dados$`Income group`)
cat("\nARI Mclust vs Income Group:", round(ari_mc, 3), "\n")

# Mclust seleccionou 4 componentes com modelo VVE (elipsoidal,
# volume e forma variáveis, orientação igual). ARI = 0.150:
# inferior aos métodos baseados em distância (K-Means, Ward.D2).
# As distribuições por grupo não são suficientemente gaussianas
# para a mistura recuperar bem os Income Groups, em particular pela
# forte assimetria de GDP per capita e Inflation.



# CLUSTERING HIERÁRQUICO

# - Complete linkage: sensível a outliers, tende a colapsar
#   observações num mega-cluster.
# - Ward.D2: minimiza variância intra-cluster, mais robusto.

dist_mat <- dist(dados_scaled)
hc        <- hclust(dist_mat, method = "complete")
hc_ward   <- hclust(dist_mat, method = "ward.D2")

par(mfrow = c(1, 2))
plot(hc,      labels = FALSE, main = "Dendrogram - Complete linkage")
rect.hclust(hc, k = 4, border = "red")
plot(hc_ward, labels = FALSE, main = "Dendrogram - Ward.D2")
rect.hclust(hc_ward, k = 4, border = "red")
par(mfrow = c(1, 1))

grupos      <- cutree(hc,      k = 4)
grupos_ward <- cutree(hc_ward, k = 4)

par(mfrow = c(1, 2))
plot(x$scores[, 1:2], col = grupos, pch = 19,
     main = "Hierarchical - Complete", xlab = "PC1", ylab = "PC2")
plot(x$scores[, 1:2], col = grupos_ward, pch = 19,
     main = "Hierarchical - Ward.D2",   xlab = "PC1", ylab = "PC2")
par(mfrow = c(1, 1))

ari_hc      <- adjustedRandIndex(grupos,      dados$`Income group`)
ari_hc_ward <- adjustedRandIndex(grupos_ward, dados$`Income group`)
cat("\nARI Hierárquico Complete vs Income Group:", round(ari_hc,      3), "\n")
cat("ARI Hierárquico Ward.D2  vs Income Group:", round(ari_hc_ward, 3), "\n")

# Complete: ARI ≈ 0.092 (fraco) — colapso quase total num
# mega-cluster por influência dos outliers extremos.
# Ward.D2: resultado substancialmente melhor.

cat("\nTabela Ward.D2 vs Income Group:\n")
print(table(grupos_ward, dados$`Income group`))

# --- Dendrograma das variáveis ---
par(mar = c(8, 4, 4, 2))
hc_var <- hclust(dist(t(dados_scaled)))
plot(hc_var, main = "Dendrogram of Variables", cex = 0.7)
par(mar = c(5, 4, 4, 2))
# Confirma estrutura do PCA:
# fertility/mortality/pop-0-14 agrupam juntos (polo subdesenvolvimento)
# internet/electricity/life-expectancy agrupam juntos (polo desenvolvimento)



# MÉTODOS SUPERVISIONADOS

# PRESSUPOSTO: NORMALIDADE MULTIVARIADA 

# LDA e QDA assumem distribuição normal multivariada por classe.
# Diagnóstico via QQ-plot das distâncias de Mahalanobis.

n_obs <- nrow(dados_scaled)
p_var <- ncol(dados_scaled)
mu_hat <- colMeans(dados_scaled)
S_hat  <- cov(dados_scaled)
d2     <- mahalanobis(dados_scaled, center = mu_hat, cov = S_hat)

qqplot(qchisq(ppoints(n_obs), df = p_var), d2,
       xlab = "Theoretical Chi-squared quantiles",
       ylab = "Squared Mahalanobis distances",
       main = "QQ-plot - Multivariate Normality Diagnostic")
abline(0, 1, col = "red", lty = 2)

# Desvios na cauda superior indicam outliers multivariados e
# caudas pesadas — ambos presentes neste dataset.
# A normalidade multivariada é uma aproximação; o LDA é
# razoavelmente robusto a desvios moderados desta premissa.




# TRAIN / TEST SPLIT (70% / 30%)
set.seed(123)
train_id <- sample(seq_len(nrow(dados_scaled)),
                   size = floor(0.7 * nrow(dados_scaled)))

train   <- as.data.frame(dados_scaled[train_id, ])
test    <- as.data.frame(dados_scaled[-train_id, ])
train_y <- dados$`Income group`[train_id]
test_y  <- dados$`Income group`[-train_id]



# ENQUADRAMENTO: TEORIA DE DECISÃO ESTATÍSTICA 

# LDA
lda_train <- data.frame(train, Income = train_y)
lda_model <- lda(Income ~ ., data = lda_train)
lda_model

# --- Proporção de trace por função discriminante ---
prop_trace <- lda_model$svd^2 / sum(lda_model$svd^2)
cat("\nProporção de trace por função discriminante:\n")
print(round(prop_trace, 3))
# LD1 explica 77,5% da discriminância total.
# LD2 explica 19,2%.

# --- Previsões ---
pred_lda <- predict(lda_model, newdata = test)

conf_lda <- table(Predito = pred_lda$class, Real = test_y)
cat("\nMatriz de confusão LDA:\n")
print(conf_lda)

acc_lda <- mean(pred_lda$class == test_y)
cat("Accuracy LDA:", round(acc_lda, 3), "\n")

# --- Visualização com legenda ---
cores_lda   <- c("black", "red", "green3", "blue")
grupos_test <- as.numeric(as.factor(test_y))

plot(pred_lda$x[, 1:2],
     col = cores_lda[grupos_test], pch = 19,
     xlab = "LD1 (77.5% of discrimination)",
     ylab = "LD2 (19.2% of discrimination)",
     main = "LDA - test set projection")
legend("topright",
       legend = levels(as.factor(test_y)),
       col    = cores_lda,
       pch    = 19, bty = "n", cex = 0.8)
abline(h = 0, v = 0, col = "grey80", lty = 2)

# LD1 separa claramente países de baixo rendimento (esquerda)
# de países de alto rendimento (direita).
# LD2 ajuda a separar Lower middle de Upper middle income.
# Erros concentram-se em categorias adjacentes (Upper middle e
# High income) 



# QDA — QUADRATIC DISCRIMINANT ANALYSIS (nas PCs)
# QDA estima uma matriz de covariância por classe - fronteiras
# quadráticas. Com muitas variáveis e poucas observações por grupo,
# é mais robusto aplicar QDA sobre as PCs

pc_data        <- as.data.frame(x$scores[, 1:n_pcs_80])
pc_data$Income <- dados$`Income group`

train_pc <- pc_data[train_id, ]
test_pc  <- pc_data[-train_id, ]

cat("\nDimensão mínima por classe (treino):\n")
print(table(train_pc$Income))

qda_model <- qda(Income ~ ., data = train_pc)
qda_model

pred_qda <- predict(qda_model, newdata = test_pc)

conf_qda <- table(Predito = pred_qda$class, Real = test_pc$Income)
cat("\nMatriz de confusão QDA:\n")
print(conf_qda)

acc_qda <- mean(pred_qda$class == test_pc$Income)
cat("Accuracy QDA:", round(acc_qda, 3), "\n")

# --- Visualização nas duas primeiras PCs ---
plot(test_pc$Comp.1, test_pc$Comp.2,
     col = as.numeric(as.factor(pred_qda$class)), pch = 19,
     xlab = "PC1 - Human Development",
     ylab = "PC2 - Productive Structure",
     main = "QDA - test set predictions (PC space)")
legend("topright",
       legend = levels(as.factor(pred_qda$class)),
       col    = 1:4, pch = 19, bty = "n", cex = 0.8)




# CART — ÁRVORE DE CLASSIFICAÇÃO 
train_cart <- data.frame(train, Income = as.factor(train_y))
test_cart  <- data.frame(test,  Income = as.factor(test_y))

# --- Construção da árvore ---
# method="class" para classificação; Gini é o critério por defeito
set.seed(123)
cart_model <- rpart(Income ~ ., data = train_cart,
                    method  = "class",
                    parms   = list(split = "gini"),
                    control = rpart.control(cp = 0.01, minsplit = 10))

# --- Visualizar árvore antes da poda ---
rpart.plot(cart_model,
           main = "CART Tree (before pruning)",
           type = 2, extra = 104, fallen.leaves = TRUE,
           cex = 0.7)

# --- Tabela de complexidade ---
cat("\n=== TABELA DE COMPLEXIDADE CART ===\n")
printcp(cart_model)
plotcp(cart_model, main = "Cost-Complexity - cp selection")

# --- Poda pelo cp óptimo (menor xerror) ---
cp_opt <- cart_model$cptable[which.min(cart_model$cptable[, "xerror"]), "CP"]
cat("\ncp óptimo:", round(cp_opt, 4), "\n")
cart_pruned <- prune(cart_model, cp = cp_opt)

rpart.plot(cart_pruned,
           main = "CART Tree (after pruning)",
           type = 2, extra = 104, fallen.leaves = TRUE,
           cex = 0.75)

# --- Previsões ---
pred_cart <- predict(cart_pruned, newdata = test_cart, type = "class")

conf_cart <- table(Predito = pred_cart, Real = test_y)
cat("\nMatriz de confusão CART:\n")
print(conf_cart)

acc_cart <- mean(pred_cart == test_y)
cat("Accuracy CART:", round(acc_cart, 3), "\n")

# --- Variáveis mais importantes na árvore ---
cat("\nImportância das variáveis (CART) — top 5:\n")
print(round(sort(cart_pruned$variable.importance, decreasing = TRUE)[1:5], 2))
# A árvore podada divide exclusivamente pelo GDP per capita
# (três cortes sucessivos). As restantes variáveis na lista de
# importância são surrogates correlacionados, não usados nos splits.



# KNN — K VIZINHOS MAIS PRÓXIMOS 
# Método não paramétrico: classifica cada observação pela classe
# maioritária dos seus K vizinhos mais próximos.

# K: sqrt(n_treino)
k_heur <- round(sqrt(nrow(train)))
cat("\nK heurístico (sqrt(n)):", k_heur, "\n")

# --- Escolha do K óptimo por cross validation dentro do treino ---
set.seed(123)
knn_tune <- tune.knn(x = as.matrix(train),
                     y = as.factor(train_y),
                     k = 1:20,
                     tunecontrol = tune.control(sampling = "cross",
                                                cross = 10))

cat("\nResultado do tuning KNN (10-fold CV no treino):\n")
print(knn_tune$best.parameters)

# Curva de erro de validação cruzada em função de K
plot(knn_tune$performances$k,
     knn_tune$performances$error,
     type = "b", pch = 19,
     xlab = "K (number of neighbours)",
     ylab = "Cross-validation error (10-fold)",
     main = "KNN - K selection by cross-validation")
abline(v = k_heur, col = "red", lty = 2)
legend("topright", legend = "Heuristic K = sqrt(n)",
       col = "red", lty = 2, bty = "n")

k_opt <- knn_tune$best.parameters$k
cat("K ótimo (validação cruzada no treino):", k_opt, "\n")

# --- Previsões no test set com o K escolhido ---
set.seed(123)
pred_knn <- knn(train = train, test = test, cl = train_y, k = k_opt)

conf_knn <- table(Predito = pred_knn, Real = test_y)
cat("\nMatriz de confusão KNN (K =", k_opt, "):\n")
print(conf_knn)

acc_knn <- mean(pred_knn == test_y)
cat("Accuracy KNN:", round(acc_knn, 3), "\n")




# SVM — SUPPORT VECTOR MACHINE 

# Maximiza a margem entre classes.
# Testamos kernel linear e kernel radial (RBF)

train_svm <- data.frame(train, Income = as.factor(train_y))
test_svm  <- data.frame(test,  Income = as.factor(test_y))

# --- SVM linear ---
set.seed(123)
svm_lin <- svm(Income ~ ., data = train_svm,
               kernel = "linear", cost = 1, scale = FALSE)
pred_svm_lin <- predict(svm_lin, newdata = test_svm)

conf_svm_lin <- table(Predito = pred_svm_lin, Real = test_y)
cat("\nMatriz de confusão SVM linear:\n")
print(conf_svm_lin)

acc_svm_lin <- mean(pred_svm_lin == test_y)
cat("Accuracy SVM linear:", round(acc_svm_lin, 3), "\n")

# --- SVM com kernel RBF (radial) ---
# Tuning de gamma e cost por validação cruzada
set.seed(123)
svm_tune <- tune(svm, Income ~ ., data = train_svm,
                 kernel = "radial",
                 ranges = list(cost  = c(0.1, 1, 10),
                               gamma = c(0.01, 0.1, 1)),
                 scale = FALSE)

cat("\nMelhores parâmetros SVM RBF:\n")
print(svm_tune$best.parameters)

svm_rbf <- svm_tune$best.model
pred_svm_rbf <- predict(svm_rbf, newdata = test_svm)

conf_svm_rbf <- table(Predito = pred_svm_rbf, Real = test_y)
cat("\nMatriz de confusão SVM RBF:\n")
print(conf_svm_rbf)

acc_svm_rbf <- mean(pred_svm_rbf == test_y)
cat("Accuracy SVM RBF:", round(acc_svm_rbf, 3), "\n")




# RESUMO COMPARATIVO
cat("\n")
cat("=================================================\n")
cat("RESUMO COMPARATIVO — TODOS OS MÉTODOS\n")
cat("=================================================\n\n")

cat("--- CLASSIFICAÇÃO SUPERVISIONADA (test accuracy) ---\n")
cat("LDA (variáveis originais):", round(acc_lda,     3), "\n")
cat("QDA (nas PCs):            ", round(acc_qda,     3), "\n")
cat("CART (árvore podada):     ", round(acc_cart,    3), "\n")
cat("KNN (K =", k_opt, "):       ", round(acc_knn,     3), "\n")
cat("SVM linear:               ", round(acc_svm_lin, 3), "\n")
cat("SVM RBF (tuned):          ", round(acc_svm_rbf, 3), "\n\n")

cat("--- CLUSTERING (ARI vs Income Group) ---\n")
cat("K-Means K=4 (var. originais):", round(ari_km4,      3), "\n")
cat("K-Means K=4 (nas PCs):       ", round(ari_km_pca,   3), "\n")
cat("Hierárquico Complete:        ", round(ari_hc,        3), "\n")
cat("Hierárquico Ward.D2:         ", round(ari_hc_ward,   3), "\n")
cat("Mclust (mistura gaussiana):  ", round(ari_mc,        3), "\n")

dev.off()   # fecha o PDF
sink()      # fecha o ficheiro de texto
