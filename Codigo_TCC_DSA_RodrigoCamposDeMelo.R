install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("htmlwidgets")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("tidyverse", dependencies = TRUE)
install.packages("RColorBrewer")
install.packages("viridis", dependencies = TRUE)
install.packages("viridisLite", dependencies = TRUE)
install.packages("readxl")
install.packages("kableExtra", dependencies = TRUE)
install.packages("geobr", dependencies = TRUE)
if (!requireNamespace("spdep", quietly = TRUE)) {
  install.packages("spdep")
}
install.packages("sf", dependencies = TRUE)
install.packages("geobr", dependencies = TRUE)
install.packages("dplyr")
install.packages("readr")
install.packages("sf", dependencies = TRUE)
remotes::install_github("ddsjoberg/gtsummary")
install.packages("ggspatial")
install.packages("tmap")
install.packages("janitor")
install.packages("sp")
install.packages("forcats", dependencies = TRUE)
library(forcats)
library(janitor)
library(tmap)
library(ggspatial)
library(sf)
library(readr)
library(viridis)
library(dplyr)
library(RColorBrewer)
library(magrittr)
library(leaflet.extras)
library(leaflet)
library(htmlwidgets)
library(tidyverse)
library(readxl)
library(ggplot2)
library(kableExtra)
library(spdep)

# Atualizar todos os pacotes instalados
update.packages(ask = FALSE)
## citation(package = "spdep") # referencia de um pacote


# Leitura de base de dados ANATEL com informações de Junho de 2024
ANATEL_JUNHO_2024 <- read_excel("C:/Users/Rodrigo/Documentos_SDD/MBA_USP/TCC/Dados/BASE_ANATEL_JUNHO_2024.xlsx")

# Ver as primeiras linhas dos dados
head(ANATEL_JUNHO_2024)

# Ver a estrutura dos dados
str(ANATEL_JUNHO_2024)

# Selecionar todas as colunas exceto as que quer excluir
ANATEL_JUNHO_2024_SELECT <- ANATEL_JUNHO_2024 %>%
  select(-c("% moradores cobertos", "% domicílios cobertos", "Área km2", "Moradores", "Domicílios"))

# Ver as primeiras linhas dos dados
head(ANATEL_JUNHO_2024_SELECT)

# Ver a estrutura dos dados
str(ANATEL_JUNHO_2024_SELECT)

# Verifica nomes das colunas
colnames(ANATEL_JUNHO_2024_SELECT)

# Renomeia variável
ANATEL_JUNHO_2024_SELECT <- ANATEL_JUNHO_2024_SELECT %>%
  rename(Código_IBGE = `Código IBGE`)

# Ver as primeiras linhas dos dados
head(ANATEL_JUNHO_2024_SELECT)

# Converter coluna de porcentagem para formato numérico
ANATEL_JUNHO_2024_SELECT <- ANATEL_JUNHO_2024_SELECT %>%
  mutate_at(vars(`% área coberta`), ~ as.numeric(gsub(",", ".", .)))

# Ver as primeiras linhas dos dados
head(ANATEL_JUNHO_2024_SELECT)

# Ver a estrutura dos dados
str(ANATEL_JUNHO_2024_SELECT)

# Verificar nomes das colunas
colnames(ANATEL_JUNHO_2024_SELECT)

# Agregar dados por Código IBGE e demais variáveis categóricas e calcular a média da coluna numéricas
ANATEL_JUNHO_2024_AGREG <- ANATEL_JUNHO_2024_SELECT %>%
  group_by(Código_IBGE, Município, UF, Operadora, Tecnologia) %>%
  summarise(
    Área_Coberta_Média = mean(`% área coberta`, na.rm = TRUE),
    .groups = 'drop'
  )

# Ver as primeiras linhas do resultado
head(ANATEL_JUNHO_2024_AGREG)


# Agregar dados por Código IBGE e demais variáveis categóricas e calcular a média da coluna numéricas
ANATEL_JUNHO_2024_AGREG <- ANATEL_JUNHO_2024_SELECT %>%
  group_by(Código_IBGE, Município, UF, Operadora, Tecnologia) %>%
  summarise(
    Área_Coberta_Média = mean(`% área coberta`, na.rm = TRUE),
   .groups = 'drop'
  )

# Ver as primeiras linhas dos dados
head(ANATEL_JUNHO_2024_AGREG)

# Ver a estrutura dos dados
str(ANATEL_JUNHO_2024_AGREG)

##################################### Estatísticas Descritivas #############################################

# Calcular estatísticas descritivas
summary(ANATEL_JUNHO_2024_AGREG$Área_Coberta_Média)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.507  45.833  69.754  66.118  89.626 100.000

# Selecionar os 10 municípios com > cobertura média
TOP_10_MUNS <- ANATEL_JUNHO_2024_AGREG %>%
  arrange(desc(Área_Coberta_Média)) %>%
  head(10)

# Gráfico de barras horizontal para os 10 municípios com maior cobertura média
ggplot(TOP_10_MUNS, aes(x = reorder(Município, Área_Coberta_Média), y = Área_Coberta_Média, fill = Área_Coberta_Média)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = round(Área_Coberta_Média, 1)), 
            hjust = -0.3, color = "black", size = 3.5) +
  coord_flip() +
  scale_fill_viridis_c(option = "viridis", direction = -1) + # Aplicação da paleta viridis
  labs(title = "", x = "Município", y = "Área Coberta Média") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(10, 10, 10, 10))


# Selecionar os 10 municípios com < cobertura média
TAIL_10_MUNS <- ANATEL_JUNHO_2024_AGREG %>%
  arrange(Área_Coberta_Média) %>%
  head(10)

#### Gráfico com os 10 Municípios com Menor Área Coberta Média
ggplot(TAIL_10_MUNS, aes(x = reorder(Município, Área_Coberta_Média), y = Área_Coberta_Média, fill = Área_Coberta_Média)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Área_Coberta_Média, 1)), hjust = -0.3, color = "black", size = 3.5) +
  coord_flip() +
  scale_fill_viridis_c(option = "viridis") +
  labs(title = "", x = "Município", y = "Área Coberta Média") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#### 10 Municípios com MAIOR Área Coberta Média
ggplot(TOP_10_MUNS, aes(x = reorder(Município, Área_Coberta_Média), y = Área_Coberta_Média, fill = Área_Coberta_Média)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Área_Coberta_Média, 1)), hjust = -0.3, color = "black", size = 3.5) +
  coord_flip() +
  scale_fill_viridis_c(option = "viridis") +
  labs(title = "10 Municípios com Maior Área Coberta Média", x = "Município", y = "Área Coberta Média") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Gráfico de barras horizontal para os 10 municípios com menor cobertura
ggplot(TAIL_10_MUNS, aes(x = reorder(Município, Área_Coberta_Média), y = Área_Coberta_Média, fill = Área_Coberta_Média)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Área_Coberta_Média, 1)), hjust = -0.3, color = "black", size = 3.5) +
  coord_flip() +
  scale_fill_viridis_c(option = "viridis") +
  labs(title = "", x = "Município", y = "Área Coberta Média") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Criar intervalos de 20 em 20
ANATEL_JUNHO_2024_INTERVALO <- ANATEL_JUNHO_2024_AGREG %>%
  mutate(intervalo = cut(Área_Coberta_Média, breaks = seq(0, 100.5, by = 20), include.lowest = TRUE))

# Contar o número de municípios em cada intervalo e calcular percentuais.
# 27% DOS MUNICIPIOS TEM ATÉ 40% DE COBERTURA

ANATEL_JUNHO_2024_INTERVALO <- ANATEL_JUNHO_2024_INTERVALO %>%
  count(intervalo) %>%
  mutate(perc = n / sum(n) * 100)  # Calcular percentuais

# Exibir os nomes das colunas no dataframe
colnames(ANATEL_JUNHO_2024_INTERVALO)

# Gráfico de barras laterais - COLORIDO - com valores e legenda invertida
ggplot(ANATEL_JUNHO_2024_INTERVALO, aes(x = factor(1), y = perc, fill = intervalo)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = paste0(n, " (", round(perc, 1), "%)")), 
            position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_viridis_d(option = "viridis") +
  labs(title = "",
       x = NULL, y = "Percentual (%)", fill = "Intervalo em % de cobertura") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE))  # Inverte apenas a legenda


################################################## IBGE ###################################################

# Obter os limites geográficos de todos os municípios do Brasil
MUNICIPIOS_BR <- geobr::read_municipality(code_muni = 'all', year = 2022)
# Filtrando para obter apenas os municípios de Minas Gerais
MUNICIPIOS_MG <- municipios[municipios$abbrev_state == 'MG', ]

str(MUNICIPIOS_MG)

# Transformando a variável code_muni para caractere para que o join seja feito, pois na base ANATEL
# essa variável vem como caractere
MUNICIPIOS_MG <- MUNICIPIOS_MG %>%
  mutate(code_muni = as.character(code_muni))

# Inner Join
ANATEL_JUNHO_2024_JOIN_MUN <- merge(ANATEL_JUNHO_2024_AGREG, MUNICIPIOS_MG, by.x = "Código_IBGE", by.y = "code_muni")

str(ANATEL_JUNHO_2024_JOIN_MUN)

# Mudando o nome da base que segue de: ANATEL_JUNHO_2023_DROP_VAR para ANATEL_JUNHO_2023_PLOT
ANATEL_JUNHO_2024_ESPACIAL <- select(ANATEL_JUNHO_2024_JOIN_MUN, 
                                 Código_IBGE, Município, UF, 
                                 Operadora, Tecnologia, 
                                 Área_Coberta_Média, 
                                 geom)
str(ANATEL_JUNHO_2024_ESPACIAL)
head(ANATEL_JUNHO_2024_ESPACIAL)

ANATEL_JUNHO_2024_ESPACIAL <- st_as_sf(ANATEL_JUNHO_2024_ESPACIAL)

# Convertendo o dataframe para um objeto sf, se necessário
if (!inherits(ANATEL_JUNHO_2024_ESPACIAL, "sf")) {
  ANATEL_JUNHO_2024_ESPACIAL <- st_as_sf(ANATEL_JUNHO_2024_ESPACIAL)
  
# Transformando o sistema de coordenadas para WGS84
  ANATEL_JUNHO_2024_ESPACIAL <- st_transform(ANATEL_JUNHO_2024_ESPACIAL, crs = 4326)
}

######################################### Plota um mapa % de cobertura ##################################################
# Plota um mapa
ggplot(data = ANATEL_JUNHO_2024_ESPACIAL) +
  geom_sf(aes(fill = Área_Coberta_Média), color = "grey50", lwd = 0.3) +  # Define as bordas em cinza
  labs(title = "", 
       fill = "Área Coberta Média") +
  scale_fill_viridis_c(option = "viridis",  # Usando a paleta 'viridis'
                       direction = 1, 
                       labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain"),
    legend.title = element_text(face = "plain"),
     ) +
  annotation_scale(location = "bl", width_hint = 0.5, style = "ticks") +  # Adiciona uma barra de escala com estilo "ticks"
  coord_sf(datum = NA)  # Remove as coordenadas dos eixos

######################################### ANALISE ESPACIAL ##################################################

# Criar a Matriz de Vizinhança Queen diretamente
ANATEL_JUNHO_2024_VIZINHANCA_QUEEN <- poly2nb(ANATEL_JUNHO_2024_ESPACIAL, queen = TRUE)

# Converter a Matriz de Vizinhança Queen em uma Lista de Pesos Espaciais
ANATEL_JUNHO_2024_LISTA_PESOS_QUEEN <- nb2listw(ANATEL_JUNHO_2024_VIZINHANCA_QUEEN, style = "W")

# Calcular o Índice de Moran Global usando a Matriz Queen
MORAN_GLOBAL_QUEEN <- moran.test(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média, ANATEL_JUNHO_2024_LISTA_PESOS_QUEEN)

# Exibir o resultado do Índice de Moran
print(MORAN_GLOBAL_QUEEN)

# Cria objeto de vizinhança e calcular índice de Moran Global
ANATEL_JUNHO_2024_VIZINHANCA <- poly2nb(ANATEL_JUNHO_2024_ESPACIAL)
head(ANATEL_JUNHO_2024_VIZINHANCA)

# Converte o objeto de vizinhança (poly2nb) em uma lista de pesos.
ANATEL_JUNHO_2024_LISTA_PESOS <- nb2listw(ANATEL_JUNHO_2024_VIZINHANCA, style = "W")

# Calcular o Índice de Moran Global
MORAN_GLOBAL <- moran.test(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média, ANATEL_JUNHO_2024_LISTA_PESOS)
print(MORAN_GLOBAL)

# "W" indica que a ponderação será baseada na matriz de contiguidade, onde cada vizinho é ponderado igualmente.
ANATEL_JUNHO_2024_LOCAL_MORAN <- localmoran(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média, ANATEL_JUNHO_2024_LISTA_PESOS_QUEEN)

# Calcula o índice de Moran Local
ANATEL_JUNHO_2024_ESPACIAL$localI <- ANATEL_JUNHO_2024_LOCAL_MORAN[,1]

# Adicionar o valor do índice de Moran Local ao df original
head(ANATEL_JUNHO_2024_ESPACIAL)

# Variável de interesse
VARIAVEL_INTERESSE <- ANATEL_JUNHO_2024_AGREG$Área_Coberta_Média

# Calcular I de Moran
moran.test(VARIAVEL_INTERESSE, ANATEL_JUNHO_2024_LISTA_PESOS_QUEEN)

# Calcular valores localmente padronizados e seus lagged values
var_std <- scale(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média)
lagged_var <- lag.listw(ANATEL_JUNHO_2024_LISTA_PESOS_QUEEN, var_std)

# Criar dataframe para o diagrama de dispersão de Moran
moran_df <- data.frame(var_std = as.vector(var_std), lagged_var = as.vector(lagged_var))

head(ANATEL_JUNHO_2024_ESPACIAL)

# Adicionar nomes dos municípios para o gráfico
moran_df$Município <- ANATEL_JUNHO_2024_ESPACIAL$Município

# Plot do diagrama de espalhamento de Moran
# moran.plot(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média, 
          # listw = ANATEL_JUNHO_2024_LISTA_PESOS_QUEEN, 
          # labels = ANATEL_JUNHO_2024_ESPACIAL$Município)

# Criar o gráfico de dispersão de Moran com ggplot2, sem rótulos internos
ggplot(mapping = aes(x = var_std, y = lagged_var)) +
  geom_point(color = "blue", size = 1.5) +  # Pontos do scatterplot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linha de tendência linear
  labs(title = "", 
       x = "Valores padronizados", 
       y = "Valores lagged padronizados") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,))

# Definir paleta de cores
QUEBRAS <- quantile(ANATEL_JUNHO_2024_ESPACIAL$localI, probs = seq(0, 1, length.out = 5), na.rm = TRUE)

PALETA <- colorBin(palette = "RdYlBu", domain = ANATEL_JUNHO_2024_ESPACIAL$localI, bins = breaks)
print(PALETA)

# Calcular o Índice de Moran Local
local_moran <- localmoran(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média, ANATEL_JUNHO_2024_LISTA_PESOS_QUEEN)

# Adicionar os resultados ao dataframe
ANATEL_JUNHO_2024_ESPACIAL$localI <- as.numeric(local_moran[, 1])
ANATEL_JUNHO_2024_ESPACIAL$p.value <- as.numeric(local_moran[, 5])

# Definir as cores para o mapa LISA
lisa_colors <- brewer.pal(5, "RdYlBu")

nrow(ANATEL_JUNHO_2024_ESPACIAL)
length(ANATEL_JUNHO_2024_ESPACIAL$localI)

# Definir a paleta de cores específica
lisa_colors <- c("red", "blue", "lightblue", "lightcoral", "grey80")

# Criar uma coluna categórica para os quadrantes
ANATEL_JUNHO_2024_ESPACIAL$LISA_Quadrant <- factor(NA, 
                                                   levels = c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Significativo"))

ANATEL_JUNHO_2024_ESPACIAL$LISA_Quadrant[ANATEL_JUNHO_2024_ESPACIAL$localI > 0 & 
                                           ANATEL_JUNHO_2024_ESPACIAL$p.value <= 0.05 & 
                                           ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média > mean(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média)] <- "Alto-Alto"

ANATEL_JUNHO_2024_ESPACIAL$LISA_Quadrant[ANATEL_JUNHO_2024_ESPACIAL$localI > 0 & 
                                           ANATEL_JUNHO_2024_ESPACIAL$p.value <= 0.05 & 
                                           ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média < mean(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média)] <- "Baixo-Baixo"

ANATEL_JUNHO_2024_ESPACIAL$LISA_Quadrant[ANATEL_JUNHO_2024_ESPACIAL$localI < 0 & 
                                           ANATEL_JUNHO_2024_ESPACIAL$p.value <= 0.05 & 
                                           ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média > mean(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média)] <- "Alto-Baixo"

ANATEL_JUNHO_2024_ESPACIAL$LISA_Quadrant[ANATEL_JUNHO_2024_ESPACIAL$localI < 0 & 
                                           ANATEL_JUNHO_2024_ESPACIAL$p.value <= 0.05 & 
                                           ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média < mean(ANATEL_JUNHO_2024_ESPACIAL$Área_Coberta_Média)] <- "Baixo-Alto"

ANATEL_JUNHO_2024_ESPACIAL$LISA_Quadrant[is.na(ANATEL_JUNHO_2024_ESPACIAL$LISA_Quadrant)] <- "Não Significativo"

# Exibir o mapa
print(mapa_lisa)

# Criar o mapa LISA - FINAL - 01_09_2024
mapa_lisa <- ggplot(data = ANATEL_JUNHO_2024_ESPACIAL) +
  geom_sf(aes(fill = LISA_Quadrant), color = "grey50", lwd = 0.2) +  # Define as bordas em cinza
  scale_fill_manual(values = lisa_colors, 
                    name = "Clusterns LISA",
                    labels = c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Significativo")) +
  labs(title = "") +  # Título vazio para manter o layout uniforme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
     ) +
  annotation_scale(location = "bl", width_hint = 0.5, style = "ticks") +  # Adiciona uma barra de escala com estilo "ticks"
  # Barra de escala no canto inferior esquerdo com estilo "bar"
  coord_sf(datum = NA)  # Remove as coordenadas dos eixos e grade

# Exibir o mapa
print(mapa_lisa)

# Definir as cores para o mapa LISA
lisa_colors <- brewer.pal(5, "RdYlBu")


# Definir as cores para o mapa de significância                  
sig_colors <- brewer.pal(3, "Reds")

# Criar uma coluna indicando significância
ANATEL_JUNHO_2024_ESPACIAL$sig <- cut(ANATEL_JUNHO_2024_ESPACIAL$p.value, breaks = c(0, 0.01, 0.05, 1), labels = c("p < 0.01", "p < 0.05", "Não significativo"))

# Criar o mapa de significância
mapa_significancia <- tm_shape(ANATEL_JUNHO_2024_ESPACIAL) +
  tm_polygons("sig", palette = sig_colors, title = "Significância do Índice de Moran Local") +
  tm_layout(main.title = "Mapa de Significância LISA", 
            legend.outside = TRUE, 
            legend.outside.position = "left",  # Ajuste para uma posição válida
            frame = FALSE) +
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

# Exibir os mapas
tmap_arrange(mapa_significancia)

sig_colors <- c("red", "orange", "yellow", "lightgrey")

# Criar o mapa de significância usando ggplot2
mapa_significancia <- ggplot(data = ANATEL_JUNHO_2024_ESPACIAL) +
  geom_sf(aes(fill = sig), color = "grey50", lwd = 0.2) +  # Define as bordas em cinza
  scale_fill_manual(values = sig_colors, 
                    name = "Significância do Moran Local",
                    labels = c("Alto", "Médio", "Baixo", "Não Significativo")) +
  labs(title = "") +  # Título vazio para manter o layout uniforme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right",  # Mover a legenda para o lado direito
    legend.justification = c(1, 0)  # Posicionar a legenda no canto inferior direito
  ) +
  annotation_scale(location = "bl", width_hint = 0.5, style = "ticks") +  # Ajusta a barra de escala para o canto inferior esquerdo
  coord_sf(datum = NA)  # Remove as coordenadas dos eixos e grade

plot(mapa_significancia)

