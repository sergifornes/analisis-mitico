
#####

# Personaje "Divinidades hinduistas"
# Falta mitología turca
# No scrapear el texto "[editar]" de los títulos de sección de la Wikipedia. Ejemplo en: https://es.wikipedia.org/wiki/Vanir
# No scrapear el texto "[cita requerida]" de algunos textos. Ejemplo en: https://es.wikipedia.org/wiki/Vanir

#####

##########
# Librerías
##########

library(tidyverse)
library(igraph)
library(tidytext)
library(tm)



##########
# Data import
##########

personajes <- read_csv("../data/personajes.csv")

mitologias <- read_csv("../data/mitologias.csv")

ady_matrix <- as.matrix(read.csv("../data/ady_matrix.csv", encoding = "UTF-8"))
gf <- graph_from_adjacency_matrix(ady_matrix)



##########
# Personajes
##########

# Hay X personajes
personajes %>%
  count()

# Hay X mitologías
personajes %>%
  select(starts_with("Mitología")) %>%
  ncol()


colContinentes <- c(`África` = "#000000",
                    `América` = "#EE334E",
                    `Asia` = "#FCB131",
                    `Europa` = "#00A651",
                    `Oceanía` = "#0081C8")


personajes %>%
  select(starts_with("Mitología")) %>%
  colSums() %>%
  sort(decreasing = T) -> nMitologias
nMitologias

mitologias %>%
  left_join(tibble(Mitología = names(nMitologias), n = nMitologias)) -> mitologias
mitologias

mitologias %>%
  top_n(15, n) %>%
  ggplot() +
  geom_col(aes(y = reorder(Mitología, n), x = n, fill = Continente)) +
  labs(x = "Cantidad de personajes", y = "Mitología") +
  scale_fill_manual(values = colContinentes) +
  theme_light() +
  theme(panel.grid.major.y = element_blank())


personajes %>%
  select(starts_with("Región")) %>%
  colSums() %>%
  sort(decreasing = T) -> nRegiones
nRegiones

tibble(Región = names(nRegiones), n = nRegiones) %>%
  mutate(Región = str_sub(Región, 8,)) %>%
  left_join(unique(select(mitologias, 2:3))) %>%
  mutate(Continente = as.factor(Continente)) -> regiones
regiones

regiones %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(Región, n), fill = Continente)) +
  labs(x = "Cantidad de personajes", y = "Región") +
  facet_wrap(~Continente, scales = "free") +
  scale_fill_manual(values = colContinentes) +
  theme_light() +
  theme(panel.grid.major.y = element_blank(), legend.position = "none")


personajes %>%
  select(starts_with("Continente")) %>%
  colSums() %>%
  sort(decreasing = T) -> nContinentes
nContinentes

tibble(Continente = names(nContinentes), n = nContinentes) %>%
  mutate(Continente = str_sub(Continente, 12,)) -> continentes
continentes

continentes %>%
  ggplot() +
  geom_col(aes(x = reorder(Continente, -n), y = n, fill = Continente)) +
  labs(x = "Continente", y = "Cantidad de personajes") +
  scale_fill_manual(values = colContinentes) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), legend.position = "none")


personajes %>%
  select(starts_with("Clase")) %>%
  colSums() %>%
  sort(decreasing = T) -> nClases
nClases

tibble(Clase = names(nClases), n = nClases) %>%
  mutate(Clase = str_sub(Clase, 7)) -> clases
clases

clases %>%
  ggplot() +
  geom_col(aes(y = reorder(Clase, n), x = n)) +
  labs(x = "Cantidad de personajes", y = "Clase") +
  theme_light() +
  theme(panel.grid.major.y = element_blank(), legend.position = "none")



##########
# Mitologías que comparten personajes
##########

personajes %>%
  select(starts_with("Mitología")) %>%
  as.matrix() %>%
  t() -> matrizMitologias
colnames(matrizMitologias) <- personajes$Nombre

gfMit <- bipartite.projection(graph_from_incidence_matrix(matrizMitologias))$proj1

getContinente <- function(x) {
  return(colContinentes[mitologias$Continente[which(mitologias$Mitología == x)]])
}

getN <- function(x) {
  return(mitologias$n[which(mitologias$Mitología == x)])
}

V(gfMit)$continente <- sapply(V(gfMit)$name, getContinente)
V(gfMit)$n <- sapply(V(gfMit)$name, getN)


plot(
  gfMit,
  edge.width = E(gfMit)$weight,
  vertex.label = NA,
  vertex.size = (V(gfMit)$n)^0.4,
  vertex.color = V(gfMit)$continente
)
legend(x=-1.1, y=-0.95, names(colContinentes), pch=21,
       col="#777777", pt.bg=colContinentes, pt.cex=2.5, bty="n", ncol=1)


subgfMit <- delete.edges(gfMit, E(gfMit)[E(gfMit)$weight < 10])
Isolated <- which(degree(subgfMit) == 0)
subgfMit <- delete.vertices(subgfMit, Isolated)

plot(
  subgfMit,
  layout = layout_with_kk,
  edge.width = (E(subgfMit)$weight)^0.6,
  edge.label = E(subgfMit)$weight,
  edge.label.color = "black",
  edge.label.cex = 0.9,
  vertex.size = (V(subgfMit)$n)^0.55,
  vertex.color = V(subgfMit)$continente,
  vertex.label.color="black",
  vertex.label.dist=(V(subgfMit)$n)^0.22
)
legend(x=-1.1, y=-0.9, names(colContinentes), pch=21,
       col="#777777", pt.bg=colContinentes, pt.cex=2.5, bty="n", ncol=1)


mitologia <- "Mitología egipcia"
auxgf <- subgraph.edges(gfMit, incident(gfMit, mitologia))
plot(
  auxgf,
  edge.width = E(auxgf)$weight,
  edge.label = E(auxgf)$weight,
  edge.label.color = "black",
  edge.label.cex = 0.9,
  vertex.size = (V(auxgf)$n)^0.55,
  vertex.color = V(auxgf)$continente,
  vertex.label.color="black",
  vertex.label.dist=(V(auxgf)$n)^0.15,
  vertex.label.degree = pi/2
)



##########
# Personajes pertenecientes a diversas mitologías
##########

personajes %>%
  mutate(nMitologias = rowSums(across(starts_with("Mitología")))) -> personajes

personajes %>%
  ggplot() +
  geom_bar(aes(nMitologias)) +
  labs(x = "Cantidad de mitologías", y = "Cantidad de personajes") +
  theme_light()



##############################
########## PRUEBAS
##############################

##########
# Importancia de los personajes usando la matriz de relaciones
##########



##########
# Conejo
##########

stopwords_esp <- rbind(
  tibble(Palabra = tm::stopwords("spanish"), lexicon = "tm"),
  mutate(read_table("stop_words_spanish.txt", col_names = "Palabra"), lexicon = "countwordsfree.com")
)

personajes %>%
  select(Nombre, Texto) %>%
  unnest_tokens(Palabra, Texto) %>%
  anti_join(stopwords_esp) %>%
  filter(str_length(Palabra) > 1) -> tidy_personajes
tidy_personajes

tidy_personajes %>%
  count(Palabra, sort = T) %>%
  head(20) %>%
  ggplot(aes(n, reorder(Palabra, n))) +
  geom_col() +
  labs(x = "Cantidad de apariciones", y = "Palabra") +
  theme_light()

# pag22

