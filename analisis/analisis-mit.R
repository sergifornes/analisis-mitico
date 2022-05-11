
#####

# Scrapear todos los personajes que cumplen los requisitos de las categorías y luego limpiarlo aquí?

# Siete dioses de la fortuna?
# Patrones de los Viajeros?
# Set-animal (jeroglífico)?
# Paredra
# añadir ángeles abrahámicos?
# Idioma Osha-Ifa
# Pueblo yoruba


#####

##########
# Librerías
##########

library(tidyverse)
library(igraph)
library(scales)
library(tidytext)
library(tm)

# Visualización de grafos
library(ggraph)
# Operaciones con matrices para datos en formato tidy
library(widyr)
# Latent Dirichlet allocation
#library(topicmodels)


##########
# Data import
##########

conts <- read_csv("../data/continentes.csv")
regs <- read_csv("../data/regiones.csv")
mits <- read_csv("../data/mitologias.csv")
clas <- read_csv("../data/clases.csv")
pers <- read_csv("../data/personajes.csv")

persXmits <- read_csv("../data/personajesXmitologias.csv")
persXclas <- read_csv("../data/personajesXclases.csv")


adj_matrix <- as.matrix(read_csv("../data/adj_matrix.csv"))
gf <- graph_from_adjacency_matrix(adj_matrix) # grafo dirigido
gf_f <- graph_from_adjacency_matrix(adj_matrix, mode = "min") # grafo de relaciones fuertes



##########
# Data transformation
##########

persXmits %>%
  arrange(Nombre) -> persXmits

# Cantidad de personajes por clase, mitología, región y continente
persXclas %>%
  count(Clase) %>%
  right_join(clas, by = "Clase") %>%
  filter(!is.na(n)) -> clas
persXmits %>%
  count(Mitología) %>%
  right_join(mits, by = "Mitología") %>%
  filter(!is.na(n)) %>%
  left_join(regs, by = "Región") -> mits
persXmits %>%
  left_join(mits, by = "Mitología") %>%
  count(Región) %>%
  right_join(regs, by = "Región") -> regs
persXmits %>%
  left_join(mits, by = "Mitología") %>%
  count(Continente) -> conts


##########
# Modificación de la tabla "r_personajes.csv"
# Creación de las tablas "r_personajesCompartidos.csv", "r_personajesLinks.csv", "r_personajesTF-IDF.csv" y "r_mitologiasTF-IDF.csv"
##########
# Preprocesamiento del archivo "personajes.csv" para la visualización en PowerBI

# Variable "Femenino"
persXclas %>%
  filter(Clase == "Diosa") %>%
  right_join(pers, by = "Nombre") %>%
  mutate(Femenino = ifelse(is.na(Clase), FALSE, TRUE)) %>%
  select(-Clase) %>%
  arrange(Nombre) -> pers

# Variable "pageRank"
pagerank <- page_rank(gf)$vector
df_pagerank <- tibble(Nombre = names(pagerank), pageRank = pagerank)
pers %>%
  left_join(df_pagerank, by = "Nombre") -> pers

# Variable "betweennessCentrality"
betw <- betweenness(gf)
df_betw <- tibble(Nombre = names(betw), betweennessCentrality = betw)
pers %>%
  left_join(df_betw, by = "Nombre") -> pers

write_csv(pers, "../data/r_personajes.csv")


# Creación de la tabla "r_personajesCompartidos.csv" para la visualización en PowerBI
persXmits %>%
  arrange(Nombre) %>%
  mutate(n = 1) %>%
  pivot_wider(names_from = Mitología, values_from = n, values_fill = 0) %>%
  select(-Nombre) %>%
  as.matrix() %>%
  t() -> inc_matrix
colnames(inc_matrix) <- pers$Nombre
gfMit <- bipartite.projection(graph_from_incidence_matrix(inc_matrix))$proj1
df_persCompartidos <- as_data_frame(gfMit)
df_persCompartidos %>%
  rename(from = to,
         to = from) %>%
  rbind(df_persCompartidos) %>%
  left_join(mits, by = c("from" = "Mitología")) %>%
  rename(weightFrom = n,
         continenteFrom = Continente) %>%
  select(-Región) %>%
  left_join(mits, by = c("to" = "Mitología")) %>%
  rename(weightTo = n,
         continenteTo = Continente) %>%
  select(-Región) %>%
  write_csv("../data/r_personajesCompartidos.csv")


# Creación de la tabla "r_personajesLinks.csv" para la visualización el PowerBI
df_grafoDebil <- as_data_frame(gf)
df_grafoFuerte <- as_data_frame(gf_f)
df_grafoDebil %>%
  rbind(df_grafoFuerte) %>%
  rename(from = to,
         to = from) %>%
  rbind(df_grafoDebil) %>%
  rbind(df_grafoFuerte) %>%
  count(from, to) %>%
  write_csv("../data/r_personajesLinks.csv")


# Creación de la tabla "r_personajesTF-IDF.csv"
stopwords_esp <- rbind(
  tibble(Palabra = stopwords("spanish"), lexicon = "tm"),
  mutate(read_table("stop_words_spanish.txt", col_names = "Palabra"), lexicon = "txt"),
  tibble(Palabra = str_to_lower(pers$Nombre), lexicon = "pers"),
  tibble(Palabra = str_sub(str_to_lower(mits$Mitología), start = 11), lexicon = "mits")
) %>%
  filter(str_count(Palabra, "\\w+") == 1) # Se quitan los personajes y mitologías con más de una palabra

pers %>%
  select(Nombre, Texto_RAW) %>%
  unnest_tokens(Palabra, Texto_RAW) %>%
  anti_join(stopwords_esp, by = "Palabra") %>%
  filter(str_detect(Palabra, "^[:alpha:]+$"), # Se quitan todos los tokens que no estén formados unicamente con letras
         str_count(Palabra) != 1) %>% # Se quitan los tokens que tengan únicamente 1 carácter
  count(Nombre, Palabra, sort = T) -> pers_words

pers_words %>%
  group_by(Nombre) %>%
  summarize(total = sum(n)) -> total_pers_words

left_join(pers_words, total_pers_words, by = "Nombre") %>%
  group_by(Nombre) %>%
  mutate(rank = row_number(), tf = n / total) %>%
  ungroup() %>%
  bind_tf_idf(Palabra, Nombre, n) %>%
  write_csv("../data/r_personajesTF-IDF.csv")


# Creación de la tabla "r_mitologiasTF-IDF.csv"
persXmits %>%
  left_join(pers, by = "Nombre") %>%
  select(Mitología, Texto_RAW) %>%
  unnest_tokens(Palabra, Texto_RAW) %>%
  anti_join(stopwords_esp, by = "Palabra") %>%
  filter(str_detect(Palabra, "^[:alpha:]+$"), # Se quitan todos los tokens que no estén formados unicamente con letras
         str_count(Palabra) != 1) %>% # Se quitan los tokens que tengan únicamente 1 carácter
  count(Mitología, Palabra, sort = T) -> mits_words

mits_words %>%
  group_by(Mitología) %>%
  summarize(total = sum(n)) -> total_mits_words

left_join(mits_words, total_mits_words, by = "Mitología") %>%
  group_by(Mitología) %>%
  mutate(rank = row_number(), tf = n / total) %>%
  ungroup() %>%
  bind_tf_idf(Palabra, Mitología, n) %>%
  write_csv("../data/r_mitologiasTF-IDF.csv")



##########
# EDA
##########

# Valores "Dios", "Diosa" y "Deidades" de la variable "Clase" unificados
persXclas %>%
  mutate(Clase = ifelse(Clase == "Dios" | Clase == "Diosa", "Deidad", Clase)) %>%
  distinct(Nombre, Clase) -> persXclas

pers <- read_csv("../data/r_personajes.csv")

# Hay 3622 personajes
pers %>%
  count()

# Hay 136 mitologías
mits %>%
  count()


colContinentes <- c(`África` = "#000000",
                    `América` = "#EE334E",
                    `Asia` = "#FCB131",
                    `Europa` = "#00A651",
                    `Oceanía` = "#0081C8")


# Cantidad de personajes por mitología
mits %>%
  top_n(15, n) %>%
  ggplot() +
  geom_col(aes(y = reorder(Mitología, n), x = n, fill = Continente)) +
  labs(x = "Cantidad de personajes", y = "Mitología") +
  scale_fill_manual(values = colContinentes) +
  theme_light() +
  theme(panel.grid.major.y = element_blank())

mits %>%
  ggplot() +
  geom_histogram(aes(x = n), na.rm = T, binwidth = 50, center = 25) +
  labs(x = "Cantidad de personajes", y = "Frecuencia") +
  theme_light()


# Cantidad de personajes por región
regs %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(Región, n), fill = Continente)) +
  labs(x = "Cantidad de personajes", y = "Región") +
  facet_wrap(~Continente, scales = "free") +
  scale_fill_manual(values = colContinentes) +
  theme_light() +
  theme(panel.grid.major.y = element_blank(), legend.position = "none")


# Cantidad de personajes por continente
conts %>%
  ggplot() +
  geom_col(aes(x = reorder(Continente, -n), y = n, fill = Continente)) +
  labs(x = "Continente", y = "Cantidad de personajes") +
  scale_fill_manual(values = colContinentes) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), legend.position = "none")


# Cantidad de personajes por clase
clas %>%
  ggplot() +
  geom_col(aes(y = reorder(Clase, n), x = n)) +
  labs(x = "Cantidad de personajes", y = "Clase") +
  theme_light() +
  theme(panel.grid.major.y = element_blank(), legend.position = "none")


# Mitologías que comparten personajes
persXmits %>%
  mutate(n = 1) %>%
  pivot_wider(names_from = Mitología, values_from = n, values_fill = 0) %>%
  select(-Nombre) %>%
  as.matrix() %>%
  t() -> inc_matrix
colnames(inc_matrix) <- pers$Nombre

gfMit <- bipartite.projection(graph_from_incidence_matrix(inc_matrix))$proj1

getCont <- function(x) {
  return(colContinentes[filter(mits, Mitología == x)$Continente])
}

getN <- function(x) {
  return(filter(mits, Mitología == x)$n)
}

V(gfMit)$cont <- sapply(V(gfMit)$name, getCont)
V(gfMit)$n <- sapply(V(gfMit)$name, getN)

plot(
  gfMit,
  edge.width = E(gfMit)$weight,
  vertex.label = NA,
  vertex.size = (V(gfMit)$n)^0.4,
  vertex.color = V(gfMit)$cont
)
legend(x=-1.1, y=-0.95, names(colContinentes), pch=21,
       col="#777777", pt.bg=colContinentes, pt.cex=2.5, bty="n", ncol=1)


# grafo con n o más personajes compartidos
n <- 10
subgfMit_abs <- delete.edges(gfMit, E(gfMit)[E(gfMit)$weight < n])
Isolated_abs <- which(degree(subgfMit_abs) == 0)
subgfMit_abs <- delete.vertices(subgfMit_abs, Isolated_abs)

plot(
  subgfMit_abs,
  layout = layout_with_kk,
  edge.width = (E(subgfMit_abs)$weight)^0.6,
  edge.label = E(subgfMit_abs)$weight,
  edge.label.color = "black",
  edge.label.cex = 0.9,
  vertex.size = (V(subgfMit_abs)$n)^0.55,
  vertex.color = V(subgfMit_abs)$cont,
  vertex.frame.color= "white",
  vertex.label.color = "black"
)
legend(x = -1.1, y = -0.9, names(colContinentes), pch = 21,
       col = "#777777", pt.bg = colContinentes, pt.cex = 2.5, bty = "n", ncol = 1)


# grafo con al menos un x% de personajes compartidos (GRÁFICO NO USADO)
x <- 1

tibble(Mitología_L = get.edgelist(gfMit)[,1], Mitología_R = get.edgelist(gfMit)[,2]) %>%
  left_join(mits, by = c("Mitología_L" = "Mitología")) %>%
  mutate(n_L = n) %>%
  select(Mitología_L, Mitología_R, n_L) %>%
  left_join(mits, by = c("Mitología_R" = "Mitología")) %>%
  mutate(n_R = n) %>%
  select(Mitología_L, Mitología_R, n_L, n_R) -> aux_edgeN
aux_edgeN

subgfMit_rel <- delete.edges(gfMit, E(gfMit)[(E(gfMit)$weight / aux_edgeN$n_L < x) & (E(gfMit)$weight / aux_edgeN$n_R < x)])
Isolated_rel <- which(degree(subgfMit_rel) == 0)
subgfMit_rel <- delete.vertices(subgfMit_rel, Isolated_rel)

plot(
  subgfMit_rel,
  layout = layout_with_kk,
  edge.width = (E(subgfMit_abs)$weight)^0.6,
  edge.label = E(subgfMit_abs)$weight,
  edge.label.color = "black",
  edge.label.cex = 0.9,
  vertex.color = V(subgfMit_rel)$cont,
  vertex.frame.color= "white",
  vertex.label.color = "black"
)
legend(x = -1.1, y = -0.9, names(colContinentes), pch = 21,
       col = "#777777", pt.bg = colContinentes, pt.cex = 2.5, bty = "n", ncol = 1)


# cantidad de personajes compartidos de la mitología m
mitologia <- "Mitología nórdica"
auxgf <- subgraph.edges(gfMit, incident(gfMit, mitologia))
plot(
  auxgf,
  edge.width = E(auxgf)$weight,
  edge.label = E(auxgf)$weight,
  edge.label.color = "black",
  edge.label.cex = 0.9,
  vertex.size = (V(auxgf)$n)^0.55,
  vertex.color = V(auxgf)$cont,
  vertex.frame.color= "white",
  vertex.label.color="black"
)
legend(x = -1.1, y = -0.9, names(colContinentes), pch = 21,
       col = "#777777", pt.bg = colContinentes, pt.cex = 2.5, bty = "n", ncol = 1)


# Personajes pertenecientes a diversas mitologías

persXmits %>%
  count(Nombre) %>%
  right_join(pers, by = "Nombre") %>%
  rename(nMits = n) -> pers

pers %>%
  ggplot() +
  geom_histogram(aes(nMits), binwidth = 1) +
  labs(x = "Cantidad de mitologías", y = "Cantidad de personajes") +
  theme_light() +
  scale_x_continuous(breaks = 1:max(pers$nMits)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")


# Personajes femeninos
pers %>%
  count(Femenino)
# Hay 676 personajes (19%) pertenecientes a categorías de personajes femeninos
# Probablemente haya más personajes femeninos que no estén en categorías femeninas, por lo que esta variable está sesgada



##########
# Análisis del gráfo (grafo dirigido)
##########

# El grafo dirigido es el grafo original
# Las aristas son los links de unos personajes apuntando a otros personajes

# Obtención del in-degree de los nodos
V(gf)$indegree <- degree(gf, mode = "in")
V(gf)$size <- 2 * sqrt(degree(gf, mode = "in"))

# Estructura general del grafo:

# \abs{V(gf)} = 3622 nodos (personajes)
nV <- length(V(gf))
nV
# \abs{E(gf)} = 18852 aristas (relaciones)
nE <- length(E(gf))
nE

# El grafo no es conexo
is.connected(gf)

# 315 componentes
components(gf)$no

head(sort(components(gf)$csize, decreasing = T), 10)
# L_1(gf) = 3277 el orden del mayor componente es de 3277
# L_2(gf) = 4 el orden del segundo mayor componente es de 4
# log(L_1(gf)) > L_2(gf) por lo que podemos decir que la red tiene un componente gigante

# El 90% de los personajes pertenecen al mayor componente
max(components(gf)$csize) / sum(components(gf)$csize)

# Un personaje mitológico está referenciado, de media, por 5 personajes mitológicos:
mean(V(gf)$indegree)

# Distancia máxima entre dos pares de nodos
diameter(gf, directed = F, unconnected = T)

# Es un grafo muy poco denso (aunque no podemos decir que es sparse), el 0.14% de las aristas posibles están definidas
edge_density(gf)


# Small World? https://stats.stackexchange.com/questions/175492/how-to-test-statistically-whether-my-network-graph-is-a-small-world-network
set.seed(2021)
L_rand <- c()
C_rand <- c()
for(i in 1:50) {
  gf_rand <- sample_gnm(n = nV, m = nE, directed = T)
  L_rand[i] <- mean_distance(gf_rand, directed = F)
  C_rand[i] <- transitivity(gf_rand)
}
lambda <- mean_distance(gf, directed = F) / mean(L_rand)
gamma <- transitivity(gf) / mean(C_rand)
lambda
# lambda cercano a 1
gamma
# gamma mucho mayor que uno
# small world cumple ambas condiciones


# Degree distribution (Scale-free network?)
# La mayoría de las distribuciones de los grados de los nodos de las redes siguen una Power Law, provenientes de un crecimiento "natural" de la red
# La Power Law se encuentra en la cola de la distribución
# como se chequea si es Scale-free network una directed network?
# one of the main observations is that the graphs are ‘scale-free’ (see [5, 7, 24] and the references therein); the distribution of vertex degrees follows a
#power law, rather than the Poisson distribution of the classical random graph models G(n, p) and G(n, M) [16, 17, 19], see also [9].
# personalmente creo que tiene sentido que los indegree sigan una power-law
deg_dist <- tibble(deg = seq(0, length(degree_distribution(gf, mode = "in")) - 1), prob = degree_distribution(gf, mode = "in")) %>%
  filter(deg > 0, prob > 0)

deg_dist %>%
  ggplot(aes(x = deg, y = prob)) +
  geom_point() +
  labs(x = "Indegree", y = "Porcentaje de vértices") +
  theme_light() +
  scale_y_continuous(labels = percent_format(accuracy = 1))

deg_dist %>%
  ggplot(aes(x = deg, y = prob)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Indegree \n (escala logarítmica)", y = "Porcentaje de vértices \n (escala logarítmica)") +
  theme_light()
# este gráfico es un indicio de que la distribución puede seguir una power law


# Camino más corto entre 2 personajes
p1 <- "Enlil"
p2 <- "Hera"
p1 <- str_replace_all(p1, "[^[:alpha:]]", ".")
p2 <- str_replace_all(p2, "[^[:alpha:]]", ".")
sp <- shortest_paths(gf, from = V(gf)[name == p1], to = V(gf)[name == p2], mode = "all")$vpath[[1]]
gf_sp <- induced_subgraph(gf, sp)
plot(gf_sp,
     vertex.color = "grey80",
     edge.color = "black",
     edge.width = 1,
     vertex.frame.color= "white",
     vertex.label.color = "black",
     vertex.label.font = 2,
     main = "Camino más corto")


# Vecinos de un personaje (vecinos débiles)
p <- "Bastet"
p <- str_replace_all(p, "[^[:alpha:]]", ".")
unique(neighbors(gf, p, mode = "all")$name)


# Análisis para diferentes subconjuntos del dataset

sub_mit <- "Mitología hinduista"
sub_clas <- "Global"
sub_fem <- FALSE
if(sub_mit == "Global") {
  index_sub <- matrix(c(TRUE), nrow = nrow(pers))
} else {
  persXmits %>%
    filter(Mitología == sub_mit) %>%
    right_join(pers) %>%
    arrange(Nombre) %>%
    replace_na(list(Mitología = 0)) %>%
    .$"Mitología" == sub_mit -> index_sub
}
if(sub_clas == "Global") {
  index_sub <- index_sub & matrix(c(TRUE), nrow = nrow(pers))
} else {
  persXclas %>%
    filter(Clase == sub_clas) %>%
    right_join(pers) %>%
    arrange(Nombre) %>%
    replace_na(list(Clase = 0)) %>%
    .$"Clase" == sub_clas -> index_sub_clas
  index_sub <- index_sub & index_sub_clas
}
if(sub_fem) {
  index_sub <- index_sub & pers$Femenino == TRUE
}
m_index_sub <- colnames(adj_matrix) %in% pers[index_sub,]$Nombre
gf_sub <- graph_from_adjacency_matrix(adj_matrix[m_index_sub, m_index_sub])
V(gf_sub)$size <- 2 * sqrt(degree(gf_sub, mode = "in"))

# betweenness como medida de centralidad
betw <- betweenness(gf_sub)
pers_sub_betw <- tibble(Nombre = names(betw), betweenness = betw)
pers_sub_betw %>%
 top_n(10, betweenness) %>%
 ggplot(aes(x = betweenness, y = reorder(Nombre, betweenness))) +
 geom_col() +
 labs(y = "Personaje") +
 theme_light() +
 theme(panel.grid.major.y = element_blank())

# pagerank como medida de centralidad
pager <- page_rank(gf_sub)$vector
pers_sub_pager <- tibble(Nombre = names(pager), pageRank = pager)
pers_sub_pager %>%
  top_n(10, pageRank) %>%
  ggplot(aes(x = pageRank, y = reorder(Nombre, pageRank))) +
  geom_col() +
  labs(y = "Personaje") +
  theme_light() +
  theme(panel.grid.major.y = element_blank())



##########
# Análisis del gráfo (grafo fuerte)
##########

# El grafo fuerte es el grafo de relaciones bidireccionales
# Los personajes están relacionados si ambos se apuntan entre ellos

# \abs{V(gf)} = 3622 nodos
nV_f <- length(V(gf_f))
nV_f
# \abs{E(gf)} = 4061 aristas
nE_f <- length(E(gf_f))
nE_f

# 1547 componentes
components(gf_f)$no

head(sort(components(gf_f)$csize, decreasing = T), 10)
# L_1(gf) = 1792 el orden del mayor componente es de 1792
# L_2(gf) = 20 el orden del segundo mayor componente es de 20
# log(L_1(gf)) < L_2(gf) por lo que la red no tiene un componente gigante, aunque si uno muy grande

# El 49% de los personajes pertenecen al mayor componente
max(components(gf_f)$csize) / sum(components(gf_f)$csize)

# Distancia máxima entre dos pares de nodos
diameter(gf_f, unconnected = T)


# Vecinos de un personaje (vecinos fuertes)
p <- "Bastet"
neighbors(gf_f, p)


# subconjunto del grafo fuerte (CORE de una mitología)

sub_f <- "Mitología de Creta"
if(sub_f == "Global") {
  index_f_sub <- matrix(c(TRUE), nrow = nrow(pers))
} else {
  persXmits %>%
    filter(Mitología == sub_f) %>%
    right_join(pers) %>%
    arrange(Nombre) %>%
    replace_na(list(Mitología = 0)) %>%
    .$"Mitología" == sub_f -> index_f_sub
}
m_index_f_sub <- colnames(adj_matrix) %in% pers[index_f_sub,]$Nombre
gf_f_sub <- graph_from_adjacency_matrix(adj_matrix[m_index_f_sub, m_index_f_sub], mode = "min")
V(gf_f_sub)$size <- 2 * sqrt(degree(gf_f_sub))
# Nos quedamos con el componente conexo más grande
Isolated_f <- which(components(gf_f_sub)$membership != which.max(components(gf_f_sub)$csize))
gf_f_sub <- delete.vertices(gf_f_sub, Isolated_f)
is.connected(gf_f_sub)
plot(gf_f_sub)

# Diámetro del subgrafo
dmt_f <- get_diameter(gf_f_sub)
gf_f_dmt <- induced_subgraph(gf_f_sub, dmt_f)
plot(gf_f_dmt,
     vertex.color = "grey80",
     edge.color = "black",
     edge.width = 1,
     vertex.frame.color= "white",
     vertex.label.color = "black",
     vertex.label.font = 2,
     main = "Diámetro del grafo")

# Mayores cliques
largest_cliques(gf_f_sub)

# subgrafo con los n personajes con mayor pageRank
n <- 20
pers[pers$Nombre %in% V(gf_f_sub)$name,] %>%
  arrange(-pageRank) %>%
  tail(-n) %>%
  .$Nombre -> pers_irrel
plot(delete.vertices(gf_f_sub, V(gf_f_sub)[pers_irrel]),
     vertex.color = "grey80",
     vertex.frame.color= "white",
     vertex.label.color = "black",
     vertex.label.font = 2)



##########
# Análisis de texto
##########

# CHAPTER 1

# A parte de los stopwords genéricos del español, se han añadido "cita", "requerida" y "editar" debido a que en la Wikipedia aparece [cita requerida] y [editar] como comentarios
# También se añaden los nombres de los personajes y de las mitologías a la lista de stopwords
stopwords_esp <- rbind(
  tibble(Palabra = stopwords("spanish"), lexicon = "tm"),
  mutate(read_table("stop_words_spanish.txt", col_names = "Palabra"), lexicon = "txt"),
  tibble(Palabra = str_to_lower(pers$Nombre), lexicon = "pers"),
  tibble(Palabra = str_sub(str_to_lower(mits$Mitología), start = 11), lexicon = "mits")
) %>%
  filter(str_count(Palabra, "\\w+") == 1) # Se quitan los personajes y mitologías con más de una palabra

pers %>%
  select(Nombre, Texto_RAW) %>%
  unnest_tokens(Palabra, Texto_RAW) %>%
  anti_join(stopwords_esp, by = "Palabra") %>%
  filter(str_detect(Palabra, "^[:alpha:]+$"), # Se quitan todos los tokens que no estén formados unicamente con letras
         str_count(Palabra) != 1) -> tidy_pers # Se quitan los tokens que tengan únicamente 1 carácter
tidy_pers

tidy_pers %>%
  count(Palabra, sort = T) %>%
  head(20) %>%
  ggplot(aes(n, reorder(Palabra, n))) +
  geom_col() +
  labs(x = "Cantidad de apariciones", y = "Palabra") +
  theme_light() +
  theme(panel.grid.major.y = element_blank())


# Diferencias entre 2 mitologías
# m2 puede ser "Global" para poder comparar m1 con el resto de personajes en su conjunto
m1 <- "Mitología egipcia"
m2 <- "Mitología nórdica"

persXmits %>%
  filter(Mitología == m1) -> auxM1

if(m2 == "Global") {
  persXmits %>%
    filter(Mitología != m1) %>%
    mutate(Mitología = "Global") -> auxM2
} else {
  persXmits %>%
    filter(Mitología == m2) -> auxM2
}

rbind(auxM1, auxM2) %>%
  left_join(tidy_pers, by = "Nombre") %>%
  count(Mitología, Palabra) %>%
  group_by(Mitología) %>%
  mutate(Proporción = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = Mitología, values_from = Proporción) %>% # OJO, las palabras con alguna frecuencia == 0 no se imprimirán
  ggplot(aes(y = !!as.symbol(m1), x = !!as.symbol(m2))) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3, na.rm = T) +
  geom_abline(color = "gray40", lty = 2) +
  geom_text(aes(label = Palabra), check_overlap = T, vjust = "outward", hjust = "outward", na.rm = T) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  theme_light() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())


# CHAPTER 3

# Análisis personajes

tidy_pers %>%
  count(Nombre, Palabra, sort = T) -> pers_words
pers_words

pers_words %>%
  group_by(Nombre) %>%
  summarize(total = sum(n)) -> total_pers_words
total_pers_words

left_join(pers_words, total_pers_words, by = "Nombre") %>%
  group_by(Nombre) %>%
  mutate(rank = row_number(), tf = n / total) %>%
  ungroup() -> pers_words
pers_words

# Term frequancy distribution por personaje (PARECE POCO INTERESANTE)
p <- "Jörmundgander"
pers_words %>%
  filter(Nombre == p) -> aux_pers_words
aux_pers_words
aux_pers_words %>%
  ggplot(aes(tf)) +
  geom_histogram()

# tf-idf por personaje con todo corpus
p <- "Helena"
pers_words %>%
  bind_tf_idf(Palabra, Nombre, n) -> pers_tf_idf
pers_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  filter(Nombre == p) %>%
  top_n(15, tf_idf) %>%
  ggplot(aes(x = tf_idf, y = fct_reorder(Palabra, tf_idf))) +
  geom_col()


# Análisis mitologías

persXmits %>%
  left_join(pers, by = "Nombre") %>%
  select(Mitología, Texto_RAW) %>%
  unnest_tokens(Palabra, Texto_RAW) %>%
  anti_join(stopwords_esp, by = "Palabra") %>%
  filter(str_detect(Palabra, "^[:alpha:]+$"), # Se quitan todos los tokens que no estén formados unicamente con letras
         str_count(Palabra) != 1) -> tidy_mits # Se quitan los tokens que tengan únicamente 1 carácter
tidy_mits

tidy_mits %>%
  count(Mitología, Palabra, sort = T) -> mits_words
mits_words

mits_words %>%
  group_by(Mitología) %>%
  summarize(total = sum(n)) -> total_mits_words
total_mits_words

left_join(mits_words, total_mits_words, by = "Mitología") %>%
  group_by(Mitología) %>%
  mutate(rank = row_number(), tf = n / total) %>%
  ungroup() -> mits_words
mits_words

# Term frequancy distribution por mitología (PARECE POCO INTERESANTE)
m <- "Mitología hinduista"
mits_words %>%
  filter(Mitología == m) -> aux_mits_words
aux_mits_words
aux_mits_words %>%
  ggplot(aes(tf)) +
  geom_histogram()

# tf-idf por mitología
mits_words %>%
  bind_tf_idf(Palabra, Mitología, n) -> mits_tf_idf
m <- "Mitología hinduista"
mits_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  filter(Mitología == m) %>%
  top_n(15, tf_idf) %>%
  ggplot(aes(x = tf_idf, y = fct_reorder(Palabra, tf_idf))) +
  geom_col()


# CHAPTER 4

# bigramas por personajes

pers %>%
  select(Nombre, Texto_RAW) %>%
  unnest_tokens(bigrama, Texto_RAW, token = "ngrams", n = 2) -> bigrams_pers
bigrams_pers

bigrams_separated <- bigrams_pers %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!palabra1 %in% stopwords_esp$Palabra) %>%
  filter(!palabra2 %in% stopwords_esp$Palabra) %>%
  filter(str_detect(palabra1, "^[:alpha:]+$"),
         str_count(palabra1) != 1) %>%
  filter(str_detect(palabra2, "^[:alpha:]+$"),
         str_count(palabra2) != 1)

bigram_counts <- bigrams_filtered %>%
  count(palabra1, palabra2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigrama, palabra1, palabra2, sep = " ")


# trigramas por personajes

pers %>%
  select(Nombre, Texto_RAW) %>%
  unnest_tokens(trigrama, Texto_RAW, token = "ngrams", n = 3) -> trigrams_pers
trigrams_pers

trigrams_separated <- trigrams_pers %>%
  separate(trigrama, c("palabra1", "palabra2", "palabra3"), sep = " ")
trigrams_filtered <- trigrams_separated %>%
  filter(!palabra1 %in% stopwords_esp$Palabra) %>%
  filter(!palabra2 %in% stopwords_esp$Palabra) %>%
  filter(!palabra3 %in% stopwords_esp$Palabra) %>%
  filter(str_detect(palabra1, "^[:alpha:]+$"),
         str_count(palabra1) != 1) %>%
  filter(str_detect(palabra2, "^[:alpha:]+$"),
         str_count(palabra2) != 1) %>%
  filter(str_detect(palabra3, "^[:alpha:]+$"),
         str_count(palabra3) != 1)

trigram_counts <- trigrams_filtered %>%
  count(palabra1, palabra2, palabra3, sort = TRUE)
trigram_counts


# bigramas por mitologías

persXmits %>%
  left_join(pers, by = "Nombre") %>%
  select(Mitología, Texto_RAW) %>%
  unnest_tokens(bigrama, Texto_RAW, token = "ngrams", n = 2) -> bigrams_mits
bigrams_mits

bigrams_mits_separated <- bigrams_mits %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ")
bigrams_mits_filtered <- bigrams_mits_separated %>%
  filter(!palabra1 %in% stopwords_esp$Palabra) %>%
  filter(!palabra2 %in% stopwords_esp$Palabra) %>%
  filter(str_detect(palabra1, "^[:alpha:]+$"),
         str_count(palabra1) != 1) %>%
  filter(str_detect(palabra2, "^[:alpha:]+$"),
         str_count(palabra2) != 1)

bigrams_mits_united <- bigrams_mits_filtered %>%
  unite(bigrama, palabra1, palabra2, sep = " ")


# bigramas con mayor tf_idf de la mitología m

bigram_mits_tf_idf <- bigrams_mits_united %>%
  count(Mitología, bigrama, sort = TRUE) %>%
  bind_tf_idf(bigrama, Mitología, n) %>%
  arrange(desc(tf_idf))
bigram_mits_tf_idf

m <- "Mitología nórdica"
bigram_mits_tf_idf %>%
  filter(Mitología == m)


# Visualización de Cadena de Márkov

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>% # bigramas con más de 20 apariciones
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
# DEBERÍA ELIMINAR LOS BIGRAMAS QUE REPRESENTES PERSONAJES? POR EJEMPLO EL BIGRAMA "HOMBRE LOBO"


m <- "Mitología nórdica"
tidy_pers %>%
  left_join(persXmits, by = "Nombre") %>%
  filter(Mitología == m) %>%
  select(-Mitología) -> tidy_submits
word_pairs <- tidy_submits %>%
  pairwise_count(Palabra, Nombre, sort = TRUE)
word_pairs
word_pairs %>%
  filter(item1 == "serpiente")
# PARECE BASTANTE INÚTIL, TODAS LAS RELACIONES SON OBVIAS Y TODOS LOS ARTÍCULOS COMPARTEN MUCHAS PALABRAS PARECIDAS COMO POR EJEMPLO MITOLOGÍA, DIOS, HIJO, NOMBRE...

word_cors <- tidy_submits %>%
  group_by(Palabra) %>%
  filter(n() >= 20) %>% # solo nos interesan palabras que aparezcan al menos 20 veces
  pairwise_cor(Palabra, Nombre, sort = TRUE)
word_cors
word_cors %>%
  filter(item1 == "serpiente")

# las n correlaciones más fuertes
n <- 30
word_cors %>%
  top_n(n * 2, correlation) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(edge_alpha = 1, show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


# CHAPTER 6

# Latent Dirichlet allocation (LDA) para crear clusters -> Topic modeling

# Clusters de personajes

pers_words %>%
  cast_dtm(Nombre, Palabra, n) -> pers_dtm

# modelo de 2 temas (k = 2)
pers_lda <- LDA(pers_dtm, k = 2)
pers_lda

# Cada tema es un conjunto de palabras
pers_topics <- tidy(pers_lda, matrix = "beta")
pers_topics

pers_top_terms <- pers_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
pers_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_spread <- pers_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>% # se eliminan los términos poco frecuentes (con beta muy pequeño)
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread

beta_spread %>%
  top_n(10, log_ratio) %>%
  arrange(-log_ratio)
beta_spread %>%
  top_n(10, -log_ratio) %>%
  arrange(log_ratio)

# Cada personaje es un conjunto de temas
pers_gamma <- tidy(pers_lda, matrix = "gamma")
pers_gamma

# Los 2 grupos están claramente separados, es decir, no hay duda sobre el grupo al que pertenecen
pers_gamma %>%
  ggplot(aes(x = gamma)) +
  geom_histogram()

pers_gamma %>%
  pivot_wider(names_from = topic, values_from = gamma, names_glue = "grupo{topic}") %>%
  mutate(grupo = ifelse(grupo1 > 0.5, 1, 2)) %>%
  rename(Nombre = document) %>%
  right_join(persXmits, by = "Nombre") %>%
  count(grupo, Mitología) %>%
  ggplot(aes(x = Mitología, y = n, fill = as_factor(grupo))) +
  geom_col()

pers_gamma %>%
  pivot_wider(names_from = topic, values_from = gamma, names_glue = "grupo{topic}") %>%
  mutate(grupo = ifelse(grupo1 > 0.5, 1, 2)) %>%
  rename(Nombre = document) %>%
  right_join(persXclas, by = "Nombre") %>%
  count(grupo, Clase) %>%
  ggplot(aes(x = Clase, y = n, fill = as_factor(grupo))) +
  geom_col()

pers_gamma %>%
  pivot_wider(names_from = topic, values_from = gamma, names_glue = "grupo{topic}") %>%
  mutate(grupo = ifelse(grupo1 > 0.5, 1, 2)) %>%
  rename(Nombre = document) %>%
  right_join(pers, by = "Nombre") %>%
  count(grupo, Femenino) %>%
  ggplot(aes(x = Femenino, y = n, fill = as_factor(grupo))) +
  geom_col()

pers_gamma %>%
  pivot_wider(names_from = topic, values_from = gamma, names_glue = "grupo{topic}") %>%
  mutate(grupo = ifelse(grupo1 > 0.5, 1, 2)) %>%
  rename(Nombre = document) %>%
  right_join(persXmits, by = "Nombre") %>%
  left_join(mits, by = "Mitología") %>%
  count(grupo, Región) %>%
  ggplot(aes(x = Región, y = n, fill = as_factor(grupo))) +
  geom_col()

pers_gamma %>%
  pivot_wider(names_from = topic, values_from = gamma, names_glue = "grupo{topic}") %>%
  mutate(grupo = ifelse(grupo1 > 0.5, 1, 2)) %>%
  rename(Nombre = document) %>%
  right_join(persXmits, by = "Nombre") %>%
  left_join(mits, by = "Mitología") %>%
  count(grupo, Continente) %>%
  ggplot(aes(x = Continente, y = n, fill = as_factor(grupo))) +
  geom_col()


# Clusters de mitologías

mits_words %>%
  cast_dtm(Mitología, Palabra, n) -> mits_dtm

# modelo de 2 temas (k = 2)
mits_lda <- LDA(mits_dtm, k = 2)
mits_lda

# Cada tema es un conjunto de palabras
mits_topics <- tidy(mits_lda, matrix = "beta")
mits_topics

mits_top_terms <- mits_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
mits_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_spread_mits <- mits_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>% # se eliminan los términos poco frecuentes (con beta muy pequeño)
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread_mits

beta_spread_mits %>%
  top_n(10, log_ratio) %>%
  arrange(-log_ratio)
beta_spread_mits %>%
  top_n(10, -log_ratio) %>%
  arrange(log_ratio)

# Cada personaje es un conjunto de temas
mits_gamma <- tidy(mits_lda, matrix = "gamma")
mits_gamma

mits_gamma %>%
  ggplot(aes(x = gamma)) +
  geom_histogram()
# Con mitologías no se ve una clara división entre los 2 grupos

