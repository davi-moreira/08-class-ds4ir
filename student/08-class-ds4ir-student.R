# ---
# title: "DS4IR"
# subtitle: "Análise de Redes"
# author: 
# - Professor Davi Moreira
# - Professor Rafael Magalhães
# date: "`r format(Sys.time(), '%d-%m-%Y')`"
# output: 
# revealjs::revealjs_presentation:
# theme: simple
# highlight: haddock
# transition: slide
# center: true
# css: stylesheet.css
# reveal_options:
# controls: false  # Desativar botões de navegação no slide
# mouseWheel: true # Passar slides com o mouse
# ---
# 
# ```{r echo=FALSE, message=FALSE, warning=FALSE, results='hide'}
# # pacotes necessários
library(lubridate)
library(stringr)
library(forcats)
library(igraph)
library(intergraph)
library(GGally)
library(network)
library(gridExtra)
library(grid)
library(networkD3)
library(randomNames) 
library(tidyverse)
library(here)
# ```
# 
# ## Programa
# 
# 1. A análise de redes
# 2. Principais medidas de centralidade
# 3. Redes dirigidas e não dirigidas
# 
# ## Análise de dados do Twitter
# 
# <center>
# 
# ![Folha](images/deputados-celulares.png){width=600px}
# 
# </center>
# 
# ## A análise de redes
# 
# A análise de redes é aplicada para descrever relações entre nossas observações de interesse.
# As relações podem ser de amizade entre pessoas, trocas comerciais entre países, conexão entre
# atividades no planejamento estratégico de uma empresa, citações e co-autoria em artigos, etc.
# 
# Através da análise de redes obtemos novas estatísticas sobre os dados que temos e
# potencializamos nossas conclusões através das visualizações.
# 
# ## Undirected Network
# 
# ## Rede de Casamentos na Florença Renascentista
# 
# 
# ```{r echo=TRUE, message=FALSE, warning=FALSE, results='hide'}

florence <- read_csv(here("data", "florentine.csv")) %>% 
column_to_rownames("FAMILY") %>%
as.matrix()

# ```
# 
# O objeto `florence` retrata uma matriz de adjacência, cujos elementos representam
# a relação entre duas unidades presentes nas linhas e colunas. Nesse caso, tratam-se de 16
# famílias da elite Florentina e a existência de casamentos entre elas.
# 
# [Robust Action and the Rise of the Medici, 1400-1434](http://www.stats.ox.ac.uk/~snijders/PadgettAnsell1993.pdf)
# 
# ## Rede de Casamentos na Florença Renascentista
# 
# - Essa matriz de adjacência representa uma rede não direcionada. Poderíamos adicionar
# direção caso tivéssemos, por exemplo, a informação de qual família propôs o casamento.
# 
# - Sem direcionamento, tanto as linhas quanto as colunas apresentam as mesmas informações:
# 
# ```{r echo=TRUE, message=FALSE, warning=FALSE, results='hide'}

rowSums(florence)
colSums(florence)

# ```
# 
# ## Rede de Casamentos na Florença Renascentista
# 
# ```{r echo=FALSE, message=FALSE, warning=FALSE, results='hide'}
set.seed(123)
florence_ntw <- florence %>% graph.adjacency(mode = "undirected", diag = FALSE, 
                       add.colnames = NULL,
                       add.rownames = NULL) 

plot(florence_ntw)
# ```
# 
# <center>
# 
# ![florence](images/florence-ntw.png){width=600px}
# </center>
# 
# ## Rede de Casamentos na Florença Renascentista
# 
# Percebam que a rede é composta por *nodes* (nós/vértices) e *edges* (arestas), sendo esse 
# o seu formato clássico de visualização. Com eles, podemos obter as principais medidas 
# da análise de redes,  as medidas de centralidade:
# 
# - **degree**: medida mais elemntar. Representa quanto um nó é conectado aos demais. Ela simplesmente
# computa o número de *edges* que cada nó possui.
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE, results='hide'}
degree(florence_ntw)
# ```
# 
# ## Rede de Casamentos na Florença Renascentista
# 
# - **farness**: computa a soma de edges de um dado nó para todos os demais nós da rede. 
# Descreve quão isolado cada node está dos demais.
# 
# - **closeness**: a distância entre dois nós é representada pelo número de edges que os 
# conectam pelo menor caminho possível. A medida closeness retrata quantas etapas um 
# nó específico precisa para acessar todos os outros nós da rede.
# 
# $$
# closeness(v) = \frac{1}{farness(v)} = \frac{1}{\sum_{u \in V, u\neq v}\text{distância entre} \quad v  \quad \mathrm{e} \quad u}
# $$
# 
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE, results='hide'}
closeness(florence_ntw)
# ```
# 
# ## Rede de Casamentos na Florença Renascentista
# 
# $$
# closeness(v) = \frac{1}{farness(v)} = \frac{1}{\sum_{u \in V, u\neq v}\text{distância entre} \quad v  \quad \mathrm{e} \quad u}
# $$
# Uma outra forma de olha para a centralidade é dividir **farness** pelo número de demais 
# nós na rede. Podemos ver, por exemplo, que, em média, a família Medici está a 2,7 edges de cada
# nó da rede.
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE, results='hide'}
1 / (closeness(florence_ntw) * (dim(florence)[1]-1))
# ```
# 
# ## Rede de Casamentos na Florença Renascentista
# 
# - *betweenness*: nesse caso, um nó é considerado central na medida em que ele permite 
# conectar outros nós.
# 
# $$
# betweenness(v) = \sum_{(t, u) \in V, t\neq u, u \neq v}\frac{\text{n de caminhos que contêm o nó} \quad v}{\text{n de caminhos entre} \quad t \quad e \quad u}
# $$
# 
# Vemos que a família Médici se localiza no menor caminho entre mais de 45% de todos os possíveis caminhos entre os outros nós.
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE, results='hide'}
betweenness(florence_ntw)
# ```
# 
# ## Exercício:
# 
# Com o resultado de `betweenness(florence_ntw)`, produza um gráfico de barras.
# 
# ## Exercício: resposta


# ## Rede de Casamentos na Florença Renascentista
# 
# Comparando medidas de centralidade:
# 
# ```{r, figures-side, echo=FALSE, warning=FALSE, message=FALSE, fig.show="hold", out.width="45%"}
set.seed(123)
par(mar = c(4, 4, .1, .1))
plot(florence_ntw, vertex.size = closeness(florence_ntw) * 1000)
plot(florence_ntw, vertex.size = betweenness(florence_ntw))

# ```
# 
# ## Rede de Casamentos na Florença Renascentista
# 
# <center>
# 
# ![undir](images/undir-ntw-dcb.png){width=600px}
# 
# </center>
# 
# ## Directed Network
# 
# ## Twitter Following Network
# 
# ```{r echo=TRUE, message=FALSE, warning=FALSE, results='hide'}
# # carregando dados
twitter <- read_csv(here("data", "twitter_following.csv"))
senator <- read_csv(here("data", "twitter_senator.csv"))

# ```
# 
# - Objeto `twitter`: temos a informação dos senadores que cada senador segue.
# - Objeto `senator`: informações sobre cada senador.
# 
# 
# 
# ## Twitter Following Network
# 
# Vamos criar nossa matriz de adjacência com o objeto `twitter`. Percebam que ela é assimétrica.
# 
# ```{r echo=TRUE, message=FALSE, warning=FALSE, results='hide'}
twitter_adj <- graph_from_edgelist(as.matrix(twitter)) 
# 
# ```
# 
# ## Twitter Following Network
# 
# Em redes direcionadas como essa, podemos analisar as medidas já vistas. Porém, agora
# temos dois tipos de **degree**, por exemplo:
# 
# 1. **Indegree**: a soma de edges que chegam a um nó. Em outras palavras, o número de seguidores.
# 2. **Outdegree**: a soma de edges que saem de um nó. Ou seja, o número de nós que um determinado nó segue.
# 
# 
# <center>
# 
# ![dir-d](images/dir-ntw-d-inout.png){width=600px}
# 
# </center>
# 
# ## Twitter Following Network
# 
# Podemos calcular as duas medidas e atribuí-las ao nosso objeto `senator`.
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE, results='hide'}
senator <- senator %>% 
mutate(indegree = igraph::degree(twitter_adj, mode = "in"),
outdegree = igraph::degree(twitter_adj, mode = "out"))

# ```
# 
# ## Twitter Following Network
# 
# Agora temos dois tipos de **closeness**: **indegree** e **outdegree**.
# 
# <center>
# 
# ![dir-c](images/dir-ntw-c-inout.png){width=600px}
# 
# </center>
# 
# ## Twitter Following Network
# 
# Podemos calcular as duas medidas e atribuí-las ao nosso objeto `senator`.
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE, results='hide'}
senator <- senator %>%
mutate(closeness_in = igraph::closeness(twitter_adj, mode = "in"),
closeness_out = igraph::closeness(twitter_adj, mode = "out")) 

# ```
# 
# ## Twitter Following Network
# 
# Para **betweenness**, os atributos **indegree** e **outdegree** não fazem diferença, 
# pois a chegada ou a saída de edges de um nó não altera sua presença no caminho da rede. 
# No entanto, podemos calcular a medida considerando ou não a direcionalidade da rede como um todo.
# 
# <center>
# 
# ![dir-c](images/dir-ntw-b-dir.png){width=350px}
# 
# </center>
# 
# ## Twitter Following Network
# 
# Podemos calcular as duas medidas e atribuí-las ao nosso objeto `senator`.
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE, results='hide'}
senator <- senator %>%
mutate(betweenness_dir = igraph::betweenness(twitter_adj, directed = TRUE),
betweenness_undir = igraph::betweenness(twitter_adj,
                           directed = FALSE)) 
# ```
# 
# 
# ## Exercício:
# 
# Com o código abaixo, plote a rede com a função `plot()` e calcule a `betweenness` da rede para `directed = FALSE` e 
# `directed = TRUE`:
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE, results='hide'}
team <- data.frame(name=c("Alice", "Maria", "Rafael", "Davi"),
age=c(48,33,45,34),
gender=c("F","M","F","M"))
relations <- data.frame(from=c("Maria", "Maria", "Rafael", "Alice", "Davi",
         "Rafael"),
  to=c("Alice", "Rafael", "Maria", "Rafael", "Rafael", 
       "Davi"))

net_dir <- graph_from_data_frame(relations, directed=TRUE, vertices=team)
# ```
# 
# 
# ## Exercício: resposta
#
# <!---
# --->
# 
# ## Twitter Following Network
# 
# Voltando ao exemplo do Twitter:
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE}
senator %>%
mutate(betweenness_dir = igraph::betweenness(twitter_adj, directed = TRUE),
betweenness_undir = igraph::betweenness(twitter_adj,
                           directed = FALSE)) %>%
ggplot(aes(x = betweenness_dir, y = betweenness_undir, colour = party,
shape = party)) +
geom_point() +
scale_color_manual(values = c("#0066ff", "#009900", "#ff3300")) +
labs(main = "Betweenness", x = "Directed", y = "Undirected")

# ```
# 
# ## Twitter Following Network
# 
# <center>
# 
# ![google](images/google.png){width=600px}
# 
# 
# [Page Rank](http://ilpubs.stanford.edu:8090/422/)
# 
# </center>
# 
# 
# ## Twitter Following Network
# 
# **Page Rank**: cada nó aloca votos para nós com ele conectados.
# 
# ```{r echo=TRUE, eval = FALSE, message=FALSE, warning=FALSE}
senator <- senator %>% mutate(page_rank = page_rank(twitter_adj)[["vector"]]) %>%
  arrange(desc(page_rank))

# ```
# 
# ## Turbinando nossa rede
# 
rm(list=ls())

# ```{r echo=FALSE, eval = TRUE, message=FALSE, warning=FALSE}
twitter <- read_csv(here("data", "twitter_following.csv"))  
senator <- read_csv(here("data", "twitter_senator.csv")) 

twitter <- twitter %>% rename(source = following,  target = followed)
senator <- senator %>%   rename(source = screen_name)

twitter_adj <- graph_from_edgelist(as.matrix(twitter))

pr <- page_rank(twitter_adj)[["vector"]]
pr <- tibble(source = names(pr), page_rank = pr*1000)

senator <- senator %>% left_join(pr)

# trocando objetos
vertices <- senator

senator <- senator %>% 
mutate(source.index = c(0:(dim(senator)[1]-1)))

twitter_following <-  twitter %>% left_join(senator) 

senator <- senator %>% 
mutate(target.index = c(0:(dim(senator)[1]-1))) %>%
select(-source.index)

twitter_following <-  twitter_following %>% left_join(senator, by = c("target" = "source")) 

net_sen <- forceNetwork(Links = twitter_following, Nodes = senator,
            Source = 'source.index', 
            Target = 'target.index',
            NodeID = 'name',
            Nodesize = 'page_rank',
            Group = 'party', # color nodes by group calculated earlier
            charge = -300, # node repulsion
            linkDistance = 200,
            zoom = T, 
            opacity = 1,
            fontSize=14)

show(net_sen)
# ```
# 
# 
# ## Material adicional
# 
# - [Social Network Visualization in R](http://curleylab.psych.columbia.edu/netviz/netviz1.html#/)
#                           - [Katherine (Katya) Ognyanova](https://kateto.net/)
#                           
#                           ## Tarefa da aula
#                           
#                           As instruções da tarefa estão no arquivo `NN-class-ds4ir-assignment.rmd` da pasta 
#                           `assignment` que se encontra na raiz desse projeto.
#                           
#                           
#                           
#                           
#                           
#                           
#                           
#                           
#                           
#                           
#                           
#                           