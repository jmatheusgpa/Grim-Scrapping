#raspar os dados de devotion do Grim Dawn



#baseado no artigo Web scraping: ou como raspar todas receitas de bolo de cenoura, do IBPAD
#browseURL("https://www.ibpad.com.br/blog/webscraping-ou-como-raspar-todas-receitas-de-bolo-de-cenoura/")

setwd("C:/Users/Zé/Dropbox/R/Projetos/1_Raspagem")
load("Grim_Devotion2.RData")

## PACOTES
library("rvest")
library("stringr")
library("purrr")
library("dplyr")
library("httr")
library("tidyr")
library("optimization")
library("lpSolve")


#==============================================================================
#pegar todos os links para as constelações

links <- read_html("http://grimdawn.wikia.com/wiki/Constellation") %>%
  html_nodes("td > a") %>%
  html_attr("href") %>%
  data.frame()

#os links são apenas os que iniciam com "/wiki/"
links <- str_subset(links[,1], "/wiki/") 

#colando o início do endereço neles
links <- paste0("http://grimdawn.wikia.com", links[]) 

#passo intermediário para filtrar os links
#links <- links[36:37]


#browseURL(links[70])





#o crossroads é diferente porque os bônus dele estão no corpo do texto. Talvez sirva para usar de molde para os demais

#raspar os requisitos e bônus em cada link
raspar_rb <- function(x)
{
  read_html(x) %>%
    html_nodes("td p") %>%
    html_text() %>%
    data.frame()
}

#raspar os nomes dos requisitos e bônus em cada link
raspar_nome_rb <- function(x)
{
  read_html(x) %>%
    html_nodes("td p a") %>%
    html_attr("title") %>%
    data.frame()
}

#raspar os nomes das constelações

raspar_nome_const <- function(x)
{
  read_html(x) %>%
    html_nodes(".page-header__title") %>%
    html_text %>%
    data.frame()
}

#montando os primeiros dfs, todos eles sem contar o crossroads

const <- map_df(links,raspar_rb)


nomes <- map_df(links, raspar_nome_rb)


nomes_const <- map_df(links, raspar_nome_const)


#==============================================================================
#parte 2: transformando os dados num formato ideal



str_detect(const$.[], pattern = "\\d") %>% as.numeric() #esse \\d é o padrão para checar se é número


const$n <- c(1:nrow(const))
detect_aff <- str_detect(const$.[], pattern = "Affinity")

aff <- filter(const, detect_aff) #todas as linhas com Affinity em texto. Então a linha +1 tem a descrição dos pontos

index <- aff  %>% select(n)

num <- slice(const, index$n + 1)



#=====================================
#as duas primeiras obs são do crossroads, então eu vou retirá-las. Vou tirar também a coluna numeros

aff <- slice(aff, 3:nrow(aff)) %>% select(".")
num <- slice(num, 3:nrow(num)) %>% select(".")

colnames(aff) <- "aff"
colnames(num) <- "num"


aff$aff <- str_replace(aff$aff, pattern = "Affinity Required", replacement = "R_")
aff$aff <- str_replace(aff$aff, pattern = "Affinity Bonus", replacement = "B_")

pontos <- bind_cols(aff, num)

#criando ID para facilitar a vida
pontos <- pontos %>% group_by(aff) %>% mutate(id = sequence(n())) 

#daqui posso começar a parte do gather, etc

#================================================================
#tratamento dos dados


pontos2 <- separate(data = pontos,
                    col = "num",
                    into = c("a1","a2","a3","a4", "a5", "a6", "a7", "a8")
)

#acho que precisa ordernar o pontos 2 pela id se não dá ruim

pontos2 <- pontos2 %>%
  gather(pontos, valor, -c("aff","id"), na.rm = T) %>%
  select(-pontos) %>%
  filter(valor!= "") %>%
  arrange(id, desc(aff)) #mostra primeiro os requisitos de cada devotion, depois os bônus






#ideia : não colocar o crossroads, e no final do cálculo colocar que o requisito pode ser +- um ponto para cada atributo
#==============================================================================



#221 linhas, como queria! Agora vamos juntar com a base nomes para descobrir quantos bonus e requisitos de cada devotion precisamos

dev <- bind_cols(pontos2, nomes)  #até aqui tá bom



dev$info <- str_c(dev$aff, dev$., sep = "")


nomes_const$id <- seq(0,78,1) #chave no nome das constelações, començando com zero porque não quero juntar crossroads
colnames(nomes_const)[1] <- "const"




#agora junto tudo
dev <- dev %>% ungroup () %>%
  select(info, valor, id) %>%
  left_join(nomes_const)


#promissor, porém o 15 tá dando erro, continua dando erro mesmo após a nova técnica. Era só ordernar o id na "pontos2" para colar certin.



formatar <- function(x){
filter(dev, id == x) %>%
  ungroup() %>%
  select(info, valor, const, id) %>%
  spread(info, valor) %>%
  data.frame() 
}

#vetor onde vai ter os nomes!

vetor <- c(1:78)

db <- map_df(vetor, formatar) #ainda tem erro no id 35 OBS: corrigido dia 21/03

#======================================================================================================

#tirar as NA para poder somar

db[is.na(db)] <- 0
#brincar um pouco com a db



#ordenar as colunas

db0 <- db[,1:2]
db1 <- db[,3:12]
db1 <- db1[, order(names(db1), decreasing = T)]
db1 <- lapply(db1, as.numeric)

str(db1)

db <- bind_cols(db0,db1)


str(db)


colnames(db)

sum(db[,3]) #soma de todos os valores requeridos de primordial

max(db[,3]) #valor máximo de primordial requerido




#==============================================================================
#agora é pegar a quantidade de pontos necessários para cada um

#custo em devotion points de cada constelação
raspar_custos <- function(x){
read_html(x) %>%
  html_nodes("td") %>%
  html_text() %>%
  data.frame() %>%
  tail(2) %>%   #pega os dois últimos valores da dataframe
  head(1)       #pega o primeiro destes dois últimos valores
  
}

custos_const <- map_df(links, raspar_custos)
colnames(custos_const)[1] <- "custo"

custos_const$id <- seq(0,78,1) #chave no nome das constelações, començando com zero porque não quero juntar crossroads


#juntando com a db

db0 <- left_join(db0, custos_const)
db <- bind_cols(db0, db1)


#agora temos uma db com o nome das constelações, o custo em pontos de cada uma, os bonus e requisitos
#podemos usar pesquisa operacional para minimizar os custos de obter uma devotion do último tier
#==============================================================================
#Otimização

#criando matriz para facilitar a montagem do problema no lpSolver

db_bonus <- select(db, -c(4:8))

#vou deixar o nome da coluna como o nome da constelação, se ficar ruim eu volto para ajustar nessa linha!

db_bonus <- select(db_bonus, -2)

#transpor a matriz
db_bonus <- t(db_bonus) %>% as.data.frame()


#=====================================================================================
#testando a minimização
#devotion escolhida: light of empyrion, id = 71

#as restrições estão nas colunas 4 a 8 da db
f.rhs <- filter(db, id == 69)[4:8]  %>% as.numeric()

#a direção da otimização é sempre maior ou igual. E são 5 direções porque são 5 restrições
f.dir <- c(rep(">=", 5))

#a função objetivo é a linha de custos da db_bonus
f.obj <- slice(db_bonus, 2) %>% as.numeric()

#os bonus estão nas linhas 3 a 7 da db_bonus
f.con <- slice(db_bonus, 3:7) %>% as.matrix()

lpsol <- lp("min", f.obj, f.con, f.dir, f.rhs, all.bin = T)
lpsol
resultado <- as.data.frame(lpsol$solution)

resultado <- resultado %>% mutate(id = sequence(n()))
colnames(resultado)[1] <- "compra"

#agora linkando a solução aos nomes já conhecidos
resultado <- left_join(db[,1:2], resultado) %>%
  filter(compra == 1)
resultado

#funciona para o resultado final, porém não calcula os requisitos das constelações iniciais. Arrumar isso depois, mas já ajuda!


#==============================================================================



save.image("Grim_Devotion2.RData")


