# Grim-Scrapping
Script em R para raspar dados do jogo Grim Dawn

Grim Dawn é um jogo de RPG com um sistema para melhorar os atributos ou habilidades dos personagens chamado de Constelações. Você tem uma quantidade de pontos para alocar entre as constelações e cada uma delas dá um bônus específico. 

Além disso, todas essas constelações têm alguns requisitos. Por exemplo, você precisa ter pelo menos um ponto utilizado em Primordial antes de poder utilizar as constelações com este requisito. O link a seguir mostra melhor essa ideia: https://www.grimtools.com/calc/

O projeto raspa então todos os os dados dessas constelações: quais os bônus, custos e requisitos de cada uma e coloca numa formatação mais amigável. Depois disso, resolvemos o problema de escolher uma constelação de nosso interesse gastando o mínimo possível de pontos para atender a seus requisitos. A raspagem de dados é feita a partir do site http://grimdawn.wikia.com/wiki/Dying_God



Inicialmente tenho apenas o script com a formatação dos dados e a minimização dos custos juntas e o próprio arquivo RData com o resultado.

Fiz esse script seguindo os passos do artigo https://www.ibpad.com.br/blog/webscraping-ou-como-raspar-todas-receitas-de-bolo-de-cenoura/
