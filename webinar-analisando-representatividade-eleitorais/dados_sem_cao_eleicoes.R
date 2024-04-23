#analisando os dados do tse rj, 2020
###carregando as bases###
cand <- read.csv("cand_rj_2020_2.csv", colClasses = "character")
elei <- read.csv("eleitos_rj_2020.csv", colClasses = "character")
muni <- read.csv("codigos_municipais.csv", sep=";", colClasses = "character")

#retirando as candidaturas que não id_bd
cand <- subset(cand, id_candidato_bd!="")
elei <- subset(elei, id_candidato_bd!="")

#atualizando o nome dos municípios
#consultando as variáveis que tem na base dos municipios
names(muni)

##deixando só as 2 variáveis que tenho interesse (código e nome)
muni <- muni[,c(12,13)]

######JUNTANDO AS COISAS######
#1) dando o merge da base com os candidatos com a de municípios
cand <- merge(cand, muni, by.x="id_municipio", by.y = "Código.Município.Completo",
              all.x = T, all.y = F, incomparables = NA)
#note que a base de candidatos ganhou mais uma variável

#a raça negra já está pronta?
table(cand$raca)
##quem for parda ou preta tem que virar negra
#transformando parda em negra
cand$raca[which(cand$raca=="parda")] <- "negra"
#transformando preta em negra
cand$raca[which(cand$raca=="preta")] <- "negra"

#repete a tabela e reveja o somatório
table(cand$raca)

#juntando a variável gênero e raça
cand$gen_raca <- paste(cand$genero, cand$raca, sep = "_")
#olhando uma tabela para ver como ficou
table(cand$gen_raca)


#####FAZENDO A NOSSA PRIMEIRA TABELA DE ANÁLISE####
tnegras <- table(cand$Nome_Município, cand$gen_raca)
tnegras <- as.data.frame.matrix(tnegras)
###OLHE#### View(tnegras)

#tirando o nome dos municípios para uma coluna
tnegras$municipio <- row.names(tnegras)

#####AQUI JÁ TEMOS A PRIMEIRA ANÁLISE#####
#podemos exportar essa base em um arquivo csv
write.csv(tnegras, "candidatos_gen_raca_rj_2020.csv", row.names = F)

##########PENSANDO NOS RESULTADOS DAS ELEIÇÕES###########
#o que temos de resultados?
table(elei$resultado)
#eleito, eleito por media, eleito por qp

#deixando SÓ os eleitos
elei <- subset(elei, resultado=="eleito" | resultado =="eleito por media" | resultado=="eleito por qp")

#fazendo merge dos cand com eleit
total <- merge(cand, elei, by="id_candidato_bd", all.x = F, all.y = T, incomparables = NA)

####A TABELA DOS ELEITOS POR MUNICÍPIO#####
t2 <- table(total$Nome_Município, total$gen_raca)
t2 <- as.data.frame.matrix(t2)

#tirando o nome dos municípios para uma coluna
t2$municipio <- row.names(t2)
###OLHE#### View(t2)

#exportando essa tabela
write.csv(t2, "eleitos_gen_raca_rj_2020.csv", row.names = F)

#pra juntar pelo nome do município
#olhando as variáveis que eu quero de fato
names(t2)
#deixando apenas os municipios e o total de mulheres negras eleitas
t2 <- t2[,c(9,4)]
#tirando o nome das linhas
row.names(t2) <- NULL

#fazendo igual pra de candidatas
#olhando as variáveis que eu quero de fato
names(tnegras)
#deixando apenas os municipios e o total de mulheres negras eleitas
tnegras <- tnegras[,c(11,5)]
#tirando o nome das linhas
row.names(t2negras) <- NULL

#finalmente juntando com a tabela de candidatas
final <- merge(t2, tnegras, by="municipio")
#renomeando para não confundir
names(final) <- c("municipio", "eleitas negras", "candidatas negras")

#exportando a base final
write.csv(final, "cand_eleit_gen_raca_2020_rj.csv", row.names = F)

#coeficiente de eleicao de mulheres negras
final$coeficiente <- final$`eleitas negras`/final$`candidatas negras`

final$coeficiente <- final$coeficiente*100
