# fazer mapas com resulttado por secao eleitoral 
# bolsonaro, haddad, bancos, nulos e abstencoes

# limpando a area
rm(list = ls())

# Carregando pacotes
library(readr)
library(data.table)
library(stringr)
library(sf)
library(ggplot2)

# carregando dados de minas gerais
mg <- read_delim("data/bweb_1t_MG_101020181954.csv", delim = ";", locale = locale(encoding = "latin1"))

# separando dado de presidente em BH
bh_pres <- mg[mg$NM_MUNICIPIO == "BELO HORIZONTE" & mg$DS_CARGO_PERGUNTA == "Presidente",] %>% data.table()

# somando total de votos dos candidatos por zona
bh_pres <- bh_pres[, .(Total_cand = sum(QT_VOTOS)), by = list(NR_PARTIDO, SG_PARTIDO, NM_PARTIDO, NM_VOTAVEL,
                                                         DS_CARGO_PERGUNTA, NR_ZONA, CD_MUNICIPIO, NM_MUNICIPIO)]
rm("mg")
gc()

# antes de Lucas Gelape enviar os locais de votacao georeferenciados
# carregando dados locais de votacoes disponibilizado pelo tse
locais <- read.csv2("data/Locais_Votacao_tse.csv", encoding = "latin1", stringsAsFactors = F)
locais <- locais[locais$UF == "MG" & locais$NM_MUNIC_TSE == "BELO HORIZONTE", ]

# baixar os dados geocoder da prifeitura de bh
# primeira tentativa coletar por cep, nao coletou tudo
# a segunda tentativa por nome do bairro tb nao coleto tudo
# entao juntei as duas tentativas
cep <-  unique(locais$NUM_CEP)

geo <- NULL
erro <- NULL # para coletar por nome os enderecos que faltaram por cep
for(i in 1:length(cep)){
  url_geo <- gsub("CEP", cep[i] ,"http://geocoder.pbh.gov.br/geocoder/v2/address?cep=CEP")
  4
  tab <- as.data.frame(jsonlite::fromJSON(url_geo))
  if(length(tab)!=16){
    err = i
    erro <- rbind(erro, err)
  }else{
    geo <- rbind(geo, tab)
  }
  
  print(i)
}

# segunda tentativa
# padroniando barri
locais$NM_BAIRRO2 <- locais$NM_BAIRRO  %>% ifelse(str_detect(., "\\("), 
                                                  str_sub(., end = str_locate(.,"\\(")[,2] - 2), .)
locais2 <- locais[locais$NUM_CEP %in% cep[erro], ]
bairro <- unique(locais2$NM_BAIRRO2)
bairro <- gsub(" ", "%", bairro)

geo2 <- NULL
erro2 <- NULL
for(i in 1:length(bairro)){
  url_geo <- gsub("NOME", bairro[i] ,"http://geocoder.pbh.gov.br/geocoder/v2/address?bairro=NOME")
  
  tab <- as.data.frame(jsonlite::fromJSON(url_geo))
  if(length(tab)!=16){
    err = i
    erro2 <- rbind(erro2, err)
  }else{
    geo2 <- rbind(geo2, tab)
  }
  
  print(i)
}


geo <- rbind(geo, geo2)

# lipando area
rm(i, geo2, tab, locais2, erro, err, erro2, cep)

#padronizando variaveis para o merge
# esse proceidmento foi feito assim pra aprofeitar um shap file que ja tinha da regioes de bh
locais$NUM_CEP <- as.character(locais$NUM_CEP)
geo$endereco.bairropopular <- toupper(geo$endereco.bairropopular)

locais2 <- merge(locais, geo, by.x = c("NM_BAIRRO2"), by.y = c("endereco.bairropopular"), all.x = T)
locais3 <- merge(locais, geo, by.x = c("NUM_CEP"), by.y = c("endereco.cep"), all.x = T)

locais4 <- rbind(locais2[, c("NM_BAIRRO2", "endereco.nomeregional", "NUM_CEP", "SECAO", "ZONA") ],
                 locais3[, c("NM_BAIRRO2", "endereco.nomeregional", "NUM_CEP", "SECAO", "ZONA") ]) 

locais4 <- locais4[!is.na(locais4$endereco.nomeregional), c(2, 5)]
locais4 <- locais4[!duplicated(locais4),]

rm(locais2, locais3)

bh_pres <- as.data.frame(bh_pres)
bh_pres1 <- merge(bh_pres, locais4, by.x = c("NR_ZONA"),  by.y =  c("ZONA"), all.x = T)
names(bh_pres1)[10] <- "NM_SUBDIST"

# agregando os dados com regiao de bh
bh_pres1 <- aggregate(Total_cand ~ NR_PARTIDO + SG_PARTIDO + NM_PARTIDO + NM_VOTAVEL +
                        DS_CARGO_PERGUNTA + CD_MUNICIPIO + NM_MUNICIPIO + NM_SUBDIST, data = bh_pres1, sum)

shp <- st_read("data/Shapes_bh/RegionaisBH.shp")

shp$NM_SUBDIST <- as.character(shp$NM_SUBDIST)

# merge das base com shap
shpp <- merge(shp, bh_pres1, by = "NM_SUBDIST", all.x = T)

# plotando os graficos
ggplot() + geom_sf(data = shpp[shpp$NM_VOTAVEL == "FERNANDO HADDAD",], aes(fill = Total_cand)) + 
  scale_fill_continuous( high= "#132B43",  low = "#56B1F7") + theme_classic() + labs(title = "Votação HADDAD - BH")
ggsave("hadda_bh.png", width = 4.2, height = 6.67)

ggplot() + geom_sf(data = shpp[shpp$NM_VOTAVEL == "JAIR BOLSONARO",], aes(fill = Total_cand)) + 
  scale_fill_continuous( high= "#132B43",  low = "#56B1F7") + theme_classic() + labs(title = "Votação BOLSONARO - BH")
ggsave("bolsonaro_bh.png", width = 4.2, height = 6.67)

ggplot() + geom_sf(data = shpp[shpp$NM_VOTAVEL == "Nulo",], aes(fill = Total_cand)) + 
  scale_fill_continuous( high= "#132B43",  low = "#56B1F7") + theme_classic() + labs(title = "Nulo - BH")
ggsave("Nulo_bh.png", width = 4.2, height = 6.67)

ggplot() + geom_sf(data = shpp[shpp$NM_VOTAVEL == "Branco",], aes(fill = Total_cand)) + 
  scale_fill_continuous( high= "#132B43",  low = "#56B1F7") + theme_classic() + labs(title = "Branco - BH")
ggsave("Branco_bh.png", width = 4.2, height = 6.67)

