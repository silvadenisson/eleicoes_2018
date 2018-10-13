# fazer mapas com resulttado usando dados de urna
# maps por setor censitario
# bolsonaro, haddad, bancos, nulos e abstencoes

# limpando a area
rm(list = ls())

# Carregando pacotes
library(readr)
library(data.table)
library(stringr)
library(sf)
library(ggplot2)

# carregando dados de Belo Horizonte
unzip(paste0("data/BWEB_1t_MG_101020181938.zip"), exdir = "data")

mg <- read_delim("data/bweb_1t_MG_101020181954.csv", delim = ";", locale = locale(encoding = "latin1"))

unlink("data/bweb_1t_MG_101020181954.csv")

# separando dado de presidente em BH
mg_pres <- mg[mg$NM_MUNICIPIO == "BELO HORIZONTE" & mg$DS_CARGO_PERGUNTA == "Presidente",] %>% data.table()

secao <- unique(mg_pres[, c(10:15)])

# somando total de votos dos candidatos por zona
mg_pres <- mg_pres[, .(Total_cand = sum(QT_VOTOS)), by = list(NR_PARTIDO, SG_PARTIDO, NM_PARTIDO, NM_VOTAVEL,
                                                         DS_CARGO_PERGUNTA, NR_ZONA, NR_LOCAL_VOTACAO,
                                                         CD_MUNICIPIO, NM_MUNICIPIO)]
rm("mg")
gc()

# carregando dados locais de votacoes georeferenciado pelo lucas gelap
locais <- read.csv2("data/2016_locais_capitais/belohorizonte2016.csv", encoding = "latin1", stringsAsFactors = F)
locais$NUM_SECAO <- NULL

locais <- locais[!duplicated(locais), ]

# merge
mg_pres <- as.data.frame(mg_pres)
mg_pres <- merge(mg_pres, locais, by.x = c("NR_ZONA", "NR_LOCAL_VOTACAO"),  by.y =  c("NUM_ZONA", "NUM_LV"), all.x = T)

na <- mg_pres[is.na(mg_pres$lat),]
na <- unique(na[, 1:2])
# local 1406, secao 239 - zona 26
# COLÉGIO LOGOSÓFICO - UNIDADE CIDADE NOVA, AV. JOSÉ CANDIDO DA SILVEIRA, 330 - SAGRADA FAMÍLIA
#lat / long   
# local 1457 , secao 240, zona 26 
# nao encontrdo no arquivo, colocar junto com a 239
# local 1384, secao 291/293/295/298/301/303, zona 31
# COLEGUIUM - UNIDADE CASTELO MANACÁS, RUA DOUTOR SYLVIO MENICUCCI, 1021 - CASTELO
# ,
# local 1511, secao 176/177/179/183/185/1897191/193, zona 33
# ESCOLA ESTADUAL BARÃO DE MACAÚBAS RUA DAVID CAMPISTA, 42 - FLORESTA
# ,
# local 1635, secao 577, zona 39
# E. M. SERGIO MIRANDA, RUA MINISTRO OSVALDO ARANHA, 345 - TUPI
# ,

mg_pres$lon[mg_pres$NR_ZONA == 26 & mg_pres$NR_LOCAL_VOTACAO == 1406] <- -43.9265441
mg_pres$lat[mg_pres$NR_ZONA == 26 & mg_pres$NR_LOCAL_VOTACAO == 1406] <- -19.8943876
mg_pres$lon[mg_pres$NR_ZONA == 26 & mg_pres$NR_LOCAL_VOTACAO == 1457] <- -43.9265441
mg_pres$lat[mg_pres$NR_ZONA == 26 & mg_pres$NR_LOCAL_VOTACAO == 1457] <- -19.8943876
mg_pres$lon[mg_pres$NR_ZONA == 31 & mg_pres$NR_LOCAL_VOTACAO == 1384] <- -44.0055605
mg_pres$lat[mg_pres$NR_ZONA == 31 & mg_pres$NR_LOCAL_VOTACAO == 1384] <- -19.886122
mg_pres$lon[mg_pres$NR_ZONA == 33 & mg_pres$NR_LOCAL_VOTACAO == 1511] <- -43.9309114
mg_pres$lat[mg_pres$NR_ZONA == 33 & mg_pres$NR_LOCAL_VOTACAO == 1511] <- -19.9161574
mg_pres$lon[mg_pres$NR_ZONA == 39 & mg_pres$NR_LOCAL_VOTACAO == 1635] <- -43.9158876
mg_pres$lat[mg_pres$NR_ZONA == 39 & mg_pres$NR_LOCAL_VOTACAO == 1635] <- -19.8310319


shape <- readOGR("data/RM_Belo_Horizonte/RM_BeloHorizonte_UDH_region.shp")

shape <- shape[shape$NM_MUNICIP == "BELO HORIZONTE", ]

 # Transformar o banco em um shp de pontos
shpp <- mg_pres
coordinates(shpp) <- ~lon+lat
shpp@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(shape)
identicalCRS(shape, shpp) # checar se as projecoes sao as mesmas

# Fazer um spatial join dos dois shp
bh_join <- point.in.poly(shpp, shape)


bh_join2 <- bh_join@data

names(bh_join2)


bh3 <- aggregate(Total_cand ~ UDH_ATLAS +  + REGIONAL + CD_GEOCODM + NM_MUNICIP + NM_VOTAVEL + SG_PARTIDO, data = bh_join2, sum)

shpb <- st_read("data/belo_horizonte_bairros/belo_horizonte_bairros.shp", options = "ENCODING=latin1")


shppnud <- st_read("data/RM_Belo_Horizonte/RM_BeloHorizonte_UDH_region.shp", options = "ENCODING=latin1")
plot(shppnud)
shppnud <- shppnud[shppnud$NM_MUNICIP == "BELO HORIZONTE",]

shpB <- merge(shppnud, bh3, by = c("UDH_ATLAS",  "REGIONAL",  "CD_GEOCODM", "NM_MUNICIP"), all.x = T)

shpB$Total_cand[is.na(shpB$Total_cand)] <- 0
fernando <- shpB
fernando$NM_VOTAVEL[is.na(fernando$NM_VOTAVEL)] <- "FERNANDO HADDAD"
fernando <- fernando[fernando$NM_VOTAVEL ==  "FERNANDO HADDAD", ]
fernando <- as_Spatial(fernando) 

library(spatstat)
sharpen(as.ppp(fernando))

ggplot() +   
  geom_sf(data = fernando, aes(fill = Total_cand), lwd = 0) +
  geom_sf(data = shpb, fill = "transparent", lwd = 0.1)  +
  scale_fill_continuous(limits=c(0, 5000),  high= "#132B43",  low = "#56B1F7", na.value="gray80") + theme_classic() +
  labs(title = "FERNANDO HADDAD - BH")
ggsave("FERNANDO_HADDAD_bairro_bh.png", width = 4.2, height = 6.67)

