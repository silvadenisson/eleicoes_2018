# donwload dados urnas

# abrindo pacotes
library(rvest)
library(stringr)

url_base <- "http://agencia.tse.jus.br/estatistica/sead/eleicoes/eleicoes2018/buweb/"

link <- read_html(url_base) %>% html_nodes(xpath = "//a") %>% html_text() %>% .[seq(6,58, 2)]

arquivo <- link[str_detect(link, "RJ")]

download.file(paste0(url_base, arquivo), paste0("data/", arquivo))

unzip(paste0("data/", arquivo), exdir = "data")

