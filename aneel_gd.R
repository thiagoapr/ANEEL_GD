# Packages
library("httr")
library("rvest")
library("stringi")
library("tidyverse")

pg <- 365 # número de páginas que serão lidas

base <- NULL

for(i in 1:pg) {

      # Read html
  
      res <- httr::GET(url = paste0("http://www2.aneel.gov.br/scg/gd/gd_fonte_detalhe.asp?tipo=12&pagina=", i))
      
      httr::stop_for_status(res)
      
      out <- httr::content(res, as = "text")
      
      # Remove white space
      
      l <- stri_trim_both(stri_split_lines(out)[[1]])
      
      # Remove blank lines and html character code
      
      l <- l[-c(which(grepl("<", l, fixed = TRUE)), 
                which(l == ""))]
      
      # Exclui as primeiras 10 linhas
      
      l <- data.frame(key = c(rep(0, 8), rep(seq(1, length(l)/16), each= 16)),
                      Variavel = c(rep(0, 8), rep(seq(1, 16), times = length(l)/16)),
                      Dados = l)
      
      l <- l %>% filter(key != 0) %>% mutate(key = key,
                                             Variavel = Variavel,
                                             Dados = as.character(Dados))
      
      l <- l %>% spread(Variavel, Dados)
      
      # Header
      
      names(l) <- c("key", "Distribuidora",	"Codigo",	"Titular", "Classe", "Subgrupo",	"Modalidade",	"UC",
                    "Município",	"UF",	"CEP",	"Conexao",	"Fonte",	"Potencia", "Modulos",	"Inversores",	"Area")

      # Pool
      
      base <- rbind(base, l)
      
}

# Ajusta variáveis numérias

base <- base %>% mutate(UC = as.numeric(gsub(",", ".", gsub("\\.", "", UC))),
                        Potencia = as.numeric(gsub(",", ".", gsub("\\.", "", Potencia))),
                        Modulos = as.numeric(gsub(",", ".", gsub("\\.", "", Modulos))),
                        Inversores = as.numeric(gsub(",", ".", gsub("\\.", "", Inversores))),
                        Area = as.numeric(gsub(",", ".", gsub("\\.", "", Area)))
)


# Ajusta variáveis

base <- base %>% mutate(Modalidade = as.factor(Modalidade))
levels(base$Modalidade) <- c("Autoconsumo remoto",
                             "Condominios",
                             "Geracao compartilhada",
                             "Geracao na propria UC")


base <- base %>% mutate(Classe = as.factor(Classe),
                        Subgrupo = as.factor(Subgrupo))

#save(base, file = "gd_aneel_dez2020.RData")


