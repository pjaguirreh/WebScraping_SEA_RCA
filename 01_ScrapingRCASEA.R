library(rvest)
library(tidyverse)
library(htmltab)
library(stringr)
library(openxlsx)
library(tictoc)

## Loop para extraer el número de pp para cada región
## Se elimina 1 a cada region debido a que si hay menos de 10 RCA en la pagina el loop falla (la última se hará manual)
nro_pp <- rep(0, 17)
for (i in c(1:16, 2420135)){
  texto <- read_html(paste0("https://seia.sea.gob.cl/busqueda/buscarProyectoAction.php?nombre=&regiones=", i, "&_paginador_fila_actual=1")) %>% 
    html_nodes("#info_resultado") %>% 
    html_text()
  
  nro_pag <- substr(substr(texto,  nchar(texto)-9, nchar(texto)), 1, 3) %>%
    as.numeric()
  
  print(nro_pag)
  
  if (i == 2420135){
    nro_pp[17] <- nro_pag - 1
  } else {
    nro_pp[i] <- nro_pag - 1
  }
}

## Función para extraer id RCA de ruta que resulta dentro del 
## loop (siguiente paso)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 


###
## 1. Extracción de Id RCA desde web del SEA
###

tic()
#listtotal <- list()
for (r in c(1:16, 2420135)) { # loop por región
  
  if (r == 2420135){
    n <- nro_pp[17]
  } else {
    n <- nro_pp[r]
  }
  
listtab <- vector("list", length = n) # Almacenará resultados
  for (i in 1:n) { # loop por pagina (cada pag con 10 RCA maximo)
    tab <-
      paste0(
        # "r" recorre las 16 regiones e "i" recorre cada una de las pp
        "http://seia.sea.gob.cl/busqueda/buscarProyectoAction.php?nombre=&regiones=", r, "&_paginador_fila_actual=",i)
    
    # Para cada una de las pags recorremos cada una de las 10 RCAs extrayendo 
    # su Id (como esta el código ahora tira error en la última pag si hay menos de 
    # 10 RCA... entonces se hace manualmente la última)
    id <- vector() # almacenará los 10 ids de cada pag
    for (j in 1:10) {
      idSea <- tab %>% # expresión que define la pag donde se hará la extracción
        read_html() %>%
        html_nodes(
          # El siguiente extracto se obtiene al hacer la inspección del nombre de 
          # la RCA que "enmascara" la ID que queremos. j en este caso es el valor 
          # que va de 1 a 10 para extraer las info de cada una de als 10 RCA
          paste0(
            '#main > div.post > div.texto > div > table > tbody > tr:nth-child(',
            j,
            ') > td:nth-child(2) > a:nth-child(1)'
          )
        ) %>%
        # arreglos para extraer el número de interés:
        html_attrs() %>%
        `[[`(1) %>%
        `[`(2) %>%
        numextract()
      
      # se almacena el resultado de cada iteración de RCA
      id[j] <- idSea
      
      print(paste("reg: ", r, "/", "pag: ", i, "/", "id: ", j)) # para seguimiento
    }
    
    # se almacena el resultado de cada iteración de pag
    #listid[[i]] <- as.data.frame(id)
    
    t <- tab %>% 
      htmltab() %>% 
      select(-N) %>% 
      cbind(as.data.frame(id), .)
    
    names(t) <- c("IdSea", "Nombre", "Tipo", "Región", "Tipología", 
                  "Titular", "Inversión (MMUSD)", "Fecha Presentación/Ingreso", "Estado")
    
    listtab[[i]] <- t
  }
  tabreg <- bind_rows(listtab) %>% 
    mutate(`Inversión (MMUSD)` = as.numeric(str_replace(str_remove(`Inversión (MMUSD)`, "\\."), ",", "."))) %>% 
    as_tibble()
  openxlsx::write.xlsx(tabreg, paste0("RCA/RCA_Region_", r, ".xlsx"))
}
toc()



