#listtotal <- list()
nro_pp2 <- nro_pp + 1
r_final <- c(9,7,6,4,7,10,10,3,7,1,4,2,6,4,5,2)
for (r in c(1:16)) { # loop por región
  listtab <- list() # Almacenará resultados
  n <- nro_pp2[r]
  for (i in n) { # loop por pagina (cada pag con 10 RCA maximo)
    tab <-
      paste0(
        # "r" recorre las 16 regiones e "i" recorre cada una de las pp
        "http://seia.sea.gob.cl/busqueda/buscarProyectoAction.php?nombre=&regiones=", r, "&_paginador_fila_actual=",i)
    
    # Para cada una de las pags recorremos cada una de las 10 RCAs extrayendo 
    # su Id (como esta el código ahora tira error en la última pag si hay menos de 
    # 10 RCA... entonces se hace manualmente la última)
    id <- vector() # almacenará los 10 ids de cada pag
    
    n2 <- r_final[r]
    for (j in 1:n2) {
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
  tabreg <- bind_rows(listtab)
  openxlsx::write.xlsx(tabreg, paste0("RCA_Region_", r, "_2.xlsx"))
}
