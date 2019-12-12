setwd("//Higuera/DGI/Proyectos/Inteligencia de negocios/Programa de fiscalización/Datos")

reg <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII", "XIV", "XV", "XVI")

for (i in reg){
  x <- readxl::read_excel(paste0("RCA_Region_", i, ".xlsx"))
  y <- readxl::read_excel(paste0("RCA_Region_", i, "_2.xlsx"))
  
  z <- bind_rows(x, y)
                          
  openxlsx::write.xlsx(z, paste0("RCA_Region_", i, "_final.xlsx"))
}


## Cargar todas las planillas de región (menos Los Rios)

listfinal <- list()
contar <- 1
for (i in reg){
  x <- readxl::read_excel(paste0("RCA/RCA_Region_", i, "_final.xlsx"))
  listfinal[[contar]] <- x
  contar <- contar + 1
}


tabla_final <- bind_rows(listfinal)

tabla_final2 <- tabla_final %>% 
  separate(`Inversión (MMUSD)`, into = c("entero", "decimal"), sep = ",", remove = FALSE) %>% 
  mutate(
    entero2 = as.numeric(entero),
    entero2 = as.character(ifelse(str_detect(entero, "[[:punct:]]"), entero2 * 1000, entero2))
  ) %>% 
  unite("numero", "entero2", "decimal", sep = ".") %>% 
  mutate(
    `Inversión (MMUSD)` = as.numeric(numero)
  ) %>% 
  select(-numero, -entero) 

openxlsx::write.xlsx(tabla_final2, "RCA/RCA_todaslasregiones.xlsx")
