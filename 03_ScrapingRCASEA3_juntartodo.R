setwd("//Higuera/DGI/Proyectos/Inteligencia de negocios/Programa de fiscalización/Datos")

reg <- c(1:16, 2420135)

for (i in seq_along(reg)){
  x <- readxl::read_excel(paste0("RCA/RCA_Region_", reg[i], ".xlsx"))
  y <- readxl::read_excel(paste0("RCA/RCA_Region_", reg[i], "_2.xlsx"))
  
  z <- bind_rows(x, y)
   
  print(nrow(z))
  
  openxlsx::write.xlsx(z, paste0("RCA/RCA_Region_", reg[i], "_final.xlsx"))
}


## Cargar todas las planillas de región (menos Los Rios)

listfinal <- vector("list", length = length(reg))
for (i in seq_along(reg)){
  x <- readxl::read_excel(paste0("RCA/RCA_Region_", reg[i], "_final.xlsx"))
  listfinal[[i]] <- x
}


tabla_final <- bind_rows(listfinal)

openxlsx::write.xlsx(tabla_final, "RCA/RCA_todaslasregiones.xlsx")
