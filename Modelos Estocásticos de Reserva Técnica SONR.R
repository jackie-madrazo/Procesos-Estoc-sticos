############################
#     Nota importante     #
###########################
#Código ejecutable en versión R version 3.6.1
#Para ejecutar el código debes dar Ctrl + Enter
#Si se quiere ejecutar el código para el modelo estocástico en un solo paso, seleccionar de la línea 16-350 y seguir instrucciones de la línea 352
#Si se quiere ejecutar el código para el modelo determinístico en un solo paso, seleccionar de la línea 364-439 y seguir instrucciones de la línea 441

############################
# Lee Tabla de Siniestros #
###########################

#La ruta se debe actualizar a la carpeta donde se encuentran los archivos descargados a utilizar.(3 archivos descargables)
#OJO: Debe estar separado por "/" y agregar al final de la carpeta donde se encuentran un "/". 
#Ejemplo:  "D:/Usuarios/Procesos Estocásticos/3.- Parcial/Proyecto/"
ruta <- "D:/Usuarios/430006716/Documentos/Actuaria/9no Semestre/Procesos Estocásticos/3.- Parcial/Proyecto/Triángulos/2019-12/"

#Selecciona los archivos por utilizar (no se requiere editar ninguna línea a partir de aqui)
arch_primas_dev <- paste(ruta, "primas_dev_201912.csv", sep = "")
arch_siniest <- paste(ruta, "triangulos_siniestros_201912.csv", sep = "")
arch_atipicos <- paste(ruta, "siniestros_atipicos_201912.csv", sep = "")

#Lee los archivos 
primas_dev <- read.csv(file=arch_primas_dev, header=TRUE, sep=",")
siniest <- read.csv(file=arch_siniest, header=TRUE, sep=",")
atipicos <- read.csv(file=arch_atipicos, header=TRUE, sep=",")

#El rango se actualiza dependiendo de la fecha de valuación, para este ejemplo se utilizara 7:102
primas_dev$tipocambio[primas_dev$moneda=='MXP'] <- 1
primas_dev[,7:102] <- primas_dev$tipocambio * primas_dev[,7:102]

# Convierte a pesos la base de siniestros
siniest$tipocambio[siniest$moneda=='MXP'] <- 1
siniest[,7:10] <- siniest$tipocambio * siniest[,7:10]


#############################################
#           PROCESO ESTOCÁSTICO            #
#############################################

#Este proceso de cálculo de reservas, tiene una estimación mensual con una periodicidad agrupada de forma trimestral y conforme van transcurriendo los periodos de
#nuestro muestreo, se van afectando por la información de siniestralidad a la fecha de estimación. Para el método de simulación, a partir de la matriz de 
#siniestralidad acumulada se obtiene la matriz de factores de siniestralidad acumulada. Los diferentes estados que conforman las simulaciones de los factores 
#de siniestralidad son el incremento, disminución o estabilización del desarrollo de los siniestros acotados por la línea de negocio, periodos y/o antigüedad.

genera_primas <- function(compañia, corte, periodicidad){
  
  inicio = (floor(corte/100)-5+floor((100*(corte/100-floor(corte/100))+1)/12))*100 + (100*(corte/100-floor(corte/100))+1)%%12
  
  periodos = c();  periodos[1] <- inicio;  i <- 2
  repeat{
    periodos[i] <- ifelse(periodos[i-1]-100*floor(periodos[i-1]/100)==12,100*(floor(periodos[i-1]/100)+1)+1,periodos[i-1]+1)
    if(periodos[i]==corte){break}
    i <- i+1
  }
  
  columnas <- paste('pr_dev_',periodos,sep="")
  
  primas_1 <- subset(primas_dev, (cia == compañia))
  
  primas_2 <- data.frame(colSums(primas_1[columnas]))
  
  for(i in 1:nrow(primas_2)){
    primas_2$periodo_mensual[i] <- nrow(primas_2)-i+1
  }
  
  num_per = ifelse(periodicidad=="M", 60, ifelse(periodicidad=="T", 20, ifelse(periodicidad =="A", 5,0)))
  
  for(i in 1:num_per){
    k = ifelse(periodicidad=="M", i, ifelse(periodicidad =="T", i*3-2, i*12-11))
    t = ifelse(periodicidad=="M", 0, ifelse(periodicidad =="T", 2, 11))
    primas_2$periodo[primas_2$periodo_mensual >= k & primas_2$periodo_mensual <= k+t] <- i
  }
  
  primas_3 <- aggregate(primas_2[1], by=list(Category=primas_2$periodo), FUN = sum)
  
  return(primas_3[order(primas_3$Category, decreasing = TRUE),])
  
}

############################
# Generación de Triángulos #
############################

genera_triangulos <- function(compañia, corte, tipo, atipicos, periodicidad){

  inicio = (floor(corte/100)-5+floor((100*(corte/100-floor(corte/100))+1)/12))*100 + (100*(corte/100-floor(corte/100))+1)%%12
  
  siniest_0 <- subset(siniest, (cia == compañia) & (año_mes_ocu >= inicio) & (año_mes_ocu <= corte) & (año_mes_mov <= corte))
  
  if(atipicos==0){
    siniest_0 <- subset(siniest_0, atipico==0)
  }
  
  columnas_sin <- c("año_mes_ocu", "año_mes_mov")
  if (tipo=="OCU") { 
    columnas_sin[3] <- "monto_ocu"
  } else if (tipo=="PAG") {
    columnas_sin[3] <- "monto_pag"
  } else if  (tipo=="GTO") {
    columnas_sin[3] <- "monto_gto"
  } else {
    columnas_sin[3] <- "monto_syr"
  }
  
  siniest_1 <- siniest_0[columnas_sin]
  
  names(siniest_1) <- c("año_mes_ocu", "año_mes_mov", "monto")
  
  periodos <- c(inicio)
  for (i in 2:60){
    periodos[i] <- periodos[i-1] + ifelse(substr(periodos[i-1],5,7)=="12",89,1)
  }
  
  num_per = ifelse(periodicidad=="M", 60, ifelse(periodicidad=="T", 20, ifelse(periodicidad =="A", 5,0)))
  
  for(i in 1:num_per){
    k = ifelse(periodicidad=="M", i, ifelse(periodicidad =="T", i*3-2, i*12-11))
    t = ifelse(periodicidad=="M", 0, ifelse(periodicidad =="T", 2, 11))
    siniest_1$per_ocu[siniest_1$año_mes_ocu >= periodos[k] & siniest_1$año_mes_ocu <= periodos[k+t]] <- i
    siniest_1$per_mov[siniest_1$año_mes_mov >= periodos[k] & siniest_1$año_mes_mov <= periodos[k+t]] <- i
  }
  
  triangulo_cont <- matrix(0,num_per,num_per)
  for(i in 1:num_per){
    for(j in i:num_per){
      triangulo_cont[i,j] <- sum(subset(siniest_1, per_ocu == i & per_mov == j)$monto)
    }
  }
  
  # Triángulo Incremental
  #-----------------------
  triangulo_inc <- matrix(0,num_per,num_per)
  for(i in 1:num_per){
    for(j in i:num_per){
      triangulo_inc[i,j-i+1] <- triangulo_cont[i,j]
    }
  }
  
  # Triángulo Acumulado
  #---------------------
  triangulo_acum <- matrix(0,num_per,num_per)
  for(i in 1:num_per){
    for(j in i:num_per){
      k = j -i + 1
      if(k == 1) triangulo_acum[i,k] <- triangulo_inc[i,k] else triangulo_acum[i,k] <- triangulo_acum[i,k-1] + triangulo_inc[i,k]
    }
  }
  
  return(triangulo_acum)
  
}

##########################
# Factores de Desarrollo #
##########################

factores_desarrollo <- function(triangulo_acum){
  #triangulo_acum <- triangulo_ocu
  num_per <- nrow(triangulo_acum)
  fd_inc <- matrix(0,num_per-1,num_per-1)
  for(i in 1:(num_per-1)){
    for(j in i:(num_per-1)){
      k = j -i + 2
      fd_inc[i,k-1] <- triangulo_acum[i,k] / triangulo_acum[i,k-1]
    }
  }
  return(fd_inc)
}

calcula_diagonal <- function(triangulo_acum){
  #num_per = nrow(triangulo_acum)
  diagonal <- matrix(0,num_per,1)
  for(i in 1:(num_per)){
    k = num_per - i + 1
    diagonal[i,1] <- triangulo_acum[i,k]
  }
  return(diagonal)
}

############################
# Simulaciones Flujos SONR #
############################

#Con la matriz anterior se realiza una selección aleatoria con reemplazo de acuerdo a una distribución uniforme discreta del conjunto 20 - j +1 factores 
#correspondientes al periodo de desarrollo j para la parte inferior del triángulo, de tal manera que cada escenario debe de estar alimentado de diferentes 
#factores de desarrollo los cuales calcularán distintos escenarios de siniestralidad última.
#Se toma en cuenta dicha distribución ya que cumple con las siguientes características: 
# *El mínimo es fijo
# *El máximo es fijo 
# *Todos los valores en el rango tienen la misma probabilidad de producirse 
# *La distribición uniforme discreta es el equivalente discreto de la distribución uniforme


simula_flujos_sonr <- function(t_acum, fd_inc, pr_dev, sin_atip, num_sim){

  #nsim <- 8023
  
  for (nsim in 1:num_sim){
    
    set.seed(nsim)
    
    
    # Factores de desarrollo incrementales seleccionados #
    num_per = nrow(t_acum)
    
    fd_inc_sim <- matrix(0,num_per,num_per)
    for(i in 1:(num_per-1)){
      for(j in 1:i){
        k = num_per - i + j - 1
        if(k==(num_per-1))
          fd_inc_sim[i+1,k] <- fd_inc[1,k]
        else{
          x <- fd_inc[1:(i-j+1),k]
          x <- x[!is.na(x)]
          x <- x[!is.infinite(x)]
          if(length(x)==0){
            x<-c(1)
          }
          fd_inc_sim[i+1,k] <- sample(x, 1, replace = TRUE)
        }
      }
    }
    
    #factor de cola
    #fd_inc_sim[,num_per] <- (fd_inc_sim[,num_per-1])^2
    fd_inc_sim[,num_per] <- 1
    fd_inc_sim[1,num_per] <- fd_inc_sim[2,num_per]
    
    # Proyección de Flujos #
    t_acum_proy <- cbind(t_acum,matrix(0,nrow=num_per))
    for(i in 1:num_per){
      for(j in (num_per-i+1):num_per){
        t_acum_proy[i,j+1] <- t_acum_proy[i,j] * fd_inc_sim[i,j]
      }
    }
    
    t_inc_proy <- t_acum_proy[,2:(num_per+1)]-t_acum_proy[,1:num_per]
    
    t_inc_proy_cont <- matrix(0,num_per,num_per)
    for (i in 1:num_per){
      for(j in (num_per-i+1):num_per){
        t_inc_proy_cont[i,i+j-num_per] <- t_inc_proy[i,j]
      }
    }
    
    dist_flujos <- t_inc_proy_cont / rowSums(t_inc_proy_cont)
    dist_flujos[is.na(dist_flujos)] <- 0
    
    dist_flujos[which(is.infinite(dist_flujos))] <- 0
    dist_flujos[rowSums(dist_flujos[,1:num_per])==0,1] <- 1
    
    # Cálculo Reserva SONR #
    
    # Diagonal    
    diagonal <- matrix(0,num_per,1)
    for(i in 1:(num_per)){
      k = num_per - i + 1
      diagonal[i,1] <- t_acum_proy[i,k]
    }
    
    # Factores de desarrollo acumulados seleccionados
    fd_acum_sim <- matrix(1,num_per,num_per)
    for(i in 1:num_per){
      for(j in (num_per-i+1):num_per){
        if(i+j==num_per+1)
          fd_acum_sim[i,j] <- fd_inc_sim[i,j]
        else
          fd_acum_sim[i,j] <- fd_acum_sim[i,j-1] * fd_inc_sim[i,j]
      }
    }
    
    # Siniestralidad Última Chain Ladder
    sin_ult_cl <- diagonal * fd_acum_sim[,num_per]
    
    porc_sin_ini <- sum(sin_ult_cl) / sum(pr_dev)
    sin_ult_bf <- diagonal + (pr_dev * porc_sin_ini * (1-1/fd_acum_sim[,num_per]))
    
    aleat_atip <- matrix(runif(nrow(sin_atip)),ncol=1)
    
    atip_sim_1 <- as.matrix((sin_atip$monto_ocu_mn * ifelse(aleat_atip <= 1/as.numeric(sin_atip$num_sin_tot),1,0)))
    
    atip_sim_1 <- as.matrix(atip_sim_1[atip_sim_1[,1]>0])
    
    if(nrow(atip_sim_1)>0){
      atip_sim_2 <- cbind(periodo_ocu=sample(20, size=nrow(atip_sim_1), replace = TRUE), data.frame(atip_sim_1))
      atip_sim_3 <- aggregate(atip_sim_2[,2], by=list(Category=atip_sim_2$periodo_ocu), FUN = sum)
      atip_sim <- merge(x=data.frame(Category=c(1:20)), y=atip_sim_3, by ="Category", all.x = TRUE)
      atip_sim[is.na(atip_sim)]<-0
    } else{
      atip_sim <- cbind(c(1:20), 0)
    }
    
    # Reserva SONR
    rva_sonr <- sin_ult_bf - diagonal
    
    flujos <- rva_sonr[,1] * dist_flujos[,1:num_per]
    flujos_atip_1 <- as.matrix(cbind(atip_sim[,2], dist_flujos[,1:num_per]))
    flujos_atip_1[rowSums(flujos_atip_1[,2:(num_per+1)])==0,2] <- 1
    flujos_atip <- flujos_atip_1[,1]*flujos_atip_1[,2:(num_per+1)]
    
    if (nsim == 1)
      resultados <- data.frame(num_sim = nsim, sum(rva_sonr), t(colSums(flujos)), t(colSums(flujos_atip)))
    else
      resultados <- rbind(resultados, data.frame(num_sim = nsim, sum(rva_sonr), t(colSums(flujos)), t(colSums(flujos_atip))))
    
    print(nsim) 
    #Sys.sleep(0.01) 
    flush.console() 
  }
  names(resultados) <- c("num_sim",
                         "rva_sonr",
                         "flujo_sonr_1","flujo_sonr_2","flujo_sonr_3","flujo_sonr_4","flujo_sonr_5","flujo_sonr_6","flujo_sonr_7","flujo_sonr_8",
                         "flujo_sonr_9","flujo_sonr_10","flujo_sonr_11","flujo_sonr_12","flujo_sonr_13","flujo_sonr_14","flujo_sonr_15","flujo_sonr_16",
                         "flujo_sonr_17","flujo_sonr_18","flujo_sonr_19","flujo_sonr_20","flujo_atip_1","flujo_atip_2","flujo_atip_3","flujo_atip_4",
                         "flujo_atip_5","flujo_atip_6","flujo_atip_7","flujo_atip_8","flujo_atip_9","flujo_atip_10","flujo_atip_11","flujo_atip_12",
                         "flujo_atip_13","flujo_atip_14","flujo_atip_15","flujo_atip_16","flujo_atip_17","flujo_atip_18","flujo_atip_19","flujo_atip_20"
  )
  return(resultados)
}

###########################
#    Definir parámetros  #
##########################

#Fecha de valuación 
#Para este ejemplo se utilizara 201912, la cual corresponde al mes de diciembre del 2019
fecha_corte <- 201912

#Primas
primas_dev <- genera_primas('H', fecha_corte, 'T')
atipicos <- atipicos[atipicos$cia=='H',]

# Triángulos
#genera_triangulos(compañia, ramo, corte, tipo, atipicos 1=incluyendo 0=excluyendo, litigios 1=incuyendo 2=excluyendo 3=sólo litigios, periodicidad)
triangulo_ocu <- genera_triangulos("H", fecha_corte, "OCU", 1, "T")

# Factores de Desarrollo
fact_des_ocu <- factores_desarrollo(triangulo_ocu)

# Simulación de Flujos
flujos_sonr <- simula_flujos_sonr(triangulo_ocu, fact_des_ocu, primas_dev[2], atipicos, 10000)

###########################
#        Salidas         #
##########################

#Salida simulaciones para metodología estocástica

write.table(flujos_sonr, file="clipboard-16384", sep="\t", row.names=FALSE)

#Colocate en la hoja "Simulaciones SONR" del archivo "Valuación SONR.xlsx" en la celda A1 y presionea Ctrl + V para visualizar las simulaciones.
#En la hoja "Valuación Estocástica" del mismo encontrarás el valor de la reservan en las celdas D29 y G10


#############################################
#         PROCESO DETERMINÍSTICO            #
#############################################

############################
# Generación de Triángulos #
############################

compañia = "H"
corte = 201912       #Para este ejemplo se utilizara 201912, la cual corresponde al mes de diciembre del 2019
tipo = "OCU"         #Para esta reserva utilizaremos únicamente el concepto de siniestros Ocurridos 
atipicos <- 1        #Incluimos siniestros atípicos
periodicidad = "T"   #La periodicidad del triángulo será trimestral 

inicio = (floor(corte/100)-5+floor((100*(corte/100-floor(corte/100))+1)/12))*100 + (100*(corte/100-floor(corte/100))+1)%%12

siniest_0 <- subset(siniest, (cia == compañia) & (año_mes_ocu >= inicio) & (año_mes_ocu <= corte) & (año_mes_mov <= corte))

if(atipicos==0){
  siniest_0 <- subset(siniest_0, atipico==0)
}

columnas_sin <- c("año_mes_ocu", "año_mes_mov")
if (tipo=="OCU") { 
  columnas_sin[3] <- "monto_ocu"
} else if (tipo=="PAG") {
  columnas_sin[3] <- "monto_pag"
} else if  (tipo=="GTO") {
  columnas_sin[3] <- "monto_gto"
} else {
  columnas_sin[3] <- "monto_syr"
}

siniest_1 <- siniest_0[columnas_sin]

names(siniest_1) <- c("año_mes_ocu", "año_mes_mov", "monto")

periodos <- c(inicio)
for (i in 2:60){
  periodos[i] <- periodos[i-1] + ifelse(substr(periodos[i-1],5,7)=="12",89,1)
}

num_per = ifelse(periodicidad=="M", 60, ifelse(periodicidad=="T", 20, ifelse(periodicidad =="A", 5,0)))

for(i in 1:num_per){
  k = ifelse(periodicidad=="M", i, ifelse(periodicidad =="T", i*3-2, i*12-11))
  t = ifelse(periodicidad=="M", 0, ifelse(periodicidad =="T", 2, 11))
  siniest_1$per_ocu[siniest_1$año_mes_ocu >= periodos[k] & siniest_1$año_mes_ocu <= periodos[k+t]] <- i
  siniest_1$per_mov[siniest_1$año_mes_mov >= periodos[k] & siniest_1$año_mes_mov <= periodos[k+t]] <- i
}

triangulo_cont <- matrix(0,num_per,num_per)
for(i in 1:num_per){
  for(j in i:num_per){
    triangulo_cont[i,j] <- sum(subset(siniest_1, per_ocu == i & per_mov == j)$monto)
  }
}

# Triángulo Incremental
#-----------------------
triangulo_inc <- matrix(0,num_per,num_per)
for(i in 1:num_per){
  for(j in i:num_per){
    triangulo_inc[i,j-i+1] <- triangulo_cont[i,j]
  }
}

# Triángulo Acumulado
#---------------------
triangulo_acum <- matrix(0,num_per,num_per)
for(i in 1:num_per){
  for(j in i:num_per){
    k = j -i + 1
    if(k == 1) triangulo_acum[i,k] <- triangulo_inc[i,k] else triangulo_acum[i,k] <- triangulo_acum[i,k-1] + triangulo_inc[i,k]
  }
}

###########################
#        Salidas         #
##########################

#Salida triángulo acumulado para metodología determinística

write.table(triangulo_acum, file="clipboard-16384", sep="\t", row.names=FALSE)

#Colocate en la hoja "Valuación Determinística" del archivo "Valuación SONR.xlsx" en la celda B8 y presionea Ctrl + V para visualizar el triángulo de 
#siniestralidad acumulada. En dicha hoja encontrarás el valor de la reservan en las celdas F56 y J38





