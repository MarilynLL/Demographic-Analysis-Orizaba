

# ==============================================================================
#               ESTUDIO DEMOGRÁFICO DE LA POBLACIÓN DE ORIZABA, VERACRUZ
#                               MARILYN LLANOS HERRERA
# ==============================================================================



# ==============================================================================
#                                   Parte 1
# ==============================================================================



# ******************************************************************************
# ************************** ---- 1: INDICE DE WHIPPLE *************************
# ******************************************************************************

# Cargar la base de datos de la población de Orizaba de los años 2010 y 2020
Pob2010 = read.csv(
  "C:/Users/hp/Documents/UNI/Actuaria/Demografia/Pob_2010_Orizaba.csv",
  header = TRUE)
View(Pob2010)
Pob2020 = read.csv(
  "C:/Users/hp/Documents/UNI/Actuaria/Demografia/Pob_2020_Orizaba.csv",
  header = TRUE)
View(Pob2020)

# Para quitar espacios en blanco de la base de datos
Pob2010[is.na(Pob2010)] = 0
#View(Pob2010)
Pob2020[is.na(Pob2020)] = 0
#View(Pob2020)

# |=|=|=|=|=|=|=|=|=|=|=|=|=| INDICE WHIPPLE 2010 |=|=|=|=|=|=|=|=|=|=|=|=|=|
w1_h_2010 = sum(Pob2010$Hombres[seq(26,61, by = 5)])
w2_h_2010 = sum(Pob2010$Hombres[seq(24,63)])
whipple_Hombres_2010 = (5*w1_h_2010/w2_h_2010)*100; whipple_Hombres_2010
w1_m_2010 = sum(Pob2010$Mujeres[seq(26,61, by = 5)])
w2_m_2010 = sum(Pob2010$Mujeres[seq(24,63)])
whipple_Mujeres_2010 = (5*w1_m_2010/w2_m_2010)*100; whipple_Mujeres_2010

# |=|=|=|=|=|=|=|=|=|=|=|=|=| INDICE WHIPPLE 2020 |=|=|=|=|=|=|=|=|=|=|=|=|=|
w1_h_2020 = sum(Pob2020$Hombres[seq(26,61, by = 5)])
w2_h_2020 = sum(Pob2020$Hombres[seq(24,63)])
whipple_Hombres_2020 = (5*w1_h_2020/w2_h_2020)*100; whipple_Hombres_2020

w1_m_2020 = sum(Pob2020$Mujeres[seq(26,61, by = 5)])
w2_m_2020 = sum(Pob2020$Mujeres[seq(24,63)])
whipple_Mujeres_2020 = (5*w1_m_2020/w2_m_2020)*100; whipple_Mujeres_2020



# ******************************************************************************
# ************************** ---- 2. INDICE DE MYERS ***************************
# ******************************************************************************

# Se obtendra el indice de Myers para edades de 10 a 79 anios, para la poblacion
# de hombres y mujeres

# |=|=|=|=|=|=|=|=|=|=|=|=|=|=| INDICE MYERS 2010 |=|=|=|=|=|=|=|=|=|=|=|=|=|=|
Mh_2010 = c()
Mm_2010 = c()
for (j in 0:9){
  # Estamos movidos un renglOn en la base de datos
  Pj1_Hombres_2010 = sum(Pob2010$Hombres[seq(11+j,61+j,by = 10)])
  Pj2_Hombres_2010 = sum(Pob2010$Hombres[seq(21+j,71+j,by = 10)])
  Pj1_Mujeres_2010 = sum(Pob2010$Mujeres[seq(11+j,61+j,by = 10)])
  Pj2_Mujeres_2010 = sum(Pob2010$Mujeres[seq(21+j,71+j,by = 10)])
  Mh_2010[j+1] = (j+1)*Pj1_Hombres_2010 + (9-j)*Pj2_Hombres_2010
  Mm_2010[j+1] = (j+1)*Pj1_Mujeres_2010 + (9-j)*Pj2_Mujeres_2010
}
mh_2010 = sum(Mh_2010)
mm_2010 = sum(Mm_2010)

Mj_Hombres_2010 = c()
Mj_Mujeres_2010 = c()



for (i in 1:10){
  Mj_Hombres_2010[i] = (Mh_2010[i]/mh_2010)-0.10
  Mj_Mujeres_2010[i] = (Mm_2010[i]/mm_2010)-0.10
}
Mj_Hombres_2010 = Mj_Hombres_2010*100; Mj_Hombres_2010
Myers_Hombres_2010 = sum(abs(Mj_Hombres_2010)); Myers_Hombres_2010
Mj_Mujeres_2010 = Mj_Mujeres_2010*100; Mj_Mujeres_2010
Myers_Mujeres_2010 = sum(abs(Mj_Mujeres_2010)); Myers_Mujeres_2010

# |=|=|=|=|=|=|=|=|=|=|=|=|=|=| INDICE MYERS 2020 |=|=|=|=|=|=|=|=|=|=|=|=|=|=|
Mh_2020 = c()
Mm_2020 = c()
for (j in 0:9){
  # Estamos movidos un renglOn en la base de datos
  Pj1_Hombres_2020 = sum(Pob2020$Hombres[seq(11+j,61+j,by = 10)])
  Pj2_Hombres_2020 = sum(Pob2020$Hombres[seq(21+j,71+j,by = 10)])
  Pj1_Mujeres_2020 = sum(Pob2020$Mujeres[seq(11+j,61+j,by = 10)])
  Pj2_Mujeres_2020 = sum(Pob2020$Mujeres[seq(21+j,71+j,by = 10)])
  Mh_2020[j+1] = (j+1)*Pj1_Hombres_2020 + (9-j)*Pj2_Hombres_2020
  Mm_2020[j+1] = (j+1)*Pj1_Mujeres_2020 + (9-j)*Pj2_Mujeres_2020
}
mh_2020 = sum(Mh_2020)
mm_2020 = sum(Mm_2020)
Mj_Hombres_2020 = c()
Mj_Mujeres_2020 = c()
for (i in 1:10){
  Mj_Hombres_2020[i] = (Mh_2020[i]/mh_2020)-0.10
  Mj_Mujeres_2020[i] = (Mm_2020[i]/mm_2020)-0.10
}
Mj_Hombres_2020 = Mj_Hombres_2020*100; Mj_Hombres_2020
Myers_Hombres_2020 = sum(abs(Mj_Hombres_2020)); Myers_Hombres_2020
Mj_Mujeres_2020 = Mj_Mujeres_2020*100; Mj_Mujeres_2020
Myers_Mujeres_2020 = sum(abs(Mj_Mujeres_2020)); Myers_Mujeres_2020


# ******************************************************************************
# ********************* ---- 3. INDICE DE NACIONES UNIDAS **********************
# ******************************************************************************

# Para el Índice de Naciones Unidas evaluaremos de las edades de 5-9 años hasta
# 65-69 años para ello necesitamos los grupos quinquenales de 0-4 años y hasta 
# 70-74 años

# |=|=|=|=|=|=|=|=|=|=|=| INDICE DE NACIONES UNIDAS 2010 |=|=|=|=|=|=|=|=|=|=|=|
Pob_quinquenios2010 <- data.frame(NA,15,3)
for (i in seq(5,75, by = 5)){
  Pob_quinquenios2010[(i/5),2:3]<- cumsum(Pob2010[(i-4):i,2:3])[5,]
}
colnames(Pob_quinquenios2010) <- c("Edad", "Hombres", "Mujeres")
Pob_quinquenios2010$Edad = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                             "35-39","40-44","45-49","50-54","55-59","60-64",
                             "65-69","70-74")
rownames(Pob_quinquenios2010) <- c(seq(1:15))

# Construimos los índices para hombres, mujeres y ambos sexos:
nu_h_2010 = 0
for (i in 2:14){
  nu_hombres_2010 <- nu_h_2010 + 
    abs(2*Pob_quinquenios2010$Hombres[i]/(Pob_quinquenios2010$Hombres[i-1] +
                                            Pob_quinquenios2010$Hombres[i+1])-1)/13
  nu_h_2010 = nu_hombres_2010
}
nu_h_2010 <- nu_h_2010 * 100;  nu_h_2010

nu_m_2010 = 0
for (i in 2:14){
  nu_mujeres_2010 <- nu_m_2010 + 
    abs(2*Pob_quinquenios2010$Mujeres[i]/(Pob_quinquenios2010$Mujeres[i-1] + 
                                            Pob_quinquenios2010$Mujeres[i+1])-1)/13
  nu_m_2010 = nu_mujeres_2010
}
nu_m_2010 <- nu_m_2010 * 100;  nu_m_2010

nu_a_2010 = 0
for (i in 2:14){
  nu_ambos_2010 <- nu_a_2010 + 
    abs(Pob_quinquenios2010$Hombres[i]/Pob_quinquenios2010$Mujeres[i] - 
          Pob_quinquenios2010$Hombres[i+1]/Pob_quinquenios2010$Mujeres[i+1])/13
  nu_a_2010 <- nu_ambos_2010
}
nu_a_2010 <- nu_a_2010*100; nu_a_2010

NU_2010 <-nu_h_2010 + nu_m_2010 + 3*nu_a_2010; NU_2010

# |=|=|=|=|=|=|=|=|=|=|=| INDICE DE NACIONES UNIDAS 2020 |=|=|=|=|=|=|=|=|=|=|=|
Pob_quinquenios2020 <- data.frame(NA,15,3)
for (i in seq(5,75, by = 5)){
  Pob_quinquenios2020[(i/5),2:3]<- cumsum(Pob2020[(i-4):i,2:3])[5,]
}
colnames(Pob_quinquenios2020) <- c("Edad", "Hombres", "Mujeres")
Pob_quinquenios2020$Edad = c("0-4","5-9","10-14","15-19","20-24","25-29",
                             "30-34","35-39","40-44","45-49","50-54","55-59",
                             "60-64","65-69","70-74")
rownames(Pob_quinquenios2020) <- c(seq(1:15))

# Construimos los índices para hombres, mujeres y ambos sexos:
nu_h_2020 = 0
for (i in 2:14){
  nu_hombres_2020 <- nu_h_2020 + 
    abs(2*Pob_quinquenios2020$Hombres[i]/(Pob_quinquenios2020$Hombres[i-1] + 
                                            Pob_quinquenios2020$Hombres[i+1])-1)/13
  nu_h_2020 = nu_hombres_2020
}
nu_h_2020 <- nu_h_2020 * 100;  nu_h_2020

nu_m_2020 = 0
for (i in 2:14){
  nu_mujeres_2020 <- nu_m_2020 + 
    abs(2*Pob_quinquenios2020$Mujeres[i]/(Pob_quinquenios2020$Mujeres[i-1] + 
                                            Pob_quinquenios2020$Mujeres[i+1])-1)/13
  nu_m_2020 = nu_mujeres_2020
}
nu_m_2020 <- nu_m_2020 * 100;  nu_m_2020

nu_a_2020 = 0
for (i in 2:14){
  nu_ambos_2020 <- nu_a_2020 + 
    abs(Pob_quinquenios2020$Hombres[i]/Pob_quinquenios2020$Mujeres[i] - 
          Pob_quinquenios2020$Hombres[i+1]/Pob_quinquenios2020$Mujeres[i+1])/13
  nu_a_2020 <- nu_ambos_2020
}
nu_a_2020 <- nu_a_2020*100; nu_a_2020

NU_2020 <-nu_h_2020 + nu_m_2020 + 3*nu_a_2020; NU_2020

# ==============================================================================
#                         PRORRATEO
# ==============================================================================

# |=|=|=|=|=|=|=|=|=|=|=| PRORRATEO 2010 |=|=|=|=|=|=|=|=|=|=|=|
# Porcentaje de concentración de la población No Especificada
Pob_Total_Hombres_2010 = sum(Pob2010$Hombres)
Pob_Total_Mujeres_2010 = sum(Pob2010$Mujeres)

Pob_NE_Hombres_2010 = Pob2010$Hombres[102]
Pob_NE_Mujeres_2010 = Pob2010$Mujeres[102]

alfa_Hombres_2010 = Pob_NE_Hombres_2010/(Pob_Total_Hombres_2010 - 
                                           Pob_NE_Hombres_2010); alfa_Hombres_2010
alfa_Mujeres_2010 = Pob_NE_Mujeres_2010/(Pob_Total_Mujeres_2010 - 
                                           Pob_NE_Mujeres_2010); alfa_Mujeres_2010


# Se procede a prorratear o agregar la Pob. No Especificada al grupo de mayor 
# peso, según sea el caso:
Pob2010_Correccion <- matrix(NA, length(Pob2010$Hombres)-1,3)

for (i in 1:(length(Pob2010$Hombres)-1)){
  if (alfa_Hombres_2010>=0.10){
    Pob2010_Correccion[i,2] = Pob2010$Hombres[i] + 
      Pob2010$Hombres[i]*alfa_Hombres_2010
  }
  else {
    if (Pob2010$Hombres[i]==max(Pob2010$Hombres[1:101])){
      Pob2010_Correccion[i,2]=Pob2010$Hombres[i] + Pob_NE_Hombres_2010
    }
    else {
      Pob2010_Correccion[i,2] = Pob2010$Hombres[i]
    }
  }
}

for (i in 1:(length(Pob2010$Mujeres)-1)){
  if (alfa_Mujeres_2010>=0.10){
    Pob2010_Correccion[i,3] = Pob2010$Mujeres[i] + 
      Pob2010$Mujeres[i]*alfa_Mujeres_2010
  }
  else {
    if (Pob2010$Mujeres[i]==max(Pob2010$Mujeres[1:101])){
      Pob2010_Correccion[i,3]=Pob2010$Mujeres[i] + Pob_NE_Mujeres_2010
    }
    else {
      Pob2010_Correccion[i,3] = Pob2010$Mujeres[i]
    }
  }
}

colnames(Pob2010_Correccion) <- c("Edad", "Hombres", "Mujeres")
Pob2010_Correccion = data.frame(Pob2010_Correccion)
Pob2010_Correccion$Edad = Pob2010$Edad[-102]

View(Pob2010_Correccion)

sum(Pob2010_Correccion$Hombres) == sum(Pob2010$Hombres)
sum(Pob2010_Correccion$Mujeres) == sum(Pob2010$Mujeres)

# |=|=|=|=|=|=|=|=|=|=|=| PRORRATEO 2020 |=|=|=|=|=|=|=|=|=|=|=|
# Porcentaje de concentración de la población No Especificada
Pob_Total_Hombres_2020 = sum(Pob2020$Hombres)
Pob_Total_Mujeres_2020 = sum(Pob2020$Mujeres)

Pob_NE_Hombres_2020 = Pob2020$Hombres[102]
Pob_NE_Mujeres_2020 = Pob2020$Mujeres[102]

alfa_Hombres_2020 = Pob_NE_Hombres_2020/(Pob_Total_Hombres_2020 - 
                                           Pob_NE_Hombres_2020); alfa_Hombres_2020
alfa_Mujeres_2020 = Pob_NE_Mujeres_2020/(Pob_Total_Mujeres_2020 - 
                                           Pob_NE_Mujeres_2020); alfa_Mujeres_2020

# Se procede a prorratear o agregar la Pob. No Especificada al grupo de mayor 
# peso, según sea el caso:
Pob2020_Correccion <- matrix(NA, length(Pob2020$Hombres)-1,3)

for (i in 1:(length(Pob2020$Hombres)-1)){
  if (alfa_Hombres_2020>=0.10){
    Pob2020_Correccion[i,2] = Pob2020$Hombres[i] + 
      Pob2020$Hombres[i]*alfa_Hombres_2020
  }
  else {
    if (Pob2020$Hombres[i]==max(Pob2020$Hombres)){
      Pob2020_Correccion[i,2]=Pob2020$Hombres[i] + Pob_NE_Hombres_2020
    }
    else {
      Pob2020_Correccion[i,2] = Pob2020$Hombres[i]
    }
  }
}

for (i in 1:(length(Pob2020$Mujeres)-1)){
  if (alfa_Mujeres_2020>=0.10){
    Pob2020_Correccion[i,3] = Pob2020$Mujeres[i] + 
      Pob2020$Mujeres[i]*alfa_Mujeres_2020
  }
  else {
    if (Pob2020$Mujeres[i]==max(Pob2020$Mujeres)){
      Pob2020_Correccion[i,3]=Pob2020$Mujeres[i] + Pob_NE_Mujeres_2020
    }
    else {
      Pob2020_Correccion[i,3] = Pob2020$Mujeres[i]
    }
  }
}

colnames(Pob2020_Correccion) <- c("Edad", "Hombres", "Mujeres")
Pob2020_Correccion = data.frame(Pob2020_Correccion)
Pob2020_Correccion$Edad = Pob2020$Edad[-102]

View(Pob2020_Correccion)

sum(Pob2020_Correccion$Hombres) == sum(Pob2020$Hombres)
sum(Pob2020_Correccion$Mujeres) == sum(Pob2020$Mujeres)

# |=|=|=|=|=|=|=|=|=|=|=|=| CORRECCION DE POBLACION 2010 |=|=|=|=|=|=|=|=|=|=|=|=|
quinquenios_2010 <- data.frame(NA, 18,3)
# Agrupar quinquenios 0-4, 5-9, ..., 80-84
for (i in seq(5,85, by = 5)){
  quinquenios_2010[(i/5),2:3] <- cumsum(Pob2010_Correccion[(i-4):i,2:3])[5,]
}

# Grupo de 85+ años
quinquenios_2010[18,2:3] <- cumsum(Pob2010_Correccion[86:101,2:3])[16,]

# Nombre de columnas
colnames(quinquenios_2010) <- c("Edad", "Hombres", "Mujeres")

# Nombre de renglones
quinquenios_2010$Edad <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                           "35-39","40-44","45-49","50-54","55-59","60-64",
                           "65-69","70-74","75-79","80-84","85 y más")
rownames(quinquenios_2010) <- c(seq(1:18))
View(quinquenios_2010)
# ////////////////////////////////////////////////////////////////////////////
Correccion2010 <- quinquenios_2010
for (i in 3:15) {
  Correccion2010[i,2:3] <- (1/16)*(-1*quinquenios_2010[i+2,2:3]+4*quinquenios_2010[i+1,2:3]+
                                     10*quinquenios_2010[i,2:3]+4*quinquenios_2010[i-1,2:3]-1*quinquenios_2010[i-2,2:3])
}
Correccion2010
Correccion2010[,2:3] = round(Correccion2010[,2:3]); Correccion2010 #Rendondear a enteros.
rownames(quinquenios_2010) <- c(seq(1:18))
View(quinquenios_2010)
# //////////////////////////////////////////////////////////////////////////
# install.packages("pyramid")
library(pyramid)
?pyramid

# Ordenar datos para función pyramid
data1_2010 <- data.frame(quinquenios_2010$Hombres, quinquenios_2010$Mujeres, 
                         quinquenios_2010$Edad)
pyramid(data1_2010, Llab = "Hombres", Rlab = "Mujeres", Clab = "Edades", 
        Rcol = "Pink", Lcol = "Cyan", main = "Estructura de la Población 2010")

#La nueva
library(pyramid)
?pyramid


data1_2010 <- data.frame(Correccion2010$Hombres, Correccion2010$Mujeres, 
                         Correccion2010$Edad)
pyramid(data1_2010, Llab = "Hombres", Rlab = "Mujeres", Clab = "Edades", 
        Rcol = "Pink", Lcol = "Cyan", main = "Estructura de la Población 2010")

# |=|=|=|=|=|=|=|=|=|=|=| CORRECCION DE POBLACION 2020 |=|=|=|=|=|=|=|=|=|=|=|
quinquenios_2020 <- data.frame(NA, 18,3)
# Agrupar quinquenios 0-4, 5-9, ..., 80-84
for (i in seq(5,85, by = 5)){
  quinquenios_2020[(i/5),2:3] <- cumsum(Pob2020_Correccion[(i-4):i,2:3])[5,]
}

# Grupo de 85+ años(
quinquenios_2020[18,2:3] <- cumsum(Pob2020_Correccion[86:101,2:3])[16,]

# Nombre de columnas
colnames(quinquenios_2020) <- c("Edad", "Hombres", "Mujeres")

# Nombre de renglones
quinquenios_2020$Edad <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                           "35-39","40-44","45-49","50-54","55-59","60-64",
                           "65-69","70-74","75-79","80-84","85 y más")
rownames(quinquenios_2020) <- c(seq(1:18))
View(quinquenios_2020)
# ////////////////////////////////////////////////////////////////////////////
Correccion2020 <- quinquenios_2020
for (i in 3:15) {
  Correccion2020[i,2:3] <- (1/16)*(-1*quinquenios_2020[i+2,2:3]+4*quinquenios_2020[i+1,2:3]+
                                     10*quinquenios_2020[i,2:3]+4*quinquenios_2020[i-1,2:3]-1*quinquenios_2020[i-2,2:3])
}
Correccion2020
Correccion2020[,2:3] = round(Correccion2020[,2:3]); Correccion2020 #Rendondear a enteros.
rownames(quinquenios_2020) <- c(seq(1:18))
View(quinquenios_2020)
# //////////////////////////////////////////////////////////////////////////

# Ordenar datos para función pyramid
data1_2020 <- data.frame(quinquenios_2020$Hombres, quinquenios_2020$Mujeres, 
                         quinquenios_2020$Edad)
pyramid(data1_2020, Llab = "Hombres", Rlab = "Mujeres", Clab = "Edades", 
        Rcol = "Pink", Lcol = "Cyan", main = "Estructura de la Población 2020")

# Nueva piramide

library(pyramid)
?pyramid


data1_2020 <- data.frame(Correccion2020$Hombres, Correccion2020$Mujeres, 
                         Correccion2020$Edad)
pyramid(data1_2020, Llab = "Hombres", Rlab = "Mujeres", Clab = "Edades", 
        Rcol = "Pink", Lcol = "Cyan", main = "Estructura de la Población 2020")
# |=|=|=|=|=|=|=|=|=|=|=| INDICE DE MASCULINIDAD 2010 |=|=|=|=|=|=|=|=|=|=|=|

Ind_Masculinidad_2010 <- quinquenios_2010
Ind_Masculinidad_2010$Ind_Masc <- (quinquenios_2010$Hombres/quinquenios_2010$Mujeres)*100

# |=|=|=|=|=|=|=|=|=|=|=| INDICE DE MASCULINIDAD 2020 |=|=|=|=|=|=|=|=|=|=|=|

Ind_Masculinidad_2020 <- quinquenios_2020
Ind_Masculinidad_2020$Ind_Masc <- (quinquenios_2020$Hombres/quinquenios_2020$Mujeres)*100



# |=|=|=|=|=|=|=|=|=|=|=|  METODO DE 1/16 |=|=|=|=|=|=|=|=|=|=|=|

# El método de 1/16 se aplica a las poblaciones por grupo quinquenal de 10-14
# años y hasta 70-74 años. Las poblaciones a las que no se les aplica no sufren
# cambios

# |=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=| 2010 |=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|
Correccion_2010 = quinquenios_2010

for(i in 3:15){
  Correccion_2010[i,2:3] = (1/16)*(-quinquenios_2010[i+2,2:3] + 
                                     4*quinquenios_2010[i+1,2:3] +
                                     10*quinquenios_2010[i,2:3] + 
                                     4*quinquenios_2010[i-1,2:3] - 
                                     quinquenios_2010[i-1,2:3])
}

Correccion_2010[,2:3] = round(Correccion_2010[,2:3])

# Separamos el grupo de 0 a 4 años en 0 a 0 y 1 a 4 años
Correccion_2010 = rbind(Pob2010_Correccion[1,],Correccion_2010)
Correccion_2010[1,1] = c("Menores a 1 año")
Correccion_2010[2,2:3] = cumsum(Pob2010_Correccion[2:5,2:3])[4,]
Correccion_2010[2,1] = c("1 a 4 años")
View(Correccion_2010)


# Gráfica para Mujeres
plot(quinquenios_2010$Mujeres[2:18], type = "o", 
     main = "Población de Mujeres Corregida: Orizaba a 2010",
     xlab = "Edad", ylab = "Población de Mujeres", col = "blue", xlim = c(0,17), 
     ylim = c(500, 15000), xaxt = "n")
axis(1, at = seq(1:17), labels = c("5-9","10-14","15-19","20-24","25-29","30-34",
                                   "35-39","40-44","45-49","50-54","55-59","60-64",
                                   "65-69","70-74","75-79","80-84","85 y más"))
lines(x = Correccion_2010$Mujeres[3:19], type = "o", col = "red")
legend(x = "topright", legend = c("No Corregida", "Corregida"), lty = c(1,1), 
       col = c("blue", "red"), pch = c(1,1))

#Gráfica para Hombres
plot(quinquenios_2010$Hombres[2:18], type = "o", 
     main = "Población de Hombres Corregida: Orizaba 2010",
     xlab = "Edad", ylab = "Población de Hombres", col = "blue", xlim = c(0,17), 
     ylim = c(500, 15000), xaxt = "n")
axis(1, at = seq(1:17), labels = c("5-9","10-14","15-19","20-24","25-29","30-34",
                                   "35-39","40-44","45-49","50-54","55-59","60-64",
                                   "65-69","70-74","75-79","80-84","85 y más"))
lines(x = Correccion_2010$Hombres[3:19], type = "o", col = "red")
legend(x = "topright", legend = c("No Corregida", "Corregida"), lty = c(1,1), 
       col = c("blue", "red"), pch = c(1,1))

# |=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=| 2020 |=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|
Correccion_2020 = quinquenios_2020

for(i in 3:15){
  Correccion_2020[i,2:3] = (1/16)*(-quinquenios_2020[i+2,2:3] + 
                                     4*quinquenios_2020[i+1,2:3] +
                                     10*quinquenios_2020[i,2:3] + 
                                     4*quinquenios_2020[i-1,2:3] - 
                                     quinquenios_2020[i-1,2:3])
}

Correccion_2020[,2:3] = round(Correccion_2020[,2:3])

# Separamos el grupo de 0 a 4 años en 0 a 0 y 1 a 4 años
Correccion_2020 = rbind(Pob2020_Correccion[1,],Correccion_2020)
Correccion_2020[1,1] = c("Menores a 1 año")
Correccion_2020[2,2:3] = cumsum(Pob2020_Correccion[2:5,2:3])[4,]
Correccion_2020[2,1] = c("1 a 4 años")
View(Correccion_2020)

# Gráfica para Mujeres
plot(quinquenios_2020$Mujeres[2:18], type = "o", 
     main = "Población de Mujeres Corregida: Orizaba 2020",
     xlab = "Edad", ylab = "Población de Mujeres", col = "blue", xlim = c(0,17), 
     ylim = c(500, 15000), xaxt = "n")
axis(1, at = seq(1:17), labels = c("5-9","10-14","15-19","20-24","25-29","30-34",
                                   "35-39","40-44","45-49","50-54","55-59","60-64",
                                   "65-69","70-74","75-79","80-84","85 y más"))
lines(x = Correccion_2020$Mujeres[3:19], type = "o", col = "red")
legend(x = "topright", legend = c("No Corregida", "Corregida"), lty = c(1,1), 
       col = c("blue", "red"), pch = c(1,1))

# Gráfica para Hombres
plot(quinquenios_2020$Hombres[2:18], type = "o", 
     main = "Población de Hombres Corregida: Orizaba 2020",
     xlab = "Edad", ylab = "Población de Hombres", col = "blue", xlim = c(0,17), 
     ylim = c(500, 15000), xaxt = "n")
axis(1, at = seq(1:17), labels = c("5-9","10-14","15-19","20-24","25-29","30-34",
                                   "35-39","40-44","45-49","50-54","55-59","60-64",
                                   "65-69","70-74","75-79","80-84","85 y más"))
lines(x = Correccion_2020$Hombres[3:19], type = "o", col = "red")
legend(x = "topright", legend = c("No Corregida", "Corregida"), lty = c(1,1), 
       col = c("blue", "red"), pch = c(1,1))

Correccion2020 = rbind(Pob2020_Correccion[1,],Correccion2020)
Correccion2020[1,1] = c("Menor a 1 año")
Correccion2020[2,2:3] = Correccion2020[2,2:3] - Correccion2020[1,2:3]
Correccion2020[2,1] = c("1 a 4 años"); 

View(Correccion_2010)

# **************************** POBLACION CORREGIDA A MITAD DE AÑO CENSAL ********************************
# ************************ CRECIMIENTO GEOMETRICO Y CRECIMIENTO EXPONENCIAL ****************************

# Las bases de datos Correccion2010 y Correccion2020 contienen la correcciOn de la poblaciOn por grupos quinquenales
# de edad por medio de 1/16. Contiene los grupos de menores de 1, 1 a 4, 5 a 9,...,80 a 84, 85 y mas años de edad.


# A partir del crecimiento geometrico y exponencial se llevara a mitad de año 2015 y 2020, 
# a la poblacion de hombres y de mujeres en cada grupo de edad menores de 1, 1 a 4, 5 a 9,...,80 a 84, 85 y mas años de edad.

#Quitamos una columna adicional que tienen los datos, no en todos los casos pasa lo mismo hay que tener cuidado con eliminar información que no se deba eliminar.
#Correccion2010 = Correccion2010[,-1]
#Correccion2020 = Correccion2020[,-1]

# Censo 2010, fecha de referencia: 12 junio 2010
# Censo 2020, fecha de referencia: 15 marzo 2020

f1 <- as.Date(c("2010-06-12", "2020-03-15"))
# La diferencia en d?as entre ambas fechas de referencia del censo 2010 y el censo 2020 es:
dias = as.numeric(f1[2]-f1[1]); dias
# La diferencia en a?os es: 
n = dias/365; n

# **********************************  1. Crecimiento Geometrico: ********************************************

# Obtenemos la tasa de crecimiento geom?trico intercensal para hombres y para mujeres,
r_Geom = Correccion2020 
r_Geom$Hombres = ((Correccion_2020$Hombres/Correccion_2010$Hombres)^(1/n))-1
r_Geom$Mujeres = ((Correccion_2020$Mujeres/Correccion_2010$Mujeres)^(1/n))-1
View(r_Geom)

# Llevamos a mitad de año (30/junio/t) a los hombres y a las mujeres para el 2015 y 2020: 
f2 <- as.Date(c("2010-06-12", "2015-06-30"))
f3 <- as.Date(c("2020-03-15", "2020-06-30"))
n1 = as.numeric(f2[2]-f2[1])/365  #Diferencia en a?os del 12/jun/2010 al 30/jun/2015
n2 = as.numeric(f3[2]-f3[1])/365  #Diferencia en a?os del 15/marzo/2020 al 30/jun/2020

Pob_Mitad2015_Geom = Correccion_2010
Pob_Mitad2020_Geom = Correccion_2020
Pob_Mitad2015_Geom
Pob_Mitad2015_Geom$Hombres=Correccion_2010$Hombres*(1+r_Geom$Hombres)^(n1)
Pob_Mitad2015_Geom$Mujeres=Correccion_2010$Mujeres*(1+r_Geom$Mujeres)^(n1)
Pob_Mitad2015_Geom$Mujeres <- round(Pob_Mitad2015_Geom$Mujeres)
Pob_Mitad2015_Geom$Hombres <- round(Pob_Mitad2015_Geom$Hombres)
Pob_Mitad2015_Geom$Edad <- Correccion_2010$Edad; View(Pob_Mitad2015_Geom)


Pob_Mitad2020_Geom$Hombres=Correccion_2020$Hombres*(1+r_Geom$Hombres)^(n2)
Pob_Mitad2020_Geom$Mujeres=Correccion_2020$Mujeres*(1+r_Geom$Mujeres)^(n2)
Pob_Mitad2020_Geom$Mujeres <- round(Pob_Mitad2020_Geom$Mujeres)
Pob_Mitad2020_Geom$Hombres <- round(Pob_Mitad2020_Geom$Hombres)
View(Pob_Mitad2020_Geom)

######**********      2. CRECIMIENTO EXPONENCIAL     **********######
# Censo 2010, fecha de referencia: 12 junio 2010
# Censo 2020, fecha de referencia: 15 marzo 2020
#install.packages("tidyverse")
library(tidyverse)

setwd(  )

fcensos <- as.Date(c("2010-06-12", "2020-03-15"))
dias = as.numeric(fcensos[2]-fcensos[1])
n = dias/365

Pob_Exp <- left_join(Correccion_2010, Correccion_2020, by="Edad")
colnames(Pob_Exp) <- c("Edad", "H2010", "M2010", "H2020", "M2020")
Pob_Exp <- Pob_Exp %>% mutate(tasah := log(H2020/H2010)/n,
                              tasam := log(M2020/M2010)/n)
View(Pob_Exp)
# fechas para proyectar la población
fechas <- as.Date(c("2015-01-30","2020-06-30"))
n1 <- as.numeric(fechas[1] - fcensos[1])/365
n2 <- as.numeric(fechas[2] - fcensos[2])/365

Pob_Mitad2015_Exp <- Pob_Exp %>% mutate(Hombres := round( H2010*exp(tasah*n1) ),
                                        Mujeres := round( M2010*exp(tasam*n1) )) %>%
  select("Edad", "Hombres", "Mujeres")
Pob_Mitad2015_Exp

Pob_Mitad2020_Exp <- Pob_Exp %>% mutate(Hombres := round(  H2020*exp(tasah*n2) ),
                                        Mujeres := round(  M2020*exp(tasam*n1) )) %>%
  select( "Edad", "Hombres", "Mujeres" )
Pob_Mitad2020_Exp
# ==============================================================================
#                                   Parte 3
# ==============================================================================

# ************************ MORTALIDAD ****************************
# ************************ 2015 ****************************

####Carga de bases de datos####
Def_2015 <- read.csv( "C:/Users/hp/Documents/UNI/Actuaria/Demografia/Def2015.csv")
Nac_2015 <- read.csv( "C:/Users/hp/Documents/UNI/Actuaria/Demografia/Nac2015.csv")



Pob_Mitad2015_Geom = Pob_Mitad2015_Geom[,-1]

colnames(Def_2015) <- c("Edad", "Hombres", "Mujeres")
colnames(Nac_2015) <- c("Edad", "Hombres", "Mujeres")
Def_2015$Mujeres<- as.numeric(Def_2015$Mujeres)
Def_2015$Hombres<- as.numeric(Def_2015$Hombres)


Def_2015[is.na(Def_2015)] = 0
Nac_2015[is.na(Nac_2015)] = 0

####*******************************MEDIDAS DE MORTALIDAD************************####
# ************************ 2015 ****************************
#HOMBRES
# Tasa Bruta de Mortalidad
TBM_H_2015 = (sum(Def_2015$Hombres)/sum(Pob_Mitad2015_Geom$Hombres))*1000; TBM_H_2015

# Tasas Espec?ficas de Mortalidad

TEM_2015 = Def_2015

TEM_2015$Hombres = (Def_2015$Hombres/Pob_Mitad2015_Geom$Hombres)*1000

TEM_2015

# Tasa Tipificada de Mortalidad

TTM_H = (sum((TEM_2015$Hombres/1000)*Pob_Mitad2020_Geom$Hombres)/sum(Pob_Mitad2020_Geom$Hombres))*1000; TTM_H

# Tasa de Mortalidad Infantil

TMI_H_2015 <- (Def_2015$Hombres[1]/sum(Nac_2015$Hombres))*1000; TMI_H_2015

#MUJERES
# Tasa Bruta de Mortalidad
TBM_M_2015 = (sum(Def_2015$Mujeres)/sum(Pob_Mitad2015_Geom$Mujeres))*1000; TBM_M_2015

# Tasas Espec?ficas de Mortalidad


TEM_2015$Mujeres = (Def_2015$Mujeres/Pob_Mitad2015_Geom$Mujeres)*1000
TEM_2015$Mujeres


# Tasa Tipificada de Mortalidad

TTM_M = (sum((TEM_2015$Mujeres/1000)*Pob_Mitad2020_Geom$Mujeres)/sum(Pob_Mitad2020_Geom$Mujeres))*1000; TTM_M

# Tasa de Mortalidad Infantil

TMI_M_2015 <- (Def_2015$Mujeres[1]/sum(Nac_2015$Mujeres))*1000; TMI_M_2015


# ************************ 2020 ****************************
#NOTA: La tasa tipificada de mortalidad es una sola, de manera que no se calculara otra vez.
####Carga de bases de datos####
Def_2020 <- read.csv( "C:/Users/hp/Documents/UNI/Actuaria/Demografia/Def2020.csv")
Nac_2020 <- read.csv( "C:/Users/hp/Documents/UNI/Actuaria/Demografia/Nac2020.csv")



Pob_Mitad2020_Geom = Pob_Mitad2020_Geom[,-1]

colnames(Def_2020) <- c("Edad", "Hombres", "Mujeres")
colnames(Nac_2020) <- c("Edad", "Hombres", "Mujeres")
Def_2020$Mujeres<- as.numeric(Def_2020$Mujeres)
Def_2020$Hombres<- as.numeric(Def_2020$Hombres)
Nac_2020$Hombres<- as.numeric(Nac_2020$Hombres)

Def_2020[is.na(Def_2020)] = 0
Nac_2020[is.na(Nac_2020)] = 0
#HOMBRES
# Tasa Bruta de Mortalidad
TBM_H_2020 = (sum(Def_2020$Hombres)/sum(Pob_Mitad2020_Geom$Hombres))*1000; TBM_H_2020

# Tasas Espec?ficas de Mortalidad

TEM_2020 = Def_2020

TEM_2020$Hombres = (Def_2020$Hombres/Pob_Mitad2020_Geom$Hombres)*1000
View(TEM_2020)
# Tasa de Mortalidad Infantil

TMI_H_2020 <- (Def_2020$Hombres[1]/sum(Nac_2020$Hombres))*1000; TMI_H_2020

#MUJERES
# Tasa Bruta de Mortalidad
TBM_M_2020 = (sum(Def_2020$Mujeres)/sum(Pob_Mitad2020_Geom$Mujeres))*1000; TBM_M_2020

# Tasas Espec?ficas de Mortalidad


TEM_2020$Mujeres = (Def_2020$Mujeres/Pob_Mitad2020_Geom$Mujeres)*1000
TEM_2020$Mujeres

# Tasa de Mortalidad Infantil

TMI_M_2020 <- (Def_2020$Mujeres[1]/sum(Nac_2020$Mujeres))*1000; TMI_M_2020

# ************************ TABLAS DE VIDA ****************************

Tabla_Vida <- function(Defunciones, Poblacion, Edad, TMI){
  #Definimos la columna correspondiente a los grupos de edades
  Edades <-Edad
  Edades = c(0,1,seq(5,85, by=5))
  #Definimos n
  n<-c()
  n[1] = 1
  n[2] = 4
  n[3:18] = 5
  n[19] = NA
  
  #Definimos nDx
  nDx = Defunciones
  
  #Definimos nNx
  nNx = Poblacion
  
  #Definimos nMx
  nMx = nDx/nNx
  
  #Definimos nmx
  nmx <- c()
  nmx[1] = TMI
  nmx[2:19] = nMx[2:19]
  
  #Definimos nax
  nax = n/2
  nax[19] = NA
  
  #Definimos nqx
  nqx=c()
  nqx[2:18] = (n[2:18]*nmx[2:18])/(1+(n[2:18]-nax[2:18])*nmx[2:18])
  nqx[1] = nmx[1]
  nqx[19] = 1
  
  #Definimos npx 
  npx = 1-nqx
  
  #Definimos lx
  lx<-c()
  lx[1] = 100000
  for(i in 2:19){
    lx[i] = lx[i-1]*npx[i-1]
  }
  
  #Definimos ndx
  
  ndx = lx*nqx
  
  #Definimos nLx
  nLx=c()
  for(i in 1:18){
    nLx[i] = (n[i]*lx[i+1])+(nax[i]*ndx[i])
  }
  nLx[19] = lx[19]/nMx[19]
  
  
  #Definimos Tx
  Tx = c()
  for(i in 1:19){
    Tx[i] = sum(nLx[i:19])
  }
  
  #Definimos ex
  ex = Tx/lx
  
  #Juntamos todos los elementos de la tabla de vida
  Tabla_Vida <- data.frame(Edades,n,nax,nDx,nNx,nMx,nmx,nqx,npx,lx,ndx,nLx,Tx,ex)
  
}


#Tabla de vida de hombres del 2015
TV_H_2015 = Tabla_Vida(Def_2015$Hombres,Pob_Mitad2015_Geom$Hombres, Def_2015$Edad, TMI_H_2015/1000)
View(TV_H_2015)

#Tabla de vida de mujeres del 2015
TV_M_2015 = Tabla_Vida(Def_2015$Mujeres,Pob_Mitad2015_Geom$Mujeres, Def_2015$Edad, TMI_M_2015/1000)
View(TV_M_2015)

#Tabla de vida de hombres del 2020
TV_H_2020 = Tabla_Vida(Def_2020$Hombres,Pob_Mitad2020_Geom$Hombres, Def_2020$Edad, TMI_H_2020/1000)
View(TV_H_2020)

#Tabla de vida de mujeres del 2020
TV_M_2020 = Tabla_Vida(Def_2020$Mujeres,Pob_Mitad2020_Geom$Mujeres, Def_2020$Edad, TMI_M_2020/1000)
View(TV_M_2020)

#Grafica esperanza de vida hombres y mujeres 2015
plot(TV_H_2015$ex[1:19], type = "o", 
     main = "Esperanza de vida 2015",
     xlab = "Edad", ylab = "Esperanza de vida", col = "blue", xlim = c(0,19), 
     ylim = c(0, 100), xaxt = "n")
axis(1, at = seq(1:19), labels = c("0","1","5","10","15","20",
                                   "25","30","35","40","45","50",
                                   "55","60","65","70","75", "80", "85 y mas"))
lines(x = TV_M_2015$ex[1:19], type = "o", col = "red")
legend(x = "topright", legend = c("Hombres", "Mujeres"), lty = c(1,1), 
       col = c("blue", "red"), pch = c(1,1))

#Grafica esperanza de vida hombres y mujeres 2020
plot(TV_H_2020$ex[1:19], type = "o", 
     main = "Esperanza de vida 2020",
     xlab = "Edad", ylab = "Esperanza de vida", col = "blue", xlim = c(0,19), 
     ylim = c(0, 100), xaxt = "n")
axis(1, at = seq(1:19), labels = c("0","1","5","10","15","20",
                                   "25","30","35","40","45","50",
                                   "55","60","65","70","75", "80", "85 y mas"))
lines(x = TV_M_2020$ex[1:19], type = "o", col = "red")
legend(x = "topright", legend = c("Hombres", "Mujeres"), lty = c(1,1), 
       col = c("blue", "red"), pch = c(1,1))

#Grafica esperanza de vida hombres 2015 y 2020
plot(TV_H_2015$ex[1:19], type = "o", 
     main = "Esperanza de vida Hombres 2015/2020",
     xlab = "Edad", ylab = "Esperanza de vida", col = "blue", xlim = c(0,19), 
     ylim = c(0, 100), xaxt = "n")
axis(1, at = seq(1:19), labels = c("0","1","5","10","15","20",
                                   "25","30","35","40","45","50",
                                   "55","60","65","70","75", "80", "85 y mas"))
lines(x = TV_H_2020$ex[1:19], type = "o", col = "red")
legend(x = "topright", legend = c("Hombres 2015", "Hombres 2020"), lty = c(1,1), 
       col = c("blue", "red"), pch = c(1,1))
#Grafica esperanza de vida mujeres 2015 y 2020
plot(TV_M_2015$ex[1:19], type = "o", 
     main = "Esperanza de vida Mujeres 2015/2020",
     xlab = "Edad", ylab = "Esperanza de vida", col = "blue", xlim = c(0,19), 
     ylim = c(0, 100), xaxt = "n")
axis(1, at = seq(1:19), labels = c("0","1","5","10","15","20",
                                   "25","30","35","40","45","50",
                                   "55","60","65","70","75", "80", "85 y mas"))
lines(x = TV_M_2020$ex[1:19], type = "o", col = "red")
legend(x = "topright", legend = c("Mujeres 2015", "Mujeres 2020"), lty = c(1,1), 
       col = c("blue", "red"), pch = c(1,1))



# ************************ RELACIONES DE SOBREVIVIENCIA ****************************
Rel_Sobrevivencia = function (nLx){
  nLx[1] = nLx [1] + nLx [2]
  nLx = nLx[-2]
  
  x=c()
  x = seq(0,80,by = 5)
  n = rep(5,17)
  n[17]=NA
  S = c()
  for (i in 1:16){
    S[i] = nLx[i+1]/nLx[i]
  }
  S[17] = nLx[18]/(nLx[17] + nLx[18])
  
  Rel_Sob = data.frame(x,n,S)
  
}

Rel_Sobrevivencia_Nac = function (nLx, radix){
  nLx[1] = nLx [1] + nLx [2]
  nLx = nLx[-2]
  radix = radix
  Rel_Sob_Nac = nLx[1]/(5*radix)
}

#2015 Hombres
S_H_2015 = Rel_Sobrevivencia(TV_H_2015$nLx); S_H_2015
S_Nac_H_2015 = Rel_Sobrevivencia_Nac(TV_H_2015$nLx, TV_H_2015$lx[1]); S_Nac_H_2015

#2015 Mujeres
S_M_2015 = Rel_Sobrevivencia(TV_M_2015$nLx); S_M_2015
S_Nac_M_2015 = Rel_Sobrevivencia_Nac(TV_M_2015$nLx, TV_M_2015$lx[1]); S_Nac_M_2015

#2020 Hombres
S_H_2020 = Rel_Sobrevivencia(TV_H_2020$nLx); S_H_2020
S_Nac_H_2020 = Rel_Sobrevivencia_Nac(TV_H_2020$nLx, TV_H_2020$lx[1]); S_Nac_H_2020

#2020 Mujeres
S_M_2020 = Rel_Sobrevivencia(TV_M_2020$nLx); S_M_2020
S_Nac_M_2020 = Rel_Sobrevivencia_Nac(TV_M_2020$nLx, TV_M_2020$lx[1]); S_Nac_M_2020



# ==============================================================================
#                                   Parte 4
# ==============================================================================


library(tidyverse)
library(openxlsx)
options(scipen=999)

setwd( )

######**********      0. INFORMACION                 **********######

#2015 
## Carga de datoS

Nac_2015 <- Nac_2015 %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(Total := Hombres + Mujeres)

Pob_Mitad2015_Geom <- Pob_Mitad2015_Geom %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(Total := Hombres + Mujeres)

######**********      1. INDICADORES DE FECUNDIDAD  **********####

# Tasa Bruta de Natalidad
TBN_2015 <- ( sum(Nac_2015$Total)/sum(Pob_Mitad2015_Geom$Total) )*1000
TBN_2015
# Tasa Fecundidad General
# Los inidces de la suma deben cubrir el periodo fértil de la mujer
TFG_2015 <- ( sum(Nac_2015$Total/sum(Pob_Mitad2015_Geom$Mujeres[5:11])) ) * 1000
TFG_2015
# Tasas Especificas de Fecundidad
# Los inidces de la suma deben cubrir el periodo fértil de la mujer
TEF_2015 <- data.frame(Edad = Nac_2015$Edad,
                       Pob_Femenina = Pob_Mitad2015_Geom$Mujeres[5:11],
                       Nacimientos = Nac_2015$Total) %>% 
  mutate(TEF := Nacimientos/Pob_Femenina)
TEF_2015
# Tasa Global de Fecundidad
TGF_2015 <- 5 * sum(TEF_2015$TEF)
TGF_2015

# Tasa Bruta de Reproduccion
K_2015 <- sum(Nac_2015$Mujeres)/sum(Nac_2015$Total)
TBR_2015 <- K * TGF_2015
TBR_2015
#####Tasa Neta de Reproduccion 
px_2015=TV_M_2015$nLx[5:11]/(5*TV_M_2015$lx[1]); px_2015
TNR_2015 =5*K_2015* sum(px_2015*TEF_2015$TEF); TNR_2015 

#2020
## Carga de datoS

Nac_2020 <- Nac_2020 %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(Total := Hombres + Mujeres)

Pob_Mitad2020_Geom <- Pob_Mitad2020_Geom %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(Total := Hombres + Mujeres)

######**********      1. INDICADORES DE FECUNDIDAD  **********####

# Tasa Bruta de Natalidad
TBN_2020 <- ( sum(Nac_2020$Total)/sum(Pob_Mitad2020_Geom$Total) )*1000
TBN_2020
# Tasa Fecundidad General
# Los inidces de la suma deben cubrir el periodo fértil de la mujer
TFG_2020 <- ( sum(Nac_2020$Total/sum(Pob_Mitad2020_Geom$Mujeres[5:11])) ) * 1000
TFG_2020
# Tasas Especificas de Fecundidad
# Los inidces de la suma deben cubrir el periodo fértil de la mujer
TEF_2020 <- data.frame(Edad = Nac_2020$Edad,
                       Pob_Femenina = Pob_Mitad2020_Geom$Mujeres[5:11],
                       Nacimientos = Nac_2020$Total) %>% 
  mutate(TEF := Nacimientos/Pob_Femenina)
TEF_2020
# Tasa Global de Fecundidad
TGF_2020 <- 5 * sum(TEF_2020$TEF)
TGF_2020

# Tasa Bruta de Reproduccion
K_2020 <- sum(Nac_2020$Mujeres)/sum(Nac_2020$Total)
TBR_2020 <- K * TGF_2020
TBR_2020
#####Tasa Neta de Reproduccion 
px_2020=TV_M_2020$nLx[5:11]/(5*TV_M_2020$lx[1]); px_2020
TNR_2020 =5*K_2020* sum(px_2020*TEF_2020$TEF); TNR_2020 
TNR_2020
