setwd("C:/Users/Fran/Documents")
file <- paste0(getwd(),"/Libro1-1.csv")
data <- read.csv2(file=file,head=TRUE)
str(data)

#media de variables
mIGPA <- mean(data$IGPA)
mNORTEGRAN <- mean(data$RETORNONORTEGRAN)
mHITES <- mean(data$RETORNOHITES)
mNUEVAPOLAR <- mean(data$RETORNONUEVAPOLAR)

#desviación de variables
dIGPA <- sd(data$RETORNOIGPA)
dNORTEGRAN <- sd(data$RETORNONORTEGRAN)
dHITES <- sd(data$RETORNOHITES)
dNUEVAPOLAR <- sd(data$RETORNONUEVAPOLAR)

#correlación entre retornos
w <- data.frame(data$RETORNOIGPA,data$RETORNONORTEGRAN,data$RETORNOHITES,data$RETORNONUEVAPOLAR)
Matrizcor <- cor(w)

#Regresión lineal NORTE GRANDE S.A.
nortegrande <- lm(data$RETORNONORTEGRAN ~ data$RETORNOIGPA , data=data)
summary(nortegrande)

#regresión lineal HITES S.A.
hites <- lm(data$RETORNOHITES ~ data$RETORNOIGPA , data=data)
summary(hites)

#regresión lineal NUEVA POLAR S.A.
nuevapolar <- lm(data$RETORNONUEVAPOLAR ~ data$RETORNOIGPA , data=data)
summary(nuevapolar)

#en caso de no tener el paquete instalado para graficar, se debe sacar el "#" del comentario siguiente
#install.packages("ggplot2")
library(ggplot2)

#grafico de variables en relación a la fecha
qplot (data$FECHA, data$RETORNOIGPA, data=data)
qplot (data$FECHA, data$RETORNONORTEGRAN, data=data)
qplot (data$FECHA, data$RETORNOHITES, data=data)
qplot (data$FECHA, data$RETORNONUEVAPOLAR, data=data)

