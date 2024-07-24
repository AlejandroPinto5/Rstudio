############
#Agregamoslibrerias
library(readxl)
library(agricolae)
library(RColorBrewer)
####################################

x = EstaturaEstudiantes$ESTUDIANTES # Guardamos la columna 'Numero-de-Libros' del libro de excel 'LibrosEnHogares' en la variable 'x'
n  = length(x) # Guardamos el tamaño de la columna 'Numero-de-libro' en la variable 'n'
m = ceiling(1+log2(n)) # Calculamos y guardamos el numero del intervalo de clase en la variable 'm'

listx = hist(x,plot = FALSE, breaks = seq(min(x),max(x), by=(max(x)-min(x))/m)) # Calculamos y guardamos el tamaños del intervalo de clase en la variable 'listx'
tabfrq = table.freq(listx) # Calculamos y guardamos la tabla de frecuencias y probabilidades en la variable 'tabfrq'

#############################################
media = mean(x) #Calculamos la media
mediana=median(x) #Calculamos la mediana

#Debido a que no pude instalar DEvsTools, cree una funcion para calcular la moda
moda1 = function(x){ 
  return(as.numeric(names(which.max(table(EstaturaEstudiantes$ESTUDIANTES)))))
}
moda=moda1(x) #Calculamos la moda
varianza = var(x) #Calculamos la varianza
desvstand = sd(x) #Calculamos la desviacion estandar
coefVariaz = (desvstand/media)*100 #Calculamos el coeficiente de varianza
cuartiles = quantile(x,probs = c(0.25,0.5,0.75)) #Calculamos tres cuartiles
percentilTreinta = quantile(x, probs = 0.3) #Calculamos el percentil 30
coefApunta = kurtosis(x) #Calculamos el coeficiente de apuntammiento
coefAsimetr = skewness(x) #Calculamos el coeficiente de asimetria

##################
#dev.off()
layout(matrix(c(1:2),ncol = 1, nrow=2, byrow=TRUE), heights = c(3,1))
layout.show(2)

#################
coulor = brewer.pal(8, "Accent") # Guardamos la paleta de colores a usar mas adelante
par(mar = c(3.8,4.1,3.1,0))
h=graph.freq(listx$breaks, counts = listx$counts ,frequency = 2,xlim= c(130,150), xlab = "Estatura de Estudiantes",ylim =c(0,0.2), ylab = "Frecuencias Relativas", main="Estatura de Estudiantes en una Istitucion Educativa", col = coulor) # Creamos y estilizamos la grafica de histograma
#Lineas de media, mediana y moda
a=abline(v =media, col= "red", lwd = 1.5)
text(139.5,0.20, "140.46", col="red")
b=abline(v =mediana, col= "black", lwd = 1.5)
text(142,0.20, "141.01", col="black")
c=abline(v =moda, col= "blue", lwd = 1.5)
text(137,0.20, "137.9", col="blue")
legend(129,0.23,c("Media", "Mediana", "Moda"),c(a,b,c), fill = c("red", "black", "blue"), bty = "n" )

##########################################################################
#Funcion para generar el grafico de caja y bigotes con cuartiles
cuart  = quantile(x) # Calculamos los cuartiles completos
print(cuart) # Verificamos que sean acordes con la teoria
par(bty = "n", mar = c(0,4.1,0,0))
boxplot(cuart, horizontal = TRUE, col = coulor) # GRaficamos los cuartiles en una grafica de cajas y bigotes

############################################################################
#Tablas donde muestra todas las medidad estadisticas calculada anteriormente
listx
cat("Media: ",media, "\n", "Mediana: ", mediana, "\n", "Moda: ", moda, "\n", "Standard Deviation: ", desvstand, "\n", "Coeficiente de Varianza: ", coefVariaz,"\n", "Cuartiles: ", cuartiles, "\n" , "Percentil 30%: ", percentilTreinta, "\n", "Coeficiente de Apuntamiento: ", coefApunta, "\n",  "Coeficiente de Asimetrica: ", coefAsimetr)
tabfrq


