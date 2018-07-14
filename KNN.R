"En el archivo productos.txt tenemos un registro de productos que fueron exitosos y otros que fracasaron, junto con información de su precio y de su presupuesto en marketing.

a) Levantar los datos del archivo y graﬁcarlos, en negro los productos “exitosos” y en rojo los “fracasados”. 

b) Para determinar un valor óptimo k de vecinos m´as cercanos se propone: 
  1) Estandarizar el total de los datos con alguno de los dos esquemas propuestos. 
  2) Particionar el conjunto en un training y un testing set. 
  3) Para un valor de k y un mecanismo de estandarizaci´on clasiﬁcar los elementos del testing set en función del training set, aplicar como función de costo la cantidad de datos incorrectamente clasiﬁcados. 
  4) Repetir con diversos valores de k y elegir como modelo aquel que minimice la funci´on de costo. 

c) Replique el ejercicio anterior con un mecanismo de K-Fold para elegir el k de vecinos más cercanos. 

d) Clasiﬁcar un producto con precio $70 y un presupuesto de marketing de $100000 utilizando el modelo óptimo encontrado antes."

#a)
datos <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/Ejercicios KNN/producto.txt", header=TRUE)

exito <- subset(datos, datos$Resultado=="exito")

fracaso <- subset(datos, datos$Resultado=="fracaso")

plot(datos$Precio,datos$Marketing, xlab="Precio", ylab="Marketing", col=ifelse(datos$Resultado=="exito", "black", "red"))

#b)1) Estandarizo los datos

estandarizacion <- function(x){
  
  aux <- (x-min(x))/(max(x)-min(x))
  return(aux)
}

dat_2 <- estandarizacion(datos[2])
dat_3 <- estandarizacion(datos[3])

Resultado <- datos$Resultado
datos_estand <- data.frame(Resultado, dat_2, dat_3)

#b)2) Separo los datos en training y testing set

set.seed(27)
index <- sample(1:nrow(datos_estand), size= 0.7*nrow(datos_estand))
train_set <- datos_estand[index,]
test_set <- datos_estand[-index,]

#b)3) Aplico knn usando como función de costo la cantidad de datos incorrectamente clasificados.

knn.classification <- function(train_set, test_set, k){
  
  predicted_results <- c(nrow(test_set))
  
  for (i in 1:nrow(test_set)){
    
    test_set_point <- test_set[i,]
    knn <- calculate_k_minor_distances(test_set_point, train_set, k)
    predicted_results[i] <- predicted.results(knn)
  }
  return(predicted_results)
}

calculate_k_minor_distances <- function(testing_set_point, train_set, k){
  
  k_minor_distance <- as.data.frame(matrix(Inf, ncol=2, nrow=k))
  for(i in 1:nrow(train_set)){
    
    dist <- as.double(sqrt((train_set[i,2]-testing_set_point[2])**2 + (train_set[i,3]-testing_set_point[3])**2))
    for(j in 1:nrow(k_minor_distance)){

      if(dist < k_minor_distance[j,1]){
      
        k_minor_distance[nrow(k_minor_distance),1] <- dist
        k_minor_distance[nrow(k_minor_distance),2] <- train_set[i,1]
        k_minor_distance <- k_minor_distance[order(k_minor_distance$V1),]
        
        break
      }
    }
  }
  return(k_minor_distance)
}

predicted.results <- function(predictions){
  
  for(i in 1:nrow(predictions)){
    if(predictions[i,2] == 1){
      predictions[i,2] <- "exito"
    }
    else{
      predictions[i,2] <- "fracaso"
    }
  }
  #cuenta la cantidad de "éxito" y "fracaso" que hay 
  results <- table(predictions$V2)

  #la clasificación de la mayoría de los k vecinos más cercanos
  predicted_result <- names(which(results == max(results)))

  return(predicted_result)
}
 
#Función para calcular el costo total 
total.cost <- function (predicted_vs_real){
  
  cost <- 0
  
  for(i in 1:nrow(predicted_vs_real)){
    
    if(predicted_vs_real[i,1] != predicted_vs_real[i,2]){
      
        cost <- cost + 1
    }
  } 
  return(cost/nrow(predicted_vs_real))
}

apply.knn <- function(train_set, test_set, k){
  
  predictions <- knn.classification(train_set, test_set, k)
  predictions <- factor(predictions, levels=c("exito", "fracaso"))
  predicted_vs_real <- data.frame(predictions, test_set$Resultado)
  cost <- total.cost(predicted_vs_real)
  return(cost)
}

#Pruebo con k=4
cost <- apply.knn(train_set, test_set, 4)
#Costo = 0.2142857

#Pruebo varios valores de k y analizo el costo de cada uno.
k <- c(1,3,5,7,9)
cost <- matrix(nrow = 5, ncol = 2)

for (i in 1:5){
  
  cost[i,1] <- k[i]
  cost[i,2] <- apply.knn(train_set, test_set, k[i])
}

#Matriz con los k probados y sus respectivos costos.
cost

#Con los k probados, el que genera menor costo es k = 7. 

#c) Utilizo K-Fold para encontrar el k que minimice la función de costo.

#Creo los sets para K=10. 
index<-sample(1:nrow(datos_estand), size=0.1*nrow(datos))
conj1.test <- datos_estand[index,]
aux <- datos[-index,]

index<-sample(1:nrow(aux), size=(1/9)*nrow(aux))
conj2.test <- aux[index,]
aux <- aux[-index,]

index<-sample(1:nrow(aux), size=(1/8)*nrow(aux))
conj3.test <- aux[index,]
aux <- aux[-index,]

index<-sample(1:nrow(aux), size=(1/7)*nrow(aux))
conj4.test <- aux[index,]
aux <- aux[-index,]

index<-sample(1:nrow(aux), size=(1/6)*nrow(aux))
conj5.test <- aux[index,]
aux <- aux[-index,]

index<-sample(1:nrow(aux), size=(1/5)*nrow(aux))
conj6.test <- aux[index,]
aux <- aux[-index,]

index<-sample(1:nrow(aux), size=(1/4)*nrow(aux))
conj7.test <- aux[index,]
aux <- aux[-index,]

index<-sample(1:nrow(aux), size=(1/3)*nrow(aux))
conj8.test <- aux[index,]
aux <- aux[-index,]

index<-sample(1:nrow(aux), size=(1/2)*nrow(aux))
conj9.test <- aux[index,]
conj10.test <- aux[-index,]



conj1.train <- rbind(conj2.test,conj3.test,conj4.test,conj5.test,conj6.test,conj7.test,conj8.test,conj9.test,conj10.test)
conj2.train <- rbind(conj1.test,conj3.test,conj4.test,conj5.test,conj6.test,conj7.test,conj8.test,conj9.test,conj10.test)
conj3.train <- rbind(conj1.test,conj2.test,conj4.test,conj5.test,conj6.test,conj7.test,conj8.test,conj9.test,conj10.test)
conj4.train <- rbind(conj1.test,conj2.test,conj3.test,conj5.test,conj6.test,conj7.test,conj8.test,conj9.test,conj10.test)
conj5.train <- rbind(conj1.test,conj2.test,conj3.test,conj4.test,conj6.test,conj7.test,conj8.test,conj9.test,conj10.test)
conj6.train <- rbind(conj1.test,conj2.test,conj3.test,conj4.test,conj5.test,conj7.test,conj8.test,conj9.test,conj10.test) 
conj7.train <- rbind(conj1.test,conj2.test,conj3.test,conj4.test,conj5.test,conj6.test,conj8.test,conj9.test,conj10.test)
conj8.train <- rbind(conj1.test,conj2.test,conj3.test,conj4.test,conj5.test,conj6.test,conj7.test,conj9.test,conj10.test)
conj9.train <- rbind(conj1.test,conj2.test,conj3.test,conj4.test,conj5.test,conj6.test,conj7.test,conj8.test,conj10.test)
conj10.train <- rbind(conj1.test,conj2.test,conj3.test,conj4.test,conj5.test,conj6.test,conj7.test,conj8.test,conj9.test)


knn.mean.k1 <- apply.knn(conj1.train,conj1.test,1) + apply.knn(conj2.train,conj2.test,1) + apply.knn(conj3.train,conj3.test,1) + apply.knn(conj4.train,conj4.test,1) + apply.knn(conj5.train,conj5.test,1) + apply.knn(conj6.train,conj6.test,1) + apply.knn(conj7.train,conj7.test,1) + apply.knn(conj8.train,conj8.test,1) + apply.knn(conj9.train,conj9.test,1) + apply.knn(conj10.train, conj10.test, 1)
#Resultado: 2.6
knn.mean.k3 <- apply.knn(conj1.train,conj1.test,3) + apply.knn(conj2.train,conj2.test,3) + apply.knn(conj3.train,conj3.test,3) + apply.knn(conj4.train,conj4.test,3) + apply.knn(conj5.train,conj5.test,3) + apply.knn(conj6.train,conj6.test,3) + apply.knn(conj7.train,conj7.test,3) + apply.knn(conj8.train,conj8.test,3) + apply.knn(conj9.train,conj9.test,3) + apply.knn(conj10.train, conj10.test, 3)
#Resultado: 2
knn.mean.k5 <- apply.knn(conj1.train,conj1.test,5) + apply.knn(conj2.train,conj2.test,5) + apply.knn(conj3.train,conj3.test,5) + apply.knn(conj4.train,conj4.test,5) + apply.knn(conj5.train,conj5.test,5) + apply.knn(conj6.train,conj6.test,5) + apply.knn(conj7.train,conj7.test,5) + apply.knn(conj8.train,conj8.test,5) + apply.knn(conj9.train,conj9.test,5) + apply.knn(conj10.train, conj10.test, 5)
#Resultado: 2.3
knn.mean.k7 <- apply.knn(conj1.train,conj1.test,7) + apply.knn(conj2.train,conj2.test,7) + apply.knn(conj3.train,conj3.test,7) + apply.knn(conj4.train,conj4.test,7) + apply.knn(conj5.train,conj5.test,7) + apply.knn(conj6.train,conj6.test,7) + apply.knn(conj7.train,conj7.test,7) + apply.knn(conj8.train,conj8.test,7) + apply.knn(conj9.train,conj9.test,7) + apply.knn(conj10.train, conj10.test, 7)
#Resultado: 2.3
knn.mean.k9 <- apply.knn(conj1.train,conj1.test,9) + apply.knn(conj2.train,conj2.test,9) + apply.knn(conj3.train,conj3.test,9) + apply.knn(conj4.train,conj4.test,9) + apply.knn(conj5.train,conj5.test,9) + apply.knn(conj6.train,conj6.test,9) + apply.knn(conj7.train,conj7.test,9) + apply.knn(conj8.train,conj8.test,9) + apply.knn(conj9.train,conj9.test,9) + apply.knn(conj10.train, conj10.test, 9)
#Resultado: 2.5

#el k que genera menor costo es k=3

#Gráfico de costo vs. k
costo_de_k <- c(knn.mean.k1, knn.mean.k3, knn.mean.k5, knn.mean.k7, knn.mean.k9)

k <- c(1,3,5,7,9)

costo.vs.k  <- cbind(k, costo_de_k)
plot(costo.vs.k, ylab = "costo")
#Se puede ver que el menor costo se da en k=3

#d)Clasifico un nuevo producto con precio $70 y presupuesto de marketing de $100000 utilizando el modelo de k=3.

dato.nuevo <- data.frame(0,70,100000)
#primer parametro es 0 pero podría ser cualquier número, ya que no se utiliza al predecir. 

knn.pred <- knn.classification(datos_estand, dato.nuevo, 3)

knn.pred
#Resultado: exito
