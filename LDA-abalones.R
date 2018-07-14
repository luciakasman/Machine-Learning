#Efectúe una clasificación por LDA de los datos abalone.txt, discriminando entre adulto e
#infante, utilizando aquellas variables que considere más pertinenes. Elija el mejor modelo, usando
#criterio, sentido común y alguno de los métdos propuestos vistos en clase para determinarlo. Se
#recomienda separar inicialmente un 20 %, 30 % de lo datos para poder hacer la evaluación final a
#través de una matriz de confusión.



library(MASS)
library(lattice)

datos<-read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/abalone.txt", sep=",")

#Reemplazo los "M" y "F" de la columna Sexo por "A" de Adulto

datos$V1<-gsub('M','A',datos$V1)
datos$V1<-gsub('F','A',datos$V1)

#separo en training set y testing set

attach(datos)

set.seed(17)
index<-sample(1:nrow(datos), size=0.7*nrow(datos))
train = datos[index,]
test = datos[-index,]

#LDA con las 4 variables (V2,V3,V4 y V5)

lda.fit1<-lda(V1~V2+V3+V4+V5,data=train)
lda.fit1

x11()
plot(lda.fit1)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset<-predict(lda.fit1,train)
mean(lda.pred_trainset$class==train$V1)

lda.predict<-predict(lda.fit1,test)

#Tabla de confusión
table(lda.predict$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict$class==test$V1)
#Resultado: 0.8030303

#------------------------------------
#LDA con la variable V2

lda.fit2<-lda(V1~V2,data=train)
lda.fit2

x11()
plot(lda.fit2)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset2<-predict(lda.fit2,train)
mean(lda.pred_trainset2$class==train$V1)

lda.predict2<-predict(lda.fit2,test)

#Tabla de confusión
table(lda.predict2$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict2$class==test$V1)
#Resultado: 0.7886762

#------------------------------------
#LDA con la variable V3

lda.fit3<-lda(V1~V3,data=train)
lda.fit3

x11()
plot(lda.fit3)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset3<-predict(lda.fit3,train)
mean(lda.pred_trainset3$class==train$V1)

lda.predict3<-predict(lda.fit3,test)

#Tabla de confusión
table(lda.predict3$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict3$class==test$V1)
#Resultado: 0.7926635

#------------------------------------
#LDA con la variable V4

lda.fit4<-lda(V1~V4,data=train)
lda.fit4

x11()
plot(lda.fit4)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset4<-predict(lda.fit4,train)
mean(lda.pred_trainset4$class==train$V1)

lda.predict4<-predict(lda.fit4,test)

#Tabla de confusión
table(lda.predict4$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict4$class==test$V1)
#Resultado: 0.7886762

#------------------------------------
#LDA con la variable V5

lda.fit5<-lda(V1~V5,data=train)
lda.fit5

x11()
plot(lda.fit5)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset5<-predict(lda.fit5,train)
mean(lda.pred_trainset5$class==train$V1)

lda.predict5<-predict(lda.fit5,test)

#Tabla de confusión
table(lda.predict5$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict5$class==test$V1)
#Resultado: 0.8070175

#------------------------------------
#LDA con las variables V2 y V3

lda.fit6<-lda(V1~V2+V3,data=train)
lda.fit6

x11()
plot(lda.fit6)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset6<-predict(lda.fit6,train)
mean(lda.pred_trainset6$class==train$V1)

lda.predict6<-predict(lda.fit6,test)

#Tabla de confusión
table(lda.predict6$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict6$class==test$V1)
#Resultado: 0.791866

#------------------------------------
#LDA con las variables V2 y V4

lda.fit7<-lda(V1~V2+V4,data=train)
lda.fit7

x11()
plot(lda.fit7)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset7<-predict(lda.fit7,train)
mean(lda.pred_trainset7$class==train$V1)

lda.predict7<-predict(lda.fit7,test)

#Tabla de confusión
table(lda.predict7$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict7$class==test$V1)
#Resultado: 0.7926635

#------------------------------------
#LDA con las variables V2 y V5

lda.fit8<-lda(V1~V2+V5,data=train)
lda.fit8

x11()
plot(lda.fit8)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset8<-predict(lda.fit8,train)
mean(lda.pred_trainset8$class==train$V1)

lda.predict8<-predict(lda.fit8,test)

#Tabla de confusión
table(lda.predict8$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict8$class==test$V1)
#Resultado: 0.7990431

#------------------------------------
#LDA con las variables V3 y V4

lda.fit9<-lda(V1~V3+V4,data=train)
lda.fit9

x11()
plot(lda.fit9)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset9<-predict(lda.fit9,train)
mean(lda.pred_trainset9$class==train$V1)

lda.predict9<-predict(lda.fit9,test)

#Tabla de confusión
table(lda.predict9$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict9$class==test$V1)
#Resultado: 0.7950558

#------------------------------------
#LDA con las variables V3 y V5

lda.fit10<-lda(V1~V3+V5,data=train)
lda.fit10

x11()
plot(lda.fit10)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset10<-predict(lda.fit10,train)
mean(lda.pred_trainset10$class==train$V1)

lda.predict10<-predict(lda.fit10,test)

#Tabla de confusión
table(lda.predict10$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict10$class==test$V1)
#Resultado: 0.7990431

#------------------------------------
#LDA con las variables V4 y V5

lda.fit11<-lda(V1~V4+V5,data=train)
lda.fit11

x11()
plot(lda.fit11)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset11<-predict(lda.fit11,train)
mean(lda.pred_trainset11$class==train$V1)

lda.predict11<-predict(lda.fit11,test)

#Tabla de confusión
table(lda.predict11$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict11$class==test$V1)
#Resultado: 0.8102073


#------------------------------------
#LDA con las variables V2, V3 y V4

lda.fit12<-lda(V1~V2+V3+V4,data=train)
lda.fit12

x11()
plot(lda.fit12)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset12<-predict(lda.fit12,train)
mean(lda.pred_trainset12$class==train$V1)

lda.predict12<-predict(lda.fit12,test)

#Tabla de confusión
table(lda.predict12$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict12$class==test$V1)
#Resultado: 0.7990431

#------------------------------------
#LDA con las variables V2, V3 y V5

lda.fit13<-lda(V1~V2+V3+V5,data=train)
lda.fit13

x11()
plot(lda.fit13)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset13<-predict(lda.fit13,train)
mean(lda.pred_trainset13$class==train$V1)

lda.predict13<-predict(lda.fit13,test)

#Tabla de confusión
table(lda.predict13$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict13$class==test$V1)
#Resultado: 0.8046252

#------------------------------------
#LDA con las variables V3, V4 y V5

lda.fit14<-lda(V1~V3+V4+V5,data=train)
lda.fit14


x11()
plot(lda.fit14)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset14<-predict(lda.fit14,train)
mean(lda.pred_trainset14$class==train$V1)

lda.predict14<-predict(lda.fit14,test)

#Tabla de confusión
table(lda.predict14$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict14$class==test$V1)
#Resultado: 0.8046252

#------------------------------------
#LDA con las variables V2, V4 y V5

lda.fit15<-lda(V1~V2+V4+V5,data=train)
lda.fit15

x11()
plot(lda.fit15)

#Porcentaje de aciertos con el set de entrenamiento
lda.pred_trainset15<-predict(lda.fit15,train)
mean(lda.pred_trainset15$class==train$V1)

lda.predict15<-predict(lda.fit15,test)

#Tabla de confusión
table(lda.predict15$class,test$V1)

#Cuántos quedaron bien clasificados
mean(lda.predict15$class==test$V1)
#Resultado: 0.8086124



#Entonces el modelo que mejor funciona es el creado con las variables V4 y V5, cuya proporción de puntos bien clasificados es 0.8102073. 