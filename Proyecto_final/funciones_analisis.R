
###An�lisis de decisiones para cada materia prima/ingrediente utilizado####

##El siguiente c�digo es una funci�n que permite la interraci�n del usuario con la computadora 
#interactue con la computadora para poder determinar el nombre del producto que se evaluara, y 
# sus caracteristicas 
###para determinar si es un punto cr�tico utiliz� un ciclo while para finalizar el an�lisis  
materia_prima <- function()
{ 
  seguir<- 1
  while (seguir == 1){
    definir<-readline("�Quieres evaluar una lista de materias primas o una sola?,
                      1= Evaluar un ingrediente, 2= evaluar una lista de ingredientes")
    definir<- as.numeric(definir)
    if (definir== 1) cat(mp1)
    if (definir==2) cat ("Escribe�funcion_lista�en el ordenador e ingresa tu lista de ingredientes")&break
    ###si elijo analizar una lista de ingredientes esta funcion se termina y me indica que debo usar 
    #"funcion_lista" y agregar una lista de ingredientes en forma de vector o lista 
    
    mp1 <- readline("Ingresa el nombre de tu materia prima") 
    #Esta funcion me permite 
    ####nombrar una cadena de caracteres interactiva llamda "mp1", la cual ser� determinada por 
    ##el usuario 
    message("A continuaci�n evaluaremos el peligro en HCCP de: ", mp1)
    message("Responde las siguientes preguntas con 1=s�, 0=No")
    ##con los siguientes comandos condicionales hago el analisis de riesgos para cada 
    #ingrediente seg�n lo establecido en los lineamientos HCCP y as� determinar si 
    #hay alg�n punto cr�tico en nuestros ingredientes 
    
    p1<- readline("�Puede contener peligro microbiologico a niveles riesgoso para el consumidor? ") 
    p1<-as.numeric(p1) 
    if (p1 == 0){ 
      cat( "No es un punto cr�tico")
    } else if (p1 == 1) { 
      cat (p2<-readline(prompt="Incluido el uso correcto por el consumidor, �Puede el procesado 
             garantizar la eliminaci�n o reducci�n del peligro hasta niveles aceptables o seguros?"))
      p2<-as.numeric(p2)
      if (p2 == 0) cat(mp1, "No es un punto cr�tico")
      if (p2==1) cat( mp1, "��ES UN PUNTO CR�TICO!!!") }
    seguir<- readline(prompt = "�Desea evaluar otro ingrediente?")
    seguir<- as.numeric(seguir)}}

###########materia prima analisis lista #####
#con la siguiente funcion puedo analizar una lista de ingredientes para poder reportar los 
#resultado obtenidos en una tabla 
funcion_lista<- function(materia){
  seguir<- 1
  while (seguir == 1){
    message("Selecciona el n�mero de tu elemento de �nteres:")
    suma=0 
    for(i in seq_along(materia)) {
      suma<- 1:i 
    }
    names(materia)<-suma
    ls<- print (materia)
    m<-readline("El an�lisis se har� para:")
    m<- as.numeric(m)
    for(i in m) { cat (materia[c(i)])
    }
    message("Responde las siguientes preguntas con 1=s�, 0=No")
    ##con los siguientes comandos condicionales hago el analisis de riesgos para cada 
    #ingrediente seg�n lo establecido en los lineamientos HCCP y as� determinar si 
    #hay alg�n punto cr�tico en nuestros ingredientes 
    p1<- readline("�Puede contener peligro microbiologico a niveles riesgoso para el consumidor? ") 
    p1<-as.numeric(p1) 
    
    if (p1 == 0){ 
      cat( "No es un punto cr�tico")
    } else if (p1 == 1) { 
      cat (p2<-readline(prompt="Incluido el uso correcto por el consumidor, �Puede el procesado 
             garantizar la eliminaci�n o reducci�n del peligro hasta niveles aceptables o seguros?"))
      p2<-as.numeric(p2)
      if (p2 == 0) cat("No es un punto cr�tico")
      if (p2==1) cat( "��ES UN PUNTO CR�TICO!!!") }
    seguir<- readline(prompt = "�Desea evaluar otro ingrediente?")
    seguir<- as.numeric(seguir)}}

###creo un data.frame que contenga una lista de las materias primas usadas en el proceso
mp<- read.csv("materia_prima.csv")
mp
matriz<-as.matrix(mp)
matriz
materia<- as.vector(matriz[,1])
materia ### mi lista ahora es un vector que puedo ingresar a mi funcion lista 

#####################funcion lista######
##En cada paso del bucle reemplazar la columna que corresponda:
CP<- lapply(materia, funcion_lista2)
PC<-c ("��ES UN PUNTO CR�TICO!!!", "No es un punto cr�tico", "No es un punto cr�tico", 
       "No es un punto cr�tico", "No es un punto cr�tico", "No es un punto cr�tico", 
       "No es un punto cr�tico", "No es un punto cr�tico") 
#este vector se llena manual a partir de los resultados ya que el objeto que resulta de la funcion es de clase NULL
materia <-  cbind(materia, PC)##A mi vector materia le a�ado el vector de resultados 
reporte<- as.table(materia)#Creo una tabla que se puede usar un un reporte 
reporte


#######An�lisis de peligros para cada etapa de fabricaci�n##########
#esta funci�n es similar a la "funcion lista" pero ahora analizaremos el mapa de flujo realizado 
###para nuestro proceso, en este caso el objeto al que se aplicar� la funci�n ser� una lista que contiene 
## las etapas del proceso 
etapas<-read.csv("proceso.csv")
etapas<-as.matrix(etapas)
etapas<-as.vector(etapas[,2])
etapas

fabricacion<- function(etapas)
  {
  seguir<- 1
  while (seguir == 1){
    message("Selecciona el n�mero de tu elemento de �nteres:")
    suma=0 
    for(i in seq_along(etapas)) {
      suma<- 1:i 
    }
    names(etapas)<-suma
    ls<- print (etapas)
    m<-readline("El an�lisis se har� para:")
    m<- as.numeric(m)
    for(i in m) { cat (etapas[c(i)])
    } 
    message("Responde las siguientes preguntas con 1=s�, 0=No")
  p1<- readline("�Puede permitir esta etapa la contaminaci�n con el agente del peligro considerado,
                 o permite que este aumente hasta niveles nocivos ? ")
  p1<-as.numeric(p1) 
  if (p1 == 0) { cat (p3<-readline("�Se pretende con esta etapa inhibir o prevenir la contaminaci�n y/o 
                 el aumento de peligro hasta niveles nocivos?"))
  p3<-as.numeric(p3)
  if(p3==0) cat("no es un punto cr�tico\n")
  if (p3==1) cat("�ES UN PUNTO CR�TICO!\n")
   } else if (p1 == 1) {cat (p2<- readline("Incluido el uso correcto por el consumidor, �Garantiza el proceso posterior,
                  la eliminaci�n del peligro o su reducci�n hasta niveles seguros?"))
  p2<- as.numeric(p2)
  if (p2==0) cat("no es un punto cr�tico\n")
  if (p2==1) cat("�ES UN PUNTO CR�TICO!\n")}
  seguir<- readline(prompt = "�Desea evaluar otro ingrediente?")
  seguir<- as.numeric(seguir)}
  }

fabricacion(etapas)## analizo cada una de mis etapas con la funcion interactiva 
PCF<- c("no es un punto cr�tico","�ES UN PUNTO CR�TICO!","�ES UN PUNTO CR�TICO!", 
       "�ES UN PUNTO CR�TICO!", "no es un punto cr�tico")# creo una lista con los resultados de esa funcion

##creo una tabla  con la lista del analisis y sus resultados 
resultados<- cbind(etapas,PCF)
resultados<- as.table(resultados)
resultados
#####################################################################################

##### ANALISIS PARA DETERMINAR LOS PUNTOS CR�TICOS DE CONTROL EN LA FABRICACION####
#Esta funci�n se hace de manera manual, ingresas el/los procesos de los cuales obtuviste como resultado
# de tu analisis de PC para determinar si esos PC son o no puntos cr�ticos de control y las medidas de 
#seguridad que se deben seguir
pcc<- function(){
  seguir<- 1
  while (seguir == 1){
  message("interactive: ", interactive()) 
  f1 <- readline("Ingresa el nombre del paso de fabricaci�n a evaluar: ") 
  message("A continuaci�n determinaremos los puntos cr�ticos de control a partir de los pc identificados")
  message("Responde las siguientes preguntas con 1=s�, 0=No")
  
  p1<- readline("�Existen medidas preventivas para el preligro identificado?")
  p1<- as.numeric(p1)
  if (p1==0) { cat (p5<-readline ("�Es necesario el control en este paso para seguridad?"))
   p5<-as.numeric (p5)
if (p5==0) cat (f1, "no es un punto cr�tico de control")
if (p5 == 1) cat("Investigar oportunidad de mejorar el paso/proceso/producto")
   } else if (p1==1){ cat (p2<- readline("En este paso �Est�(s) medida(s) preventivas eliminan o reducen la probabilidad 
              de ocurrencia de peligro a un nivel aceptable?"))
p2<-as.numeric(p2)
if (p2==1) cat (f1, "�ES UN PUNTO CR�TICO DE CONTROL!")
} else if (p2==0) { cat( p3<- readline("�Puede la p�rdida de control de este paso contribuir o causar posteriormente una falla
              o podr�an estos incrementar a niveles aceptables?"))
    p3<- as.numeric(p3)
    if (p3==0) cat (f1, "no es un punto cr�tico de control") 
} else if (p3==1) {cat (p4<-readline("�Podr�a un paso posterior eliminar el peligro identificado o reducir su ocurrencia 
              a un nivel aceptable?"))
p4<- as.numeric(p4)
if (p4== 1) cat (f1, "no es un punto cr�tico de control") 
if (p4 == 0) cat (f1, "�ES UN PUNTO CR�TICO DE CONTROL!")}
seguir<- readline(prompt = "�Desea evaluar otro punto cr�tico?")
seguir<- as.numeric(seguir)}}
########
if(interactive()) pcc()
#### Como es una funcion dise�ada para analizar de manera manual el punto 
#cre� dos vectores que contenian el punto a evaluar y el resultado del an�lisis 
paso<-c("Concentrado de tomate", " Mezclado ", "Pasteurizacion", "Concentracion")
  ptoCC<- c("no es un punto cr�tico de control", "no es un punto cr�tico de control",
            "�ES UN PUNTO CR�TICO DE CONTROL!", "�ES UN PUNTO CR�TICO DE CONTROL!")
  res<- cbind(paso, ptoCC)
  res<-as.table(res)
  res