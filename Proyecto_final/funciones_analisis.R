
###Análisis de decisiones para cada materia prima/ingrediente utilizado####

##El siguiente código es una función que permite la interración del usuario con la computadora 
#interactue con la computadora para poder determinar el nombre del producto que se evaluara, y 
# sus caracteristicas 
###para determinar si es un punto crítico utilizó un ciclo while para finalizar el análisis  
materia_prima <- function()
{ 
  seguir<- 1
  while (seguir == 1){
    definir<-readline("¿Quieres evaluar una lista de materias primas o una sola?,
                      1= Evaluar un ingrediente, 2= evaluar una lista de ingredientes")
    definir<- as.numeric(definir)
    if (definir== 1) cat(mp1)
    if (definir==2) cat ("Escribe´funcion_lista´en el ordenador e ingresa tu lista de ingredientes")&break
    ###si elijo analizar una lista de ingredientes esta funcion se termina y me indica que debo usar 
    #"funcion_lista" y agregar una lista de ingredientes en forma de vector o lista 
    
    mp1 <- readline("Ingresa el nombre de tu materia prima") 
    #Esta funcion me permite 
    ####nombrar una cadena de caracteres interactiva llamda "mp1", la cual será determinada por 
    ##el usuario 
    message("A continuación evaluaremos el peligro en HCCP de: ", mp1)
    message("Responde las siguientes preguntas con 1=sí, 0=No")
    ##con los siguientes comandos condicionales hago el analisis de riesgos para cada 
    #ingrediente según lo establecido en los lineamientos HCCP y así determinar si 
    #hay algún punto crítico en nuestros ingredientes 
    
    p1<- readline("¿Puede contener peligro microbiologico a niveles riesgoso para el consumidor? ") 
    p1<-as.numeric(p1) 
    if (p1 == 0){ 
      cat( "No es un punto crítico")
    } else if (p1 == 1) { 
      cat (p2<-readline(prompt="Incluido el uso correcto por el consumidor, ¿Puede el procesado 
             garantizar la eliminación o reducción del peligro hasta niveles aceptables o seguros?"))
      p2<-as.numeric(p2)
      if (p2 == 0) cat(mp1, "No es un punto crítico")
      if (p2==1) cat( mp1, "¡¡ES UN PUNTO CRÍTICO!!!") }
    seguir<- readline(prompt = "¿Desea evaluar otro ingrediente?")
    seguir<- as.numeric(seguir)}}

###########materia prima analisis lista #####
#con la siguiente funcion puedo analizar una lista de ingredientes para poder reportar los 
#resultado obtenidos en una tabla 
funcion_lista<- function(materia){
  seguir<- 1
  while (seguir == 1){
    message("Selecciona el número de tu elemento de ínteres:")
    suma=0 
    for(i in seq_along(materia)) {
      suma<- 1:i 
    }
    names(materia)<-suma
    ls<- print (materia)
    m<-readline("El análisis se hará para:")
    m<- as.numeric(m)
    for(i in m) { cat (materia[c(i)])
    }
    message("Responde las siguientes preguntas con 1=sí, 0=No")
    ##con los siguientes comandos condicionales hago el analisis de riesgos para cada 
    #ingrediente según lo establecido en los lineamientos HCCP y así determinar si 
    #hay algún punto crítico en nuestros ingredientes 
    p1<- readline("¿Puede contener peligro microbiologico a niveles riesgoso para el consumidor? ") 
    p1<-as.numeric(p1) 
    
    if (p1 == 0){ 
      cat( "No es un punto crítico")
    } else if (p1 == 1) { 
      cat (p2<-readline(prompt="Incluido el uso correcto por el consumidor, ¿Puede el procesado 
             garantizar la eliminación o reducción del peligro hasta niveles aceptables o seguros?"))
      p2<-as.numeric(p2)
      if (p2 == 0) cat("No es un punto crítico")
      if (p2==1) cat( "¡¡ES UN PUNTO CRÍTICO!!!") }
    seguir<- readline(prompt = "¿Desea evaluar otro ingrediente?")
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
PC<-c ("¡¡ES UN PUNTO CRÍTICO!!!", "No es un punto crítico", "No es un punto crítico", 
       "No es un punto crítico", "No es un punto crítico", "No es un punto crítico", 
       "No es un punto crítico", "No es un punto crítico") 
#este vector se llena manual a partir de los resultados ya que el objeto que resulta de la funcion es de clase NULL
materia <-  cbind(materia, PC)##A mi vector materia le añado el vector de resultados 
reporte<- as.table(materia)#Creo una tabla que se puede usar un un reporte 
reporte


#######Análisis de peligros para cada etapa de fabricación##########
#esta función es similar a la "funcion lista" pero ahora analizaremos el mapa de flujo realizado 
###para nuestro proceso, en este caso el objeto al que se aplicará la función será una lista que contiene 
## las etapas del proceso 
etapas<-read.csv("proceso.csv")
etapas<-as.matrix(etapas)
etapas<-as.vector(etapas[,2])
etapas

fabricacion<- function(etapas)
  {
  seguir<- 1
  while (seguir == 1){
    message("Selecciona el número de tu elemento de ínteres:")
    suma=0 
    for(i in seq_along(etapas)) {
      suma<- 1:i 
    }
    names(etapas)<-suma
    ls<- print (etapas)
    m<-readline("El análisis se hará para:")
    m<- as.numeric(m)
    for(i in m) { cat (etapas[c(i)])
    } 
    message("Responde las siguientes preguntas con 1=sí, 0=No")
  p1<- readline("¿Puede permitir esta etapa la contaminación con el agente del peligro considerado,
                 o permite que este aumente hasta niveles nocivos ? ")
  p1<-as.numeric(p1) 
  if (p1 == 0) { cat (p3<-readline("¿Se pretende con esta etapa inhibir o prevenir la contaminación y/o 
                 el aumento de peligro hasta niveles nocivos?"))
  p3<-as.numeric(p3)
  if(p3==0) cat("no es un punto crítico\n")
  if (p3==1) cat("¡ES UN PUNTO CRÍTICO!\n")
   } else if (p1 == 1) {cat (p2<- readline("Incluido el uso correcto por el consumidor, ¿Garantiza el proceso posterior,
                  la eliminación del peligro o su reducción hasta niveles seguros?"))
  p2<- as.numeric(p2)
  if (p2==0) cat("no es un punto crítico\n")
  if (p2==1) cat("¡ES UN PUNTO CRÍTICO!\n")}
  seguir<- readline(prompt = "¿Desea evaluar otro ingrediente?")
  seguir<- as.numeric(seguir)}
  }

fabricacion(etapas)## analizo cada una de mis etapas con la funcion interactiva 
PCF<- c("no es un punto crítico","¡ES UN PUNTO CRÍTICO!","¡ES UN PUNTO CRÍTICO!", 
       "¡ES UN PUNTO CRÍTICO!", "no es un punto crítico")# creo una lista con los resultados de esa funcion

##creo una tabla  con la lista del analisis y sus resultados 
resultados<- cbind(etapas,PCF)
resultados<- as.table(resultados)
resultados
#####################################################################################

##### ANALISIS PARA DETERMINAR LOS PUNTOS CRÍTICOS DE CONTROL EN LA FABRICACION####
#Esta función se hace de manera manual, ingresas el/los procesos de los cuales obtuviste como resultado
# de tu analisis de PC para determinar si esos PC son o no puntos críticos de control y las medidas de 
#seguridad que se deben seguir
pcc<- function(){
  seguir<- 1
  while (seguir == 1){
  message("interactive: ", interactive()) 
  f1 <- readline("Ingresa el nombre del paso de fabricación a evaluar: ") 
  message("A continuación determinaremos los puntos críticos de control a partir de los pc identificados")
  message("Responde las siguientes preguntas con 1=sí, 0=No")
  
  p1<- readline("¿Existen medidas preventivas para el preligro identificado?")
  p1<- as.numeric(p1)
  if (p1==0) { cat (p5<-readline ("¿Es necesario el control en este paso para seguridad?"))
   p5<-as.numeric (p5)
if (p5==0) cat (f1, "no es un punto crítico de control")
if (p5 == 1) cat("Investigar oportunidad de mejorar el paso/proceso/producto")
   } else if (p1==1){ cat (p2<- readline("En este paso ¿Está(s) medida(s) preventivas eliminan o reducen la probabilidad 
              de ocurrencia de peligro a un nivel aceptable?"))
p2<-as.numeric(p2)
if (p2==1) cat (f1, "¡ES UN PUNTO CRÍTICO DE CONTROL!")
} else if (p2==0) { cat( p3<- readline("¿Puede la pérdida de control de este paso contribuir o causar posteriormente una falla
              o podrían estos incrementar a niveles aceptables?"))
    p3<- as.numeric(p3)
    if (p3==0) cat (f1, "no es un punto crítico de control") 
} else if (p3==1) {cat (p4<-readline("¿Podría un paso posterior eliminar el peligro identificado o reducir su ocurrencia 
              a un nivel aceptable?"))
p4<- as.numeric(p4)
if (p4== 1) cat (f1, "no es un punto crítico de control") 
if (p4 == 0) cat (f1, "¡ES UN PUNTO CRÍTICO DE CONTROL!")}
seguir<- readline(prompt = "¿Desea evaluar otro punto crítico?")
seguir<- as.numeric(seguir)}}
########
if(interactive()) pcc()
#### Como es una funcion diseñada para analizar de manera manual el punto 
#creé dos vectores que contenian el punto a evaluar y el resultado del análisis 
paso<-c("Concentrado de tomate", " Mezclado ", "Pasteurizacion", "Concentracion")
  ptoCC<- c("no es un punto crítico de control", "no es un punto crítico de control",
            "¡ES UN PUNTO CRÍTICO DE CONTROL!", "¡ES UN PUNTO CRÍTICO DE CONTROL!")
  res<- cbind(paso, ptoCC)
  res<-as.table(res)
  res