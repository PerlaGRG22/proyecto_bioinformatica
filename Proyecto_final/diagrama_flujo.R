#####Diagrama de flujo####
#utilizo las siguientes librerias 
library(grid)
library(Gmisc)
############ tableBox() funcion ###########
###Funcion diseñada para crear cuadros  de texto que contendra nuestro diagraama 
#####
labels <- c("Proceso", "detalles") ## definimos las etiquetas  

tableBox <- function(labels, x=.5, y=.5) {
  nlabel <- length(labels) #la longitud de nuestro marco será por el tamaño de las etiquetas 
  tablevp <-
    viewport(x=x, y=y,
             width=max(stringWidth(labels)) +
               unit(2, "mm"),
             height=unit(nlabel, "lines"))# definimos el tamaño del cuadro 
  pushViewport(tablevp)
  grid.roundrect()
  if (nlabel > 1) {
    for (i in 1:(nlabel - 1)) { 
      # con este ciclo le digo a r que quiero un color diferente por cada etiqueta dentro de mi cuadro de tecto 
      fill <- c("mistyrose1", "plum")[i %% 2 + 1] #colores 
      grid.clip(y=unit(i, "lines"), just="bottom")
      grid.roundrect(gp=gpar(fill=fill))
    }
  }
  grid.clip()
  grid.text(labels,
            x=unit(2, "mm"), y=unit(nlabel:1 - .5, "lines"),
            just="left") ### con esto le decimos a r que texto aparecerá en el cuadro
  popViewport()
}
############## boxGrob () funcion###################
##Nos ayuda a definir nuestros objetos como clase "box" para que la funcion "grid.Draw"
#nos ayude a que podamos leerlos despues 
boxGrob <- function(labels, x=.5, y=1.5) { 
  grob(labels=labels, x=x, y=y, cl="box")
}
drawDetails.box <- function(x, ...) {
  tableBox(x$labels, x$x, x$y)
}

#######XDetails.box#########
xDetails.box <- function(x, theta) {
  nlines <- length(2)
  height <- unit(nlines, "lines")
  width <- unit(4, "mm") + max(stringWidth(2))
  grobX(roundrectGrob(x=x$x, y=x$y, width=width, height=height),
        theta)
}

######## YDetails.box ########
yDetails.box <- function(x, theta) {
  nlines <- length(2)
  height <- unit(nlines, "lines")
  width <- unit(4, "mm") + max(stringWidth(x$labels))
  grobY(rectGrob(x=x$x, y=x$y, width=width, height=height),
        theta)}
###
library(grid)
procesos<- read.csv("proceso.csv")
matriz<- as.matrix.data.frame(procesos)
# Determine el tamaño del marco rectangular y la etiqueta dentro del marco rectangular
box1 <- boxGrob(c("´´Procesamiento",
                           "industrial de", 
                  "la salsa catsup´´ "),x=0.5, y= 0.8)

box2 <- boxGrob(c(matriz [1,2],matriz[1,3]), x=0.5, y=0.7)

box3 <- boxGrob(c(matriz[2,2],matriz[2,3]), x=0.5, y=0.6)

box4 <- boxGrob(c(matriz[3,2],matriz[3,3]), x=0.5,y=0.5)

box5 <- boxGrob(c(matriz[4,2],matriz[4,3]), x=0.5,y=0.4)

box6 <- boxGrob(c(matriz[5,2],matriz[5,3]), x=0.5,y=0.3)
### Dibuja líneas de conexión entre diferentes cajas rectangulares
####union caja 1-2

grid.curve(xDetalis.box(box1, "south"),
           
           yDetalis.box(box1, "north"), 
           
           xDetalis.box(box2, "north"),
           
           yDetalis.box(box2, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
###unión caja 2-3
grid.curve(xDetalis.box(box2, "south"),
           
           yDetalis.box(box2, "north"), 
           
           xDetalis.box(box3, "north"),
           
           yDetalis.box(box3, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
###unión caja 3-4 
grid.curve(xDetalis.box(box3, "south"),
           
           yDetalis.box(box3, "north"), 
           
           xDetalis.box(box4, "north"),
           
           yDetalis.box(box4, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
### union caja 4-5
grid.curve(xDetalis.box(box4, "south"),
           
           yDetalis.box(box4, "north"), 
           
           xDetalis.box(box5, "north"),
           
           yDetalis.box(box5, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
##union caja 5-6 
grid.curve(xDetalis.box(box5, "south"),
           
           yDetalis.box(box5, "north"), 
           
           xDetalis.box(box6, "north"),
           
           yDetalis.box(box6, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
#Dibujo de marcos rectangulares y etiquetas
grid.draw(box1)
grid.draw(box2)
grid.draw(box3)
grid.draw(box4) 
grid.draw(box5)
grid.draw(box6)

####Diagrama de flujo con los PCC detectados####
#Luego de hacer el análisis con la funcion "pcc", manualmente le añadí a la matriz una columna que contiene 
#si la etapa es un pcc 

library(grid)
procesos<- read.csv("proceso.csv")
matriz<- as.matrix.data.frame(procesos)
pcce<-c("no es un pcc", "no es un pcc","¡ES UN PUNTO CRÍTICO DE CONTROL!",
        "¡ES UN PUNTO CRÍTICO DE CONTROL!","no es un pcc")
matriz<- cbind(matriz, pcce)
matriz
# Determine el tamaño del marco rectangular y la etiqueta dentro del marco rectangular
box1 <- boxGrob(c("´´Procesamiento",
                  "industrial de", 
                  "la salsa catsup´´ "),x=0.5, y= 0.8)

box2 <- boxGrob(c(matriz [1,2],matriz[1,3], matriz[1,4] ), x=0.5, y=0.7)

box3 <- boxGrob(c(matriz[2,2],matriz[2,3],matriz[2,4] ), x=0.5, y=0.6)

box4 <- boxGrob(c(matriz[3,2],matriz[3,3], matriz[3,4]), x=0.5,y=0.5)

box5 <- boxGrob(c(matriz[4,2],matriz[4,3], matriz[4,4]), x=0.5,y=0.4)

box6 <- boxGrob(c(matriz[5,2],matriz[5,3], matriz[5,4]), x=0.5,y=0.3)
### Dibuja líneas de conexión entre diferentes cajas rectangulares
####union caja 1-2

grid.curve(xDetalis.box(box1, "south"),
           
           yDetalis.box(box1, "north"), 
           
           xDetalis.box(box2, "north"),
           
           yDetalis.box(box2, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
###unión caja 2-3
grid.curve(xDetalis.box(box2, "south"),
           
           yDetalis.box(box2, "north"), 
           
           xDetalis.box(box3, "north"),
           
           yDetalis.box(box3, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
###unión caja 3-4 
grid.curve(xDetalis.box(box3, "south"),
           
           yDetalis.box(box3, "north"), 
           
           xDetalis.box(box4, "north"),
           
           yDetalis.box(box4, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
### union caja 4-5
grid.curve(xDetalis.box(box4, "south"),
           
           yDetalis.box(box4, "north"), 
           
           xDetalis.box(box5, "north"),
           
           yDetalis.box(box5, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
##union caja 5-6 
grid.curve(xDetalis.box(box5, "south"),
           
           yDetalis.box(box5, "north"), 
           
           xDetalis.box(box6, "north"),
           
           yDetalis.box(box6, "south"),
           
           arrow=arrow(type="closed", angle=8, length=unit(0.1, "mm")), gp=gpar(fill="black"))
#Dibujo de marcos rectangulares y etiquetas
grid.draw(box1)
grid.draw(box2)
grid.draw(box3)
grid.draw(box4) 
grid.draw(box5)
grid.draw(box6)

