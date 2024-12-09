#install.packages('readxl')
library(readxl)
#file.choose()

#funcion para calcular el costo dada una asignacion de maquinas a lugares
CostoAsig<- function(NMaq,a,MatDis,MatVia) 
{
  # Codes fragments
  costo=0
  for(i in 1:NMaq)
  { 
    for (j in 1:NMaq) # 
    {
      costo=costo+MatVia[i,j]*MatDis[a[i],a[j]]
      
    } # 
  }
  
  CostoAsig=costo/2
  #print(CostoAsig)
}

#funcion de intercambio de parejas de maquinas
Intercambio=function(a,u,v)
{
  
  (ai=a)
  # print(u)
  #print(v)
  ai[u]=a[v]
  ai[v]=a[u]
  #print (ai)
  
  Intercambio=ai}
# Datos de entrada

NumMaq=5 # Numero de maquinas y lugares

# Lectura de datos
excelM <- "C:\\Users\\amp14\\OneDrive\\Escritorio\\Septimo Semestre\\MAQUINAS.xlsx"
MatrizViajes <- as.matrix(read_excel(excelM, sheet = 'VIAJES', range = "A1:E6"))
MatrizDistancias <- as.matrix(read_excel(excelM, sheet = 'MTS', range = "A1:E6"))

# Validación de datos
cat("Dimensiones MatVia:", dim(MatVia), "\n")
cat("Dimensiones MatDis:", dim(MatDis), "\n")
print(MatVia)
print(MatDis)

a0=c(3,4,2,1,5) 
# Termina datos de entrada


# calculos de los incrementos de mejora con respecto a la asignacion inicial


maximo=1E10
while(maximo>0){
  costoBase=CostoAsig(NumMaq,a0,MatrizDistancias,MatrizViajes )
  
  (a=a0)
  renglones=choose(NumMaq,2)
  columnas=3
  tabla=matrix(nrow = renglones, ncol = columnas)
  m=0
  for(u in 1:(NumMaq-1))
    
  {
    for(v in (u+1):NumMaq)
      
    {
      m=m+1
      ai=Intercambio(a,u,v)
      DC=costoBase-CostoAsig(NumMaq,ai,MatrizDistancias,MatrizViajes)
      #print("diferencia de costos")
      #print(DC)
      vector=c(u,v,DC)
      #print(vector)
      
      tabla[m,1:3]=vector
    }} # fin de comparaciones
  
  #terminacion de la tabla
  print (tabla)
  maximo=max(tabla[,3])
  mejorcambio=(which(tabla[,3]==maximo))
  if(maximo<0)
    break
  print (mejorcambio)
  tabla[mejorcambio,]
  nuevoarreglo=Intercambio(a0,tabla[mejorcambio,1],tabla[mejorcambio,2])
  a0=nuevoarreglo
  print("mejor arreglo")
  print(a0)
  print("mejor costo")
  nuevoCostoBase=CostoAsig(NumMaq,nuevoarreglo,MatrizDistancias,MatrizViajes)
  print(nuevoCostoBase)
  #MejorOpcion=salida[nuevoarreglo,nuevoCostoBase]
  #} # termina funcion
}

