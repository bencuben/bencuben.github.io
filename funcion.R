#funcion auxiliar
#Brahian Cano Urrego
#Universidad Nacional de Colombia
#Modulo[R]
#fecha:10/04/2016
#--------------------------------------------------------------------------------------------

#coje cada posible forma de ganar y verifica si la suma en la matriz es 3 o 0 
#lo que indica que alguien gano
gano=function(x){
ganar=FALSE
  
  suma3=sum(m[1,3],m[2,2],m[3,1])
  suma2=sum(m[1,1],m[2,2],m[3,3])
  if(suma3==0 | suma3==3){
    ganar=TRUE
  }
  if(suma3==3){
    ganar=TRUE
  }
  if(suma2==0){
    ganar=TRUE
   
  }
  if(suma2==3){
    ganar=TRUE
  }
      
    if(sum(m[1,1],m[1,2],m[1,3])==0){
      ganar=TRUE
    }
    if(sum(m[1,1],m[1,2],m[1,3])==3){
      ganar=TRUE
    }
    if(sum(m[2,1],m[2,2],m[2,3])==0){
      ganar=TRUE
    }
    if(sum(m[2,1],m[2,2],m[2,3])==3){
      ganar=TRUE
    }
    if(sum(m[3,1],m[3,2],m[3,3])==0){
      ganar=TRUE
      
    }
    if(sum(m[3,1],m[3,2],m[3,3])==3){
      ganar=TRUE
    }
    
  
  
  
  
  if(sum(m[1,1],m[2,1],m[3,1])==0){
    ganar=TRUE
  }
  if(sum(m[1,1],m[2,1],m[3,1])==3){
    ganar=TRUE
  }
  if(sum(m[1,2],m[2,2],m[3,2])==0){
    ganar=TRUE
  }
  if(sum(m[1,2],m[2,2],m[3,2])==3){
    ganar=TRUE
  }
  if(sum(m[1,3],m[2,3],m[3,3])==0){
    ganar=TRUE
    
  }
  if(sum(m[1,3],m[2,3],m[3,3])==3){
    ganar=TRUE
  }
  
  
return(ganar)
}

  
    
  



