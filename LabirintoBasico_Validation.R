# Stress test

# loops
loops <- 10^5.5

# Início
atual_i <- origem_i # Empresta do script anterior
atual_j <- origem_j # Empresta do script anterior

# Definir estratégias
Estrategia <- function(E){
  if(E == "A"){
    Estrategia_A <<- T # aumenta o valor do caminho sobre o qual passa em 1
    Estrategia_B <<- F
    Estrategia_C <<- F
  }
  if(E == "B"){
    Estrategia_A <<- F
    Estrategia_B <<- T # cada passo tem um valor crescente em 1
    Estrategia_C <<- F
  }
}
Estrategia("B")

# Trash variables
m2 <- m
Counts <- 0
Step_count <- 3

# S T A R T

for(i in 1:loops){

  # Movimento
  cimabaixo <- sample(c(-2,2), 1)
  esquerdir <- sample(c(-2,2), 1)
  
  # Escolher entre eixos para decidir o movimento
  horizvert <- sample(c("horiz", "vert"), 1)
  if(horizvert == "horiz"){
    Move[1] <- 0
    Move[2] <- esquerdir
  } else {
    Move[1] <- cimabaixo
    Move[2] <- 0
  }
  
  # Checar se movimento é válido
  if((atual_i + Move[1]) %in% 1:altura && (atual_j + Move[2]) %in% 1:largura){
    
    # (0, >=1) = inválido (caminho não deve atravessar paredes)
    if(m2[(atual_i+Move[1]/2), (atual_j+Move[2]/2)]==0 && m[(atual_i+Move[1]), (atual_j+Move[2])]>=1){
      next
    } else {
      
      # Estratégias
      if(Estrategia_A){ # aumenta o valor do caminho sobre o qual passa em 1
        m2[(atual_i+Move[1]), (atual_j+Move[2])] <- 1 + m2[(atual_i+Move[1]), (atual_j+Move[2])]
        m2[(atual_i+Move[1]/2), (atual_j+Move[2]/2)] <- 1 + m2[(atual_i+Move[1]), (atual_j+Move[2])]
        atual_i <- atual_i+Move[1]
        atual_j <- atual_j+Move[2]
        next
      }
      if(Estrategia_B){ # cada passo tem um valor crescente em 1
        m2[(atual_i+Move[1]), (atual_j+Move[2])] <- 1 + Step_count
        m2[(atual_i+Move[1]/2), (atual_j+Move[2]/2)] <- 1 + Step_count
        atual_i <- atual_i+Move[1]
        atual_j <- atual_j+Move[2]
        Step_count <- Step_count + 1
        next
      }
    }
  }
} 


colfunc <- colorRampPalette(c(rgb(1,1,1,1),
                              rgb(1,0,1,1),
                              rgb(1,1,0,1),
                              rgb(0,1,1,1),
                              rgb(0,0,1,1), 
                              rgb(0,1,0,1),
                              rgb(1,0,0,1)), alpha=TRUE, bias = 1)
image(t(apply(m2, 2, rev)), asp = altura/largura, axes=FALSE, col = colfunc(1000));
