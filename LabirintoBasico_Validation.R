# Stress test (Use "m" resultante do script "LabirintoBasico")

# loops
loops <- 10^5

# Início
atual_i <- altura - 1
atual_j <- 2

# Trash variables
Step_count <- 0
m2 <- m

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
  # (atual, atual+move, atual+[move/2])
  # (1, 0, 1) NOT -> repeat
  if((atual_i + Move[1]) %in% 1:altura && (atual_j + Move[2]) %in% 1:largura){
    # (1, 0, 1) REPEAT
    if(m2[(atual_i+Move[1]/2), (atual_j+Move[2]/2)]==0 && m[(atual_i+Move[1]), (atual_j+Move[2])]>=1){
      next
    } else {
      m2[(atual_i+Move[1]), (atual_j+Move[2])] <- 1 + Step_count
      m2[(atual_i+Move[1]/2), (atual_j+Move[2]/2)] <- 1 + Step_count
      atual_i <- atual_i+Move[1]
      atual_j <- atual_j+Move[2]
      Step_count <- Step_count + 1
      next
    }
  } else {
    next
  }
}

colfunc <- colorRampPalette(c(rgb(1,1,1,1),rgb(1,0,1,1), rgb(0,0,0,1)), alpha=TRUE, bias = 0.5)
image(t(apply(m2, 2, rev)), asp = 1, axes=FALSE, col = colfunc(256))
