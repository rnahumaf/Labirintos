# Dimensões
largura <- 51 # Ímpar
altura <- 51 # Ímpar

# Matriz
m <- matrix(rep(0, largura*altura), nrow = altura)

# Início
atual_i <- altura - 1
atual_j <- 2
m[atual_i, atual_j] <- 1

# Estratégias
Trocar_origem <- T # Quando encontra uma quina, muda o centro das ramificações para aquela quina
origem_i <- altura-1
origem_j <- 2

# Trash variables
Move <- c(0, 0)
Counts <- 0
DoCount = T
Exp <- 3
CountExp <- 0

# S T A R T

repeat{

  # Counts
  if(DoCount){
    Counts <- Counts + 1
    if(Counts >= 10^Exp){
      atual_i <- origem_i
      atual_j <- origem_j
      Counts <- 0
    }
    CountExp <- CountExp + 1
    if(CountExp >= 10){
      CountExp <- 0
      Exp <- Exp + 1
    }
  }
  
  if(Trocar_origem){
    # Trocar origem, quando chegar na quina oposta
    if(atual_i == 2 && atual_j == largura-1){
      origem_i <- atual_i
      origem_j <- atual_j
    }
  }
  
  # Checar se o quadro já foi concluído
  if(sum(m==1)==((largura-1)/2)*((altura-1)/2)){
    break
  }
  # Movimento
  cimabaixo <- sample(c(-2,2), 1, prob = c(1,1))
  esquerdir <- sample(c(-2,2), 1, prob = c(1,1))
  
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
  # (1, 0, 0) OK -> (1, 2, 1)
  # (1, 2, 1) OK -> (1, 2, 1) (opcional: Repeat -> Usado para limiar de Counts curtos)
  # (1, 0, 1) NOT -> repeat
  if((atual_i + Move[1]) %in% 1:altura && (atual_j + Move[2]) %in% 1:largura){
    if(m[(atual_i+Move[1]), (atual_j+Move[2])]==0){
      # (1, 0, 0) OK -> (1, 1, 1)
      m[(atual_i+Move[1]), (atual_j+Move[2])] <- 1
      m[(atual_i+Move[1]/2), (atual_j+Move[2]/2)] <- 2
      atual_i <- atual_i+Move[1]
      atual_j <- atual_j+Move[2]
      next
    } else {
      # (1, 2, 1) OK -> (1, 2, 1) (opcional: Repeat -> Usado para limiar de Counts curtos)
      if(m[(atual_i+Move[1]/2), (atual_j+Move[2]/2)]==2){
        atual_i <- atual_i+Move[1]
        atual_j <- atual_j+Move[2]
        next
      } else {
        # (1, 0, 1) REPEAT
        next
      }
    }
  } else {
    next
  }
}

image(t(apply(m, 2, rev)), asp = altura/largura, axes=FALSE, col = c("black", "white", "white"))


# Print output
if(F){
  m3 <- m
  m3[(2), (2)] <- 3 # Entrada
  m3[largura-1, largura-1] <- 3 # Saída
  png("Impressao6.jpg", width = 1000, height = 1000)
  # 2. Create the plot
  par(mar=c(2,2,2,2)) # Borderless
  image(t(apply(m3, 2, rev)), asp = altura/largura, axes=FALSE, col = c("black", "white", "white", "red"))
  # 3. Close the file
  dev.off()
}

