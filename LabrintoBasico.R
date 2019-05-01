# Dimensões
largura <- 51 # Ímpar
altura <- 51 # Ímpar

# Matriz
m <- matrix(rep(0, largura*altura), nrow = altura)

# Início
atual_i <- altura - 1
atual_j <- 2
m[atual_i, atual_j] <- 1

# Trash variables
Move <- c(0, 0)
# S T A R T

repeat{

  # Checar se o quadro já foi concluído
  if(sum(m==1)==((largura-1)/2)*((altura-1)/2)){
    break
  }
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
  #   com base nas próximas casas (atual + Move/2) e (atual + Move)
  # Dois espaços em branco (0, 0) = válido (avança caminho novo)
  # Dois espaços tomados (>=1, >=1) = válido (retorna em caminho pré-existente)
  # Um espaço em branco e um espaço tomado (0, >=1) = inválido (caminho não deve atravessar paredes)
  if((atual_i + Move[1]) %in% 1:altura && (atual_j + Move[2]) %in% 1:largura){
    if(m[(atual_i+Move[1]), (atual_j+Move[2])]==0){
      # (0, 0) = válido (avança)
      m[(atual_i+Move[1]), (atual_j+Move[2])] <- 1
      m[(atual_i+Move[1]/2), (atual_j+Move[2]/2)] <- 2
      atual_i <- atual_i+Move[1]
      atual_j <- atual_j+Move[2]
      next
    } else {
      # (>=1, >=1) = válido (retorna em caminho pré-existente)
      if(m[(atual_i+Move[1]/2), (atual_j+Move[2]/2)]==2){
        atual_i <- atual_i+Move[1]
        atual_j <- atual_j+Move[2]
        next
      } else {
        # (0, >=1) = inválido (caminho não deve atravessar paredes)
        next
      }
    }
  } else {
    next
  }
}

image(t(apply(m, 2, rev)), asp = 1, axes=FALSE, col = c("black", "white", "white"))


