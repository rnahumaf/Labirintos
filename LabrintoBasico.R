# Dimensões
largura <- 51 # Ímpar
altura <- 51 # Ímpar

# Matriz
m <- matrix(rep(0, largura*altura), nrow = altura)

# Início
origem_i <- altura-1
origem_j <- 2
atual_i <- origem_i
atual_j <- origem_j
m[atual_i, atual_j] <- 1

# Estratégias
Trocar_origem <- T # Quando encontra um dos vértices, muda a origem para aquele vértice

# Trash variables
Move <- c(0, 0)
Counts <- 0
DoCount = T
Exp <- 2
CountExp <- 0

# S T A R T

repeat{

  # Counts
  if(DoCount){
    # A cada 10^Exp rodadas, aumentar Exp = Exp + 1
    # Normalmente, a cada 10^Exp rodadas, a origem é resetada
    # Isso proporciona maior ramificação a partir da origem
    # Contudo, se 10^Exp for muito baixo, às vezes é impossível completar o labirinto
    CountExp <- CountExp + 1 
    if(CountExp >= 10^Exp){
      CountExp <- 0
      Exp <- Exp + 1
    }
    
    # A cada 10^Exp rodadas, resetar a origem
    Counts <- Counts + 1
    if(Counts >= 10^Exp){
      atual_i <- origem_i
      atual_j <- origem_j
      Counts <- 0
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
  
  # Checar se movimento é válido com base em (atual + move/2) e (atual + Move)
  # Em seguida, mover o pixel, junto com os valores de seu rastro
  # NOTA: Valor 2 será atribuído para as casas de posição ímpar
  #       Valor 1 será atribuído para as casas de posição par
  #       RAZÃO: isso facilitará a contabilização do preenchimento do labirinto,
  #               que deverá ser feita somando apenas as casas com valor = 1.
  # (0, 0) = válido: significa que não há caminhos adiante. Pode prosseguir.
  # (2, 1) = válido: significa que o caminho irá retroceder por rota pré-existente.
  # (0, 1) = inválido:  não é permitido atravessar paredes
  if((atual_i + Move[1]) %in% 1:altura && (atual_j + Move[2]) %in% 1:largura){
    if(m[(atual_i+Move[1]), (atual_j+Move[2])]==0){
      # (0, 0) = válido: significa que não há caminhos adiante. Pode prosseguir.
      m[(atual_i+Move[1]), (atual_j+Move[2])] <- 1
      m[(atual_i+Move[1]/2), (atual_j+Move[2]/2)] <- 2
      atual_i <- atual_i+Move[1]
      atual_j <- atual_j+Move[2]
      next
    } else {
      # (2, 1) = válido: significa que o caminho irá retroceder por rota pré-existente.
      if(m[(atual_i+Move[1]/2), (atual_j+Move[2]/2)]==2){
        atual_i <- atual_i+Move[1]
        atual_j <- atual_j+Move[2]
        next
      } else {
        # (0, 1) = inválido:  não é permitido atravessar paredes
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
  m3[(altura-1), (2)] <- 3 # Entrada
  m3[2, largura-1] <- 3 # Saída
  png("Impressao9.jpg", width = 1000, height = 1000)
  # 2. Create the plot
  par(mar=c(2,2,2,2)) # Borderless
  image(t(apply(m3, 2, rev)), asp = altura/largura, axes=FALSE, col = c("black", "white", "white", "red"))
  # 3. Close the file
  dev.off()
}

