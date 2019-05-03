# NUMERO DE PLOTS PRODUZIDOS
n_plots <- 20

for(k in 1:n_plots){


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
  # NOTA: O valor "2" será atribuído para as casas de posição ímpar
  #       O valor "1" será atribuído para as casas de posição par
  #       RAZÃO: isso facilitará a contabilização do preenchimento do labirinto,
  #               que deverá ser feita somando apenas as casas com valor = 1.
  # (0, 0) = válido: significa que não há caminhos adiante. Pode prosseguir.
  # (2, 1) = válido: significa que o caminho irá retroceder por rota pré-existente.
  # (0, 1) = inválido:  não é permitido atravessar paredes
  if((atual_i + Move[1]) %in% 1:altura && (atual_j + Move[2]) %in% 1:largura){ # O movimento ocorrerá nos limites da matriz
    if(m[(atual_i+Move[1]), (atual_j+Move[2])]==0){
      # (0, 0) = válido: significa que não há caminhos adiante. Pode prosseguir.
      m[(atual_i+Move[1]), (atual_j+Move[2])] <- 1
      m[(atual_i+Move[1]/2), (atual_j+Move[2]/2)] <- 2
      atual_i <- atual_i+Move[1]
      atual_j <- atual_j+Move[2]
      next
    } else {
      # (2, 1) = válido: significa que o caminho irá retroceder ou avançar (e.g. a partir da origem) por rota pré-existente
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

# image(t(apply(m, 2, rev)), asp = altura/largura, axes=FALSE, col = c("black", "white", "white"))



# D E T E R M I N E   D I F F I C U L T Y   L E V E L



# Início
QIE <- c(altura-1, 2)
QSD <- c(2, largura-1)
QSE <- c(2, 2)
QID <- c(altura-1, largura-1)

# Determinar eixos principais de ( origem e oposto )
eixos <- function(E){
  if(E == "crescente"){
    origem_i <<- QIE[1] 
    origem_j <<- QIE[2]
    oposto_i <<- QSD[1]
    oposto_j <<- QSD[2]
    swap_origem_i <<- QSD[1] 
    swap_origem_j <<- QSD[2]
    swap_oposto_i <<- QIE[1]
    swap_oposto_j <<- QIE[2]
    atual_i <<- origem_i
    atual_j <<- origem_j
  }
  if(E == "decrescente"){
    origem_i <<- QSE[1] 
    origem_j <<- QSE[2]
    oposto_i <<- QID[1]
    oposto_j <<- QID[2]
    swap_origem_i <<- QID[1] 
    swap_origem_j <<- QID[2]
    swap_oposto_i <<- QSE[1]
    swap_oposto_j <<- QSE[2]
    atual_i <<- origem_i
    atual_j <<- origem_j
  }
}

eixos("decrescente")

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
Passos <- 0
Registro_passos <- 0
Chegadas <- 0
encruzilhadas <- 0

# S T A R T

repeat{
  
  # Conferir se já chegou ao lado oposto
  if(atual_i == oposto_i && 
     atual_j == oposto_j){
    
    print(Chegadas)
    
    # Após chegar ao lado oposto, voltar ao início e repetir o processo (e registrar o número de passos obtido)
    Chegadas <- Chegadas + 1
    atual_i <- origem_i
    atual_j <- origem_j
    Registro_passos[length(Registro_passos)+1] <- Passos
    Passos <- 0
    
    # Após ocorrer 5 chegadas, mudar de lado
    if(Chegadas == 5){
      origem_i <- swap_origem_i
      origem_j <- swap_origem_j
      oposto_i <- swap_oposto_i
      oposto_j <- swap_oposto_j
      atual_i <- origem_i
      atual_j <- origem_j
    }
    
    # Após ocorrer 5 chegadas do outro lado, terminar o programa
    if(Chegadas == 10){
      break
    }
  }
  
  # Identificar se chegou num beco sem saída
  if(sum(m[(atual_i - 1):(atual_i + 1), (atual_j - 1):(atual_j + 1)]==0) == 7){
    notifica <- T
  }
  
  # Contabiliza os passos
  Passos <- Passos + 1
  
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
        if(notifica){
        # Como um beco foi atingido recentemente, vou selar o caminho de volta com zeros (em posição de "parede"), para evitar retorno ao mesmo beco
          atual_i <- atual_i+Move[1]
          atual_j <- atual_j+Move[2]
          
          # Se o movimento termina em encruzilhada, considero que saímos do beco, então desligo a notificação
          # Adicionalmente, adiciono a presença da encruzilhada numa contagem
          if(sum(m[(atual_i - 1):(atual_i + 1), (atual_j - 1):(atual_j + 1)]==0) <= 5){
            notifica <- F
            encruzilhadas <- encruzilhadas + 1
          }
          
          # Sela definitivamente a entrada para o caminho do beco
          m2[(atual_i-Move[1]/2), (atual_j-Move[2]/2)] <- 0
 
          next
        } else {
        # 
          atual_i <- atual_i+Move[1]
          atual_j <- atual_j+Move[2]
          next
        }
      }
    }
  }
}

Dificuldade <- sum(Registro_passos)/(length(Registro_passos)-1)
Dificuldade <- round(Dificuldade/1000, 2)
encruzilhadas <- encruzilhadas/(length(Registro_passos)-1) # número de encruzilhadas por caminho
Dificuldade <- Dificuldade*encruzilhadas/1000

print(paste0("Número de passos médio: ", Dificuldade))

# Print output
  m3 <- m
  m3[oposto_i-1, (1)] <- 3 # Entrada
  m3[oposto_i+1, (1)] <- 3 # Entrada
  m3[oposto_i, (1)] <- 1 # Entrada
  m3[origem_i-1, largura] <- 3 # Saída
  m3[origem_i+1, largura] <- 3 # Saída
  m3[origem_i, largura] <- 1 # Saída
  # 2. Create the plot
  png(paste0("Dif_", Dificuldade, ".jpg"), width = 1000, height = 1000)
  par(mar=c(2,2,2,2)) # Borderless
  image(t(apply(m3, 2, rev)), asp = altura/largura, axes=FALSE, col = c("black", "white", "white", "red"))
  mtext(bquote(paste(bold("Dificuldade: "), .(Dificuldade))), side = 3, line = 0.5, cex = 2)
  # 3. Close the file
  dev.off()

}
