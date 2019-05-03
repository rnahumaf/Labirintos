# NUMERO DE PLOTS PRODUZIDOS
n_plots <- 1

for(k in 1:n_plots){
  
  
  # Dimensões
  largura <- 51 # Ímpar
  altura <- 51 # Ímpar
  
  # Matriz
  m <- matrix(rep(0, largura*altura), nrow = altura)
  
  # As bordas do labirinto possuem número negativo (-1) para facilitar a delimitação dos movimentos válidos
  m[,1] <- -1
  m[1,] <- -1
  m[nrow(m),] <- -1
  m[,ncol(m)] <- -1
  
  # Início
  # Início
  QIE <- c(altura-1, 2)
  QSD <- c(2, largura-1)
  QSE <- c(2, 2)
  QID <- c(altura-1, largura-1)
  
  # Determinar eixos principais ( origem e oposto )
  eixos <- function(E){
    if(E == "crescente"){
      origem_i <<- QIE[1] 
      origem_j <<- QIE[2]
      oposto_i <<- QSD[1]
      oposto_j <<- QSD[2]
      atual_i <<- origem_i
      atual_j <<- origem_j
    }
    if(E == "decrescente"){
      origem_i <<- QSE[1] 
      origem_j <<- QSE[2]
      oposto_i <<- QID[1]
      oposto_j <<- QID[2]
      atual_i <<- origem_i
      atual_j <<- origem_j
    }
  }
  
  eixos("crescente")
  
  m[atual_i, atual_j] <- 1
  
  # Trash variables
  Move <- c(0, 0)
  
  # S T A R T
  
  repeat{
    
    # Conferir se já chegou ao lado oposto, trocar origens com opostos
    if(atual_i == oposto_i && atual_j == oposto_j){
      origem_i <- origem_i + oposto_i
      oposto_i <- origem_i - oposto_i
      origem_i <- origem_i - oposto_i
      origem_j <- origem_j + oposto_j
      oposto_j <- origem_j - oposto_j
      origem_j <- origem_j - oposto_j
      atual_i <- origem_i
      atual_j <- origem_j
    }
    
    # Checar se o quadro já foi concluído
    if(sum(m==1)==((largura-1)/2)*((altura-1)/2)){
      break
    }
    
     
    
    ##############################################################
    # Movimento
    
    # Checar se movimento é válido com base em (atual + 1) e (atual + 2)
    # Em seguida, mover o pixel, junto com os valores de seu rastro
    # Regra dos rastros: O valor "2" será atribuído para as casas de posição ímpar
    #                   O valor "1" será atribuído para as casas de posição par
    #                 RAZÃO: isso facilitará a contabilização do preenchimento do labirinto,
    #               que deverá ser feita somando apenas as casas com valor = 1.
    
    # (0,   0) = válido: significa que não há caminhos adiante. Pode prosseguir.
    # (>=1, 1) = válido: significa que o caminho irá retroceder por rota pré-existente.
    # (0, >=1) = inválido:  não é permitido atravessar paredes
    # (-1,  X) = inválido:  não é permitido sair dos limites da matriz
    
    ###############################################################
    
    # Declarar movimentos viáveis
    if(m[(atual_i+1), (atual_j)]>=1){
      prob_baixo <- 1
    } else { # situações em que (atual_n+1) seja <= 0
      if(m[(atual_i+1), (atual_j)]==-1){
        prob_baixo <- 0
      } else { # situações em que (atual_n+1) seja == 0
        if(m[(atual_i+2), (atual_j)]==0){
          prob_baixo <- 1
        } else { # situação em que (atual_n+2) seja >= 1
          prob_baixo <- 0
        }
      }
    }
    
    if(m[(atual_i-1), (atual_j)]>=1){
      prob_cima <- 1
    } else { # situações em que (atual_n+1) seja <= 0
      if(m[(atual_i-1), (atual_j)]==-1){
        prob_cima <- 0
      } else { # situações em que (atual_n+1) seja == 0
        if(m[(atual_i-2), (atual_j)]==0){
          prob_cima <- 1
        } else { # situação em que (atual_n+2) seja >= 1
          prob_cima <- 0
        }
      }
    }
    
    if(m[(atual_i), (atual_j+1)]>=1){
      prob_dir <- 1
    } else { # situações em que (atual_n+1) seja <= 0
      if(m[(atual_i), (atual_j+1)]==-1){
        prob_dir <- 0
      } else { # situações em que (atual_n+1) seja == 0
        if(m[(atual_i), (atual_j+2)]==0){
          prob_dir <- 1
        } else { # situação em que (atual_n+2) seja >= 1
          prob_dir <- 0
        }
      }
    }
    
    if(m[(atual_i), (atual_j-1)]>=1){
      prob_esq <- 1
    } else { # situações em que (atual_n+1) seja <= 0
      if(m[(atual_i), (atual_j-1)]==-1){
        prob_esq <- 0
      } else { # situações em que (atual_n+1) seja == 0
        if(m[(atual_i), (atual_j-2)]==0){
          prob_esq <- 1
        } else { # situação em que (atual_n+2) seja >= 1
          prob_esq <- 0
        }
      }
    }
    
    # Sortear entre os viáveis
    if(sum(c(prob_cima, prob_baixo, prob_esq, prob_dir))==0){
      image(t(apply(m, 2, rev)), asp = altura/largura, axes=FALSE, col = c("black", "white", "white", "red"))
      break
    }
    direcao <- sample(c("cima", "baixo", "esq", "dir"), 1, prob = c(prob_cima, prob_baixo, prob_esq, prob_dir))
    
    # Preparar o movimento sorteado
    if(direcao == "cima"){
      Move[1] <- -2
      Move[2] <- 0
    }
    if(direcao == "baixo"){
      Move[1] <- 2
      Move[2] <- 0
    }
    if(direcao == "esq"){
      Move[1] <- 0
      Move[2] <- -2
    }
    if(direcao == "dir"){
      Move[1] <- 0
      Move[2] <- 2
    }
    
    
    # Movimento

    m[(atual_i+Move[1]), (atual_j+Move[2])] <- 1
    m[(atual_i+Move[1]/2), (atual_j+Move[2]/2)] <- 2
    atual_i <- atual_i+Move[1]
    atual_j <- atual_j+Move[2]
    next
    
  }
  
  # image(t(apply(m, 2, rev)), asp = altura/largura, axes=FALSE, col = c("black", "white", "white"))
  
  
  
  # D E T E R M I N E   D I F F I C U L T Y   L E V E L
  

  
  # Reset atual_i e atual_j
  atual_i <- origem_i
  atual_j <- origem_j
  
  # Definir outro eixo
  eixos("decrescente")
  
  # Trash variables
  m2 <- m
  Counts <- 0
  Step_count <- 3
  Passos <- 0
  Registro_passos <- 0
  Chegadas <- 0
  encruzilhadas <- 0
  notifica <- F
  
  # S T A R T
  
  repeat{
    
    # Conferir se já chegou ao lado oposto
    if(atual_i == oposto_i && 
       atual_j == oposto_j){
      
      print(Chegadas+1)
      
      # Após chegar ao lado oposto, voltar ao início e repetir o processo (e registrar o número de passos obtido)
      m2 <- m # Apagar os bloqueios de becos
      Chegadas <- Chegadas + 1
      atual_i <- origem_i
      atual_j <- origem_j
      Registro_passos[length(Registro_passos)+1] <- Passos
      Passos <- 0
      
      # Após ocorrer 5 chegadas, mudar de lado
      if(Chegadas == 5){
        origem_i <- origem_i + oposto_i
        oposto_i <- origem_i - oposto_i
        origem_i <- origem_i - oposto_i
        origem_j <- origem_j + oposto_j
        oposto_j <- origem_j - oposto_j
        origem_j <- origem_j - oposto_j
        atual_i <- origem_i
        atual_j <- origem_j
      }
      
      # Após ocorrer 5 chegadas do outro lado, terminar o programa
      if(Chegadas == 10){
        break
      }
    }
    
    # Identificar se chegou num beco sem saída
    if(sum(m2[(atual_i - 1):(atual_i + 1), (atual_j - 1):(atual_j + 1)]==0) == 7){
      notifica <- T
    }
    
    # Movimento
    # Declarar movimentos viáveis
    if(m2[(atual_i+1), (atual_j)]<=0){
      prob_baixo <- 0
    } else {
      prob_baixo <- 1
    }
    if(m2[(atual_i-1), (atual_j)]<=0){
      prob_cima <- 0
    } else {
      prob_cima <- 1
    }
    if(m2[atual_i, (atual_j-1)]<=0){
      prob_esq <- 0
    } else {
      prob_esq <- 1
    }
    if(m2[(atual_i), (atual_j+1)]<=0){
      prob_dir <- 0
    } else {
      prob_dir <- 1
    }
    
    # Sortear entre os viáveis
    if(sum(c(prob_cima, prob_baixo, prob_esq, prob_dir))==0){
      image(t(apply(m2, 2, rev)), asp = altura/largura, axes=FALSE, col = c("black", "white", "white", "red"))
      break
    }
    direcao <- sample(c("cima", "baixo", "esq", "dir"), 1, prob = c(prob_cima, prob_baixo, prob_esq, prob_dir))
    
    # Preparar o movimento sorteado
    if(direcao == "cima"){
      Move[1] <- -2
      Move[2] <- 0
    }
    if(direcao == "baixo"){
      Move[1] <- 2
      Move[2] <- 0
    }
    if(direcao == "esq"){
      Move[1] <- 0
      Move[2] <- -2
    }
    if(direcao == "dir"){
      Move[1] <- 0
      Move[2] <- 2
    }
      
      # Contabiliza o novo passo
      Passos <- Passos + 1
      
      if(notifica){
        # Se a notificação foi ativada, os próximos passos devem "selar" os caminhos com paredes
        # Isso impedirá retorno ao beco identificado
            
        # Primeiro concretizamos o movimento válido
        atual_i <- atual_i+Move[1]
        atual_j <- atual_j+Move[2]
            
        # Depois verificamos se o movimento válido cai em encruzilhada
        if(sum(m2[(atual_i - 1):(atual_i + 1), (atual_j - 1):(atual_j + 1)]<=0) <= 5){
          # Se o movimento cai em encruzilhada, a notificação é desligada, pois saímos do beco
          # Adicionalmente, contabilizo a presença da encruzilhada
          notifica <- F
          encruzilhadas <- encruzilhadas + 1
        }
        
        # Selar a entrada daquele beco com uma parede
        m2[(atual_i-Move[1]/2), (atual_j-Move[2]/2)] <- 0
        m2[atual_i, atual_j] <- 3
        next
        
      } else {
        # Caso não haja notificação, prossegue-se andando normalmente
        atual_i <- atual_i+Move[1]
        atual_j <- atual_j+Move[2]
        m2[atual_i, atual_j] <- 3
        next
      
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
