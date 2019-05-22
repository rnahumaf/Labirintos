# Lista com coordenadas dos pontos [x, y]
lista_a <- cbind(c(1,3), c(3,1), c(5,2)); rownames(lista_a) <- c("x", "y")

# Lista com combinações entre os pontos
#   E.g. se a lista_a possuir 4 pontos: aa, bb, cc, dd.
#   As combinações então seriam 6: aabb, aacc, aadd, bbcc, bbdd, ccdd.
lista_b <- combn(1:ncol(lista_a), 2)

# Lista com distância das combinações

## lista_a[1, p] # posição x, ponto p
## lista_a[2, p] # posição y, ponto p

## lista_b[, i] # combinação i

# A combinação i tem como resultado os pontos p e p' [, i] = [1,] p [2,] p'
# se p for usado em j[1,] e j[2,], fornecerá a posição x e y, respectivamente
# se p' for usado em j[1,] e j[2,], fornecerá a posição x e y, respectivamente

lista_c <- c()

for(i in 1:ncol(lista_b)){

xp <- lista_a[1, lista_b[1, i]] # Posição x do p
xpl <- lista_a[1, lista_b[2, i]] # Posição x do p'

yp <- lista_a[2, lista_b[1, i]] # Posição y do p
ypl <- lista_a[2, lista_b[2, i]] # Posição y do p'

lista_c[i] <- sqrt((xp-xpl)^2+(yp-ypl)^2)

}

# Escolher ponto aleatório para começar a árvore
pstart <- sample(1:ncol(lista_a), 1)

# Escolher o ponto mais próximo
opcoes <- lista_b[1,]==pstart | lista_b[2,]==pstart
proximo <- which(lista_c==min(lista_c[opcoes]))

# Desenhar a linha
plot(lista_a[1,], lista_a[2,])

xp <- lista_a[1, lista_b[1, proximo]] # Posição x do p
xpl <- lista_a[1, lista_b[2, proximo]] # Posição x do p'

yp <- lista_a[2, lista_b[1, proximo]] # Posição y do p
ypl <- lista_a[2, lista_b[2, proximo]] # Posição y do p'

lines(c(xp, yp), c(xpl, ypl))

# Armazenar combinações utilizadas num vetor
lista_u <- proximo

# Escolher ponto da árvore

