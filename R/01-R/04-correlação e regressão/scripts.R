amostra <- c(509, 505, 495, 510, 496, 509, 497, 502, 503, 505, 
             501, 505, 510, 505, 504, 497, 506, 506, 508, 505, 
             497, 504, 500, 498, 506, 496, 508, 497, 503, 501, 
             503, 506, 499, 498, 509, 507, 503, 499, 509, 495, 
             502, 505, 504, 509, 508, 501, 505, 497, 508, 507)

amostra <- data.frame(Amostra = amostra)
head(amostra)

media_amostra <- mean(amostra$Amostra)
media_amostra

desvio_padrao_amostra <- sd(amostra$Amostra)
desvio_padrao_amostra

media <- 500
significancia <- 0.05
confianca <- 1 - significancia
n <- 50

probabilidade <- (0.5 + (confianca / 2))
probabilidade

z_alpha_2 <- qnorm(probabilidade)
z_alpha_2

z <- (media_amostra - media) / (desvio_padrao_amostra / sqrt(n))
z

z <= -z_alpha_2

z >= z_alpha_2

if(z <= -z_alpha_2 || z >= z_alpha_2){
  'Rejeitar H0'
  }else{
    'Aceitar H0'
}

p_valor <- 2 * (1 - pnorm(z))
p_valor

p_valor <= significancia

# install.packages('DescTools')

library(DescTools)

ZTest(amostra$Amostra, mu = media, sd_pop = desvio_padrao_amostra)
resultado <- ZTest(amostra$Amostra, mu = media, sd_pop = desvio_padrao_amostra)

resultado$statistic

p_valor <- resultado$p.value
p_valor

p_valor <= significancia


########################################################################################
media <- 22
n <- 10
media_amostra <- 20
desvio_padrao <- 0.4
significancia <- 0.01

# 1) Qual a hipótese nula a ser testada?
hipotese <- 'H0: μ = 25'
hipotese

probabilidade <- (0.5 + (confianca / 2))
z_alpha_2 <- qnorm(probabilidade)

# 2) Qual o valor da estatística de teste?
z <- (media_amostra - media) / (desvio_padrao / sqrt(n))
z

# 3) Qual a conclusão do teste?
if (z <= -z_alpha_2 || z >= z_alpha_2) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

p_valor <- 2 * (1 - pnorm(z))
p_valor

media <- 150
n <- 20
graus_de_liberdade <- n - 1
media_amostra <- 230
desvio_padrao <- 90
significancia <- 0.05
confianca <- 1 - significancia

t_alpha <- qt(confianca, graus_de_liberdade)
t <- (media_amostra - media) / (desvio_padrao / sqrt(n))
t

if (t >= t_alpha) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

media <- 350
n <- 35
media_amostra <- 330
desvio_padrao <- 80
significancia <- 0.05
confianca <- 1 - significancia

z_alpha <- qnorm(confianca)
z <- (media_amostra - media) / (desvio_padrao / sqrt(n))
z

if (z <= -z_alpha) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

media <- 75
n <- 23
graus_de_liberdade <- n - 1
desvio_padrao <- 12
significancia <- 0.05

# 1) Média amostral = 82
t <- (82 - media) / (desvio_padrao / sqrt(n))

p_valor <- pt(t, graus_de_liberdade, lower.tail = T)
p_valor

if (p_valor <= significancia) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

# 2) Média amostral = 70
t <- (70 - media) / (desvio_padrao / sqrt(n))

p_valor <- pt(t, graus_de_liberdade, lower.tail = T)
p_valor

if (p_valor <= significancia) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

media <- 32

resultado <- t.test(dataset$Amostra, alternative = "two.sided", mu = media)
t <- resultado$statistic
t

p_valor <- resultado$p.value
p_valor

if (p_valor <= significancia) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

media_sala_A <- 5.3
desvio_padrao_sala_A <- 2.1
media_sala_B <- 7.6
desvio_padrao_sala_B <- 2.8

significancia <- 0.05
confianca <- 1 - significancia
n_sala_A <- 50
n_sala_B <- 55
D_0 <- 0

probabilidade <- 0.5 + (confianca / 2)
z_alpha_2 <- qnorm(probabilidade)

numerador <- (media_sala_A - media_sala_B) - D_0
denominador <- sqrt((desvio_padrao_sala_A ** 2 / n_sala_A) + (desvio_padrao_sala_B ** 2 / n_sala_B))
z <- numerador / denominador
z

if (z <= -z_alpha_2 || z >= z_alpha_2) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

media_Leblon <- 21800
desvio_padrao_Leblon <- 450
media_Ipanema <- 20300
desvio_padrao_Ipanema <- 320

significancia <- 0.05
confianca <- 1 - significancia
n_Leblon <- 13
n_Ipanema <- 13
D_0 <- 1000

t_alpha <- qt(confianca, n_Leblon + n_Ipanema - 2)

numerador <- (media_Leblon - media_Ipanema) - D_0
denominador <- sqrt((desvio_padrao_Leblon ** 2 / n_Leblon) + (desvio_padrao_Ipanema ** 2 / n_Ipanema))
t <- numerador / denominador
t

if (t >= t_alpha) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

pchisq(7.45,5)

alunos <- data.frame(
  Sem_Alura = c(4, 3, 5, 4, 8, 6, 5, 7, 6, 7, 8, 7, 3, 7, 4, 7, 5, 7, 5, 7, 6, 4, 3, 3, 6, 3, 7, 4, 6, 6),
  Com_Alura = c(8, 6, 10, 3, 9, 5, 8, 5, 4, 10, 5, 9, 8, 5, 10, 8, 7, 9, 9, 5, 5, 7, 8, 9, 8, 5, 5, 8, 5, 9)
)
alunos <- data.frame(
  Sem_Alura = c(4, 3, 5, 4, 8, 6, 5, 7, 6, 7, 8, 7, 3, 7, 4, 7, 5, 7, 5, 7, 6, 4, 3, 3, 6, 3, 7, 4, 6, 6),
  Com_Alura = c(8, 6, 10, 3, 9, 5, 8, 5, 4, 10, 5, 9, 8, 5, 10, 8, 7, 9, 9, 5, 5, 7, 8, 9, 8, 5, 5, 8, 5, 9)
)
significancia <- 0.1
confianca <- 1 - significancia
n <- 30

probabilidade <- (0.5 + (confianca / 2))
z_alpha_2 <- qnorm(probabilidade)

alunos['Dif'] <- alunos$Com_Alura - alunos$Sem_Alura
alunos['|Dif|'] <- abs(alunos['Dif'])
alunos <- alunos[order(alunos$'|Dif|'),]
alunos['Posto'] <- seq(1, nrow(alunos))
posto <- aggregate(x = alunos$'Posto', by = list('|Dif|' = alunos$'|Dif|'), FUN = mean)
colnames(posto) <- c('|Dif|', 'Posto')
alunos$Posto <- NULL
alunos <- merge(x = alunos, y = posto, by = '|Dif|', all.x = TRUE)
alunos['Posto (+)'] <- apply(alunos[, c('Dif', 'Posto')], 1, function(x) if(x[1] > 0) x[2] else 0)
alunos['Posto (-)'] <- apply(alunos[, c('Dif', 'Posto')], 1, function(x) if(x[1] < 0) x[2] else 0)
alunos$Posto <- NULL

T <- min(sum(alunos['Posto (+)']), sum(alunos['Posto (-)']))
T

mu_T <- (n * (n + 1)) / 4
sigma_T <- sqrt((n * (n + 1) * ((2 * n) + 1)) / 24)
Z <- (T - mu_T) / sigma_T
Z

if (Z <= -z_alpha_2 || Z >= z_alpha_2) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

pesos <- data.frame(
  Sem_Composto = c(180, -39, 325, 303, 127, 149, 271, 163, 287, 255, 398, 324, 335, 421, 
                   216, 274, 373, 116, 197, -37, 321, 431, 112, 304, 417, 362, 51, 187, 195, 304, 202, 158, 105, 90, 245),
  Com_Composto = c(484, 187, 442, 108, 488, 283, 286, 419, 240, 266, 198, 130, 484, 424, 
                   145, 133, 282, 386, 408, 290, 429, 386, 318, 390, 347, 442, 440, 356, 517, 454, 401, 108, 228, 471, 495)
)

resultado <- wilcox.test(
  pesos$Sem_Composto,
  pesos$Com_Composto,
  alternative = "two.sided",
  paired = T
)

resultado

media <- 2500
media_amostra <- 2400
desvio_padrao <- 500
n <- 300
significancia <- 0.03

z <- (media_amostra - media) / (desvio_padrao / sqrt(n))
z

p_valor <- pnorm(z, lower.tail = T)
p_valor

if (p_valor <= significancia) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

