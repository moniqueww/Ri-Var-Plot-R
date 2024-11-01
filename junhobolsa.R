#lendo o arquivo
dados <- read.table("~/Área de Trabalho/171819.txt",sep=",")
install.packages("ggplot2")
#criando a matriz dos dados de todos os 3 dias
t <- matrix(0, ncol=3,nrow=1440)
ur <- matrix(0, ncol=3,nrow=1440)
p <- matrix(0, ncol=3,nrow=1440)
vel_v <- matrix(0, ncol=3,nrow=1440)
dir_v <- matrix(0, ncol=3,nrow=1440)
#vetor das horas
horas <- vector("integer", length = 1397)

#atribuindo dados nas matrizes
#temperatura
for(i in 1:1397){
  t[i,1] <- dados[i,6]
}
for(i in 1398:2837){
  t[i-1398,2] <- dados[i,6]
}
for(i in 2838:4277){
  t[i-2838,3] <- dados[i,6]
}
#umidade relativa
for(i in 1:1397){
  ur[i,1] <- dados[i,7]
}
for(i in 1398:2837){
  ur[i-1398,2] <- dados[i,7]
}
for(i in 2838:4277){
  ur[i-2838,3] <- dados[i,7]
}
#pressao
for(i in 1:1397){
  p[i,1] <- dados[i,8]
}
for(i in 1398:2837){
  p[i-1398,2] <- dados[i,8]
}
for(i in 2838:4277){
  p[i-2838,3] <- dados[i,8]
}
#velocidade do vento
for(i in 1:1397){
  vel_v[i,1] <- dados[i,9]
}
for(i in 1398:2837){
  vel_v[i-1398,2] <- dados[i,9]
}
for(i in 2838:4277){
  vel_v[i-2838,3] <- dados[i,9]
}
#dire??o do vento
for(i in 1:1397){
  dir_v[i,1] <- dados[i,10]
}
for(i in 1398:2837){
  dir_v[i-1398,2] <- dados[i,10]
}
for(i in 2838:4277){
  dir_v[i-2838,3] <- dados[i,10]
}


#-------------------------------------PLOTS----------------------------------------------------#
direcao <- vector("integer",length=720)
velocidade <- vector("integer",length=720)
for(i in 1:681){
  direcao[i] <- dir_v[i,1]
  velocidade[i] <- vel_v[i,1]
}
for(i in 1:720){
  direcao[i] <- dir_v[i,3]
  velocidade[i] <- vel_v[i,3]
}

# Criar um data frame com os dados
data <- data.frame(Horas = seq(as.POSIXct("2023-09-01 00:00:00"), as.POSIXct("2023-09-01 11:59:00"), by = "1 min"), Direcao = direcao, Magnitude = velocidade)

# Criar o gráfico com duas escalas de eixo y
ggplot(data, aes(x = Horas)) +
  geom_line(aes(y = Direcao, color = "Direção"), size = 0.6) +
  geom_line(aes(y = Magnitude * 70, color = "Magnitude"), size = 0.6) +
  scale_color_manual(values = c("Direção" = "blue", "Magnitude" = "red")) +
  labs(title = "Direção e Magnitude do Vento - Dia 17", x = "", y = "Direção") +
  scale_y_continuous(sec.axis = sec_axis(~./70, name = "Magnitude")) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") + # Mostrar a cada 3 horas
  theme_minimal()

#ur e temperatura
umida <- vector("integer",length=1439)
tt <- vector("integer",length=1439)
for(i in 1:1440){
  if(i > 680 && i < 727){
    umida[i] <- NA
    tt[i] <- NA
  }else if(i>727){
    umida[i] <- ur[i-45,1]
    tt[i] <- t[i-45,1]
  }else{
    umida[i] <- ur[i,1]
    tt[i] <- t[i,1]
  }
}
for(i in 1:1440){
  umida[i] <- ur[i,3]
  tt[i] <- t[i,3]
  if(i == 1440){
    umida[i] <- NA
    tt[i] <- NA
  }
}
# Criar um data frame com os dados
data <- data.frame(Horas = seq(as.POSIXct("2023-09-01 00:00:00"), as.POSIXct("2023-09-01 23:59:00"), by = "1 min"), umi = umida, temperatura = tt)

# Criar o gráfico com duas escalas de eixo y
ggplot(data, aes(x = Horas)) +
  geom_line(aes(y = umi, color = "UR"), size = 0.5) +
  geom_line(aes(y = temperatura * 6 , color = "Temperatura"), size = 0.5) +
  scale_color_manual(values = c("UR" = "orange", "Temperatura" = "green")) +
  labs(title = "Temperatura e Umidade Relativa - Dia 17", x = "", y = "Umidade relativa") +
  scale_y_continuous(sec.axis = sec_axis(~./6, name = "Temperatura")) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") + # Mostrar a cada 3 horas
  theme_minimal()


#----------------------------------------NÚMERO DE RICHARDSON---------------------------------#
#CONSTANTES DA SONDAGEM
ctes_sondagem_dia17 <- matrix(0, ncol=4,nrow=4)
ctes_sondagem_dia18 <- matrix(0,ncol=4,nrow=5)
ctes_sondagem_dia19 <- matrix(0,ncol=4,nrow=6)

#pressão
ctes_sondagem_dia17[1,1] <- 1000
ctes_sondagem_dia17[2,1] <- 968
ctes_sondagem_dia17[3,1] <- 955
ctes_sondagem_dia17[4,1] <- 925

ctes_sondagem_dia18[1,1] <- 1004
ctes_sondagem_dia18[2,1] <- 1000
ctes_sondagem_dia18[3,1] <- 986
ctes_sondagem_dia18[4,1] <- 935
ctes_sondagem_dia18[5,1] <- 925

ctes_sondagem_dia19[1,1] <- 1001
ctes_sondagem_dia19[2,1] <- 1000
ctes_sondagem_dia19[3,1] <- 980
ctes_sondagem_dia19[4,1] <- 960
ctes_sondagem_dia19[5,1] <- 945
ctes_sondagem_dia19[6,1] <- 925

#altura
ctes_sondagem_dia17[1,2] <- 219
ctes_sondagem_dia17[2,2] <- 490
ctes_sondagem_dia17[3,2] <- 602
ctes_sondagem_dia17[4,2] <- 866

ctes_sondagem_dia18[1,2] <- 186
ctes_sondagem_dia18[2,2] <- 220
ctes_sondagem_dia18[3,2] <- 337
ctes_sondagem_dia18[4,2] <- 777
ctes_sondagem_dia18[5,2] <- 866

ctes_sondagem_dia19[1,2] <- 196
ctes_sondagem_dia19[2,2] <- 205
ctes_sondagem_dia19[3,2] <- 372
ctes_sondagem_dia19[4,2] <- 543
ctes_sondagem_dia19[5,2] <- 674
ctes_sondagem_dia19[6,2] <- 853

#sknt
ctes_sondagem_dia17[1,3] <- 2.05778
ctes_sondagem_dia17[2,3] <- 4.11556
ctes_sondagem_dia17[3,3] <- 5.14444
ctes_sondagem_dia17[4,3] <- 5.65889

ctes_sondagem_dia18[1,3] <- 0.51444
ctes_sondagem_dia18[2,3] <- 0.51444
ctes_sondagem_dia18[3,3] <- 1.54333
ctes_sondagem_dia18[4,3] <- 5.65889
ctes_sondagem_dia18[5,3] <- 7.20222

ctes_sondagem_dia19[1,3] <- 2.05778
ctes_sondagem_dia19[2,3] <- 2.05778
ctes_sondagem_dia19[3,3] <- 4.11556
ctes_sondagem_dia19[4,3] <- 6.68778
ctes_sondagem_dia19[5,3] <- 8.23111
ctes_sondagem_dia19[6,3] <- 7.71667

#THTA
ctes_sondagem_dia17[1,4] <- 284.1
ctes_sondagem_dia17[2,4] <- 286.4
ctes_sondagem_dia17[3,4] <- 286.4
ctes_sondagem_dia17[4,4] <- 286.5

ctes_sondagem_dia18[1,4] <- 282
ctes_sondagem_dia18[2,4] <- 282.1
ctes_sondagem_dia18[3,4] <- 283.3
ctes_sondagem_dia18[4,4] <- 286.1
ctes_sondagem_dia18[5,4] <- 286.7

ctes_sondagem_dia19[1,4] <- 277.9
ctes_sondagem_dia19[2,4] <- 278.4
ctes_sondagem_dia19[3,4] <- 284
ctes_sondagem_dia19[4,4] <- 285.1
ctes_sondagem_dia19[5,4] <- 289.2
ctes_sondagem_dia19[6,4] <- 291.8

#EXPRESSÕES PARA RI
#MÉDIA THETA
#MÉDIA DE THETA PARA 10 HORAS DO DIA 17
theta_dia17_10horas <-matrix(0,ncol=4,nrow=60)
for(i in 1:60){
  theta <- t[600+i,1]*((1000/p[600+i,1])**0.28) +273.15
  theta_dia17_10horas[i,1] <- (theta+ctes_sondagem_dia17[1,4])/2
  theta_dia17_10horas[i,2] <- (theta+ctes_sondagem_dia17[1,4]+ctes_sondagem_dia17[2,4])/3
  theta_dia17_10horas[i,3] <- (theta+ctes_sondagem_dia17[1,4]+ctes_sondagem_dia17[2,4]+ctes_sondagem_dia17[3,4])/4
  theta_dia17_10horas[i,4] <- (theta+ctes_sondagem_dia17[1,4]+ctes_sondagem_dia17[2,4]+ctes_sondagem_dia17[3,4]+ctes_sondagem_dia17[4,4])/5
}

#MÉDIA DE THETA PARA 9 HORAS DO DIA 18
theta_dia18_9horas <-matrix(0,ncol=5,nrow=60)
for(i in 1:60){
  theta <- t[540+i,2]*((1000/p[540+i,2])**0.28) +273.15
  theta_dia18_9horas[i,1] <- (theta+ctes_sondagem_dia18[1,4])/2
  theta_dia18_9horas[i,2] <- (theta+ctes_sondagem_dia18[1,4]+ctes_sondagem_dia18[2,4])/3
  theta_dia18_9horas[i,3] <- (theta+ctes_sondagem_dia18[1,4]+ctes_sondagem_dia18[2,4]+ctes_sondagem_dia18[3,4])/4
  theta_dia18_9horas[i,4] <- (theta+ctes_sondagem_dia18[1,4]+ctes_sondagem_dia18[2,4]+ctes_sondagem_dia18[3,4]+ctes_sondagem_dia18[4,4])/5
  theta_dia18_9horas[i,5] <- (theta+ctes_sondagem_dia18[1,4]+ctes_sondagem_dia18[2,4]+ctes_sondagem_dia18[3,4]+ctes_sondagem_dia18[4,4]+ctes_sondagem_dia18[5,4])/6
}

#MÉDIA DE THETA PARA 6 HORAS DO DIA 19
theta_dia19_6horas <-matrix(0,ncol=6,nrow=60)
for(i in 1:60){
  theta <- t[360+i,3]*((1000/p[360+i,3])**0.28) +273.15
  theta_dia19_6horas[i,1] <- (theta+ctes_sondagem_dia19[1,4])/2
  theta_dia19_6horas[i,2] <- (theta+ctes_sondagem_dia19[1,4]+ctes_sondagem_dia19[2,4])/3
  theta_dia19_6horas[i,3] <- (theta+ctes_sondagem_dia19[1,4]+ctes_sondagem_dia19[2,4]+ctes_sondagem_dia19[3,4])/4
  theta_dia19_6horas[i,4] <- (theta+ctes_sondagem_dia19[1,4]+ctes_sondagem_dia19[2,4]+ctes_sondagem_dia19[3,4]+ctes_sondagem_dia19[4,4])/5
  theta_dia19_6horas[i,5] <- (theta+ctes_sondagem_dia19[1,4]+ctes_sondagem_dia19[2,4]+ctes_sondagem_dia19[3,4]+ctes_sondagem_dia19[4,4]+ctes_sondagem_dia19[5,4])/6
  theta_dia19_6horas[i,6] <- (theta+ctes_sondagem_dia19[1,4]+ctes_sondagem_dia19[2,4]+ctes_sondagem_dia19[3,4]+ctes_sondagem_dia19[4,4]+ctes_sondagem_dia19[5,4]+ctes_sondagem_dia19[6,4])/6
}



#DELTA THETA
#DELTA THETA PARA 10 HORAS DO DIA 17
deltatheta_dia17_10horas <-matrix(0,ncol=4,nrow=60)
for(i in 1:60){
  theta <- t[600+i,1]*((1000/p[600+i,1])**0.28) +273.15
  deltatheta_dia17_10horas[i,1] <- ctes_sondagem_dia17[1,4] - theta
  deltatheta_dia17_10horas[i,2] <- ctes_sondagem_dia17[2,4] - theta
  deltatheta_dia17_10horas[i,3] <- ctes_sondagem_dia17[3,4] - theta
  deltatheta_dia17_10horas[i,4] <- ctes_sondagem_dia17[4,4] - theta
}

#DELTA THETA PARA 9 HORAS DO DIA 18
deltatheta_dia18_9horas <-matrix(0,ncol=5,nrow=60)
for(i in 1:60){
  theta <- t[540+i,2]*((1000/p[541+i,2])**0.28) +273.15
  deltatheta_dia18_9horas[i,1] <- ctes_sondagem_dia18[1,4] - theta
  deltatheta_dia18_9horas[i,2] <- ctes_sondagem_dia18[2,4] - theta
  deltatheta_dia18_9horas[i,3] <- ctes_sondagem_dia18[3,4] - theta
  deltatheta_dia18_9horas[i,4] <- ctes_sondagem_dia18[4,4] - theta
  deltatheta_dia18_9horas[i,5] <- ctes_sondagem_dia18[5,4] - theta
}

#DELTA THETA PARA 6 HORAS DO DIA 19
deltatheta_dia19_6horas <-matrix(0,ncol=6,nrow=60)
for(i in 1:60){
  theta <- t[360+i,3]*((1000/p[360+i,3])**0.28) +273.15
  deltatheta_dia19_6horas[i,1] <- ctes_sondagem_dia19[1,4] - theta
  deltatheta_dia19_6horas[i,2] <- ctes_sondagem_dia19[2,4] - theta
  deltatheta_dia19_6horas[i,3] <- ctes_sondagem_dia19[3,4] - theta
  deltatheta_dia19_6horas[i,4] <- ctes_sondagem_dia19[4,4] - theta
  deltatheta_dia19_6horas[i,5] <- ctes_sondagem_dia19[5,4] - theta
  deltatheta_dia19_6horas[i,6] <- ctes_sondagem_dia19[6,4] - theta
}

#DELTA Z
deltaz_dia17_10horas <- matrix(0,ncol=4,nrow=1)
z <- 3
deltaz_dia17_10horas[1,1] <- ctes_sondagem_dia17[1,2] - z
deltaz_dia17_10horas[1,2] <- ctes_sondagem_dia17[2,2] - z
deltaz_dia17_10horas[1,3] <- ctes_sondagem_dia17[3,2] - z
deltaz_dia17_10horas[1,4] <- ctes_sondagem_dia17[4,2] - z

deltaz_dia18_9horas <- matrix(0,ncol=5,nrow=1)
deltaz_dia18_9horas[1,1] <- ctes_sondagem_dia18[1,2] - z
deltaz_dia18_9horas[1,2] <- ctes_sondagem_dia18[2,2] - z
deltaz_dia18_9horas[1,3] <- ctes_sondagem_dia18[3,2] - z
deltaz_dia18_9horas[1,4] <- ctes_sondagem_dia18[4,2] - z
deltaz_dia18_9horas[1,5] <- ctes_sondagem_dia18[5,2] - z

deltaz_dia19_6horas <- matrix(0,ncol=6,nrow=1)
deltaz_dia19_6horas[1,1] <- ctes_sondagem_dia19[1,2] - z
deltaz_dia19_6horas[1,2] <- ctes_sondagem_dia19[2,2] - z
deltaz_dia19_6horas[1,3] <- ctes_sondagem_dia19[3,2] - z
deltaz_dia19_6horas[1,4] <- ctes_sondagem_dia19[4,2] - z
deltaz_dia19_6horas[1,5] <- ctes_sondagem_dia19[5,2] - z
deltaz_dia19_6horas[1,6] <- ctes_sondagem_dia19[6,2] - z

#DELTA U
#DELTA VELOCIDADE PARA 10 HORAS DO DIA 17
deltau_dia17_10horas <-matrix(0,ncol=4,nrow=60)
for(i in 1:60){
  u <- vel_v[i+600,1]
  deltau_dia17_10horas[i,1] <- ctes_sondagem_dia17[1,3] - u
  deltau_dia17_10horas[i,2] <- ctes_sondagem_dia17[2,3] - u
  deltau_dia17_10horas[i,3] <- ctes_sondagem_dia17[3,3] - u
  deltau_dia17_10horas[i,4] <- ctes_sondagem_dia17[4,3] - u
}

#DELTA VELOCIDADE PARA 9 HORAS DO DIA 18
deltau_dia18_9horas <-matrix(0,ncol=5,nrow=60)
for(i in 1:60){
  u <- vel_v[i+540,2]
  deltau_dia18_9horas[i,1] <- ctes_sondagem_dia18[1,3] - u
  deltau_dia18_9horas[i,2] <- ctes_sondagem_dia18[2,3] - u
  deltau_dia18_9horas[i,3] <- ctes_sondagem_dia18[3,3] - u
  deltau_dia18_9horas[i,4] <- ctes_sondagem_dia18[4,3] - u
  deltau_dia18_9horas[i,5] <- ctes_sondagem_dia18[5,3] - u
}

#DELTA VELOCIDADE PARA 6 HORAS DO DIA 19
deltau_dia19_6horas <-matrix(0,ncol=6,nrow=60)
for(i in 1:60){
  u <- vel_v[i+360,3]
  deltau_dia19_6horas[i,1] <- ctes_sondagem_dia19[1,3] - u
  deltau_dia19_6horas[i,2] <- ctes_sondagem_dia19[2,3] - u
  deltau_dia19_6horas[i,3] <- ctes_sondagem_dia19[3,3] - u
  deltau_dia19_6horas[i,4] <- ctes_sondagem_dia19[4,3] - u
  deltau_dia19_6horas[i,5] <- ctes_sondagem_dia19[5,3] - u
  deltau_dia19_6horas[i,6] <- ctes_sondagem_dia19[6,3] - u
}


#RI
RI_dia17_10horas <-matrix(0,ncol=4,nrow=60)
for(i in 1:60){
  RI_dia17_10horas[i,1] <- (9.8/theta_dia17_10horas[i,1])*((deltatheta_dia17_10horas[i,1]*deltaz_dia17_10horas[1,1])/((deltau_dia17_10horas[i,1])**2))
  RI_dia17_10horas[i,2] <- (9.8/theta_dia17_10horas[i,2])*((deltatheta_dia17_10horas[i,2]*deltaz_dia17_10horas[1,2])/((deltau_dia17_10horas[i,2])**2))
  RI_dia17_10horas[i,3] <- (9.8/theta_dia17_10horas[i,3])*((deltatheta_dia17_10horas[i,3]*deltaz_dia17_10horas[1,3])/((deltau_dia17_10horas[i,3])**2))
  RI_dia17_10horas[i,4] <- (9.8/theta_dia17_10horas[i,4])*((deltatheta_dia17_10horas[i,4]*deltaz_dia17_10horas[1,4])/((deltau_dia17_10horas[i,4])**2))
}

#RI PARA 9 HORAS DO DIA 18
RI_dia18_9horas <-matrix(0,ncol=5,nrow=60)
for(i in 1:60){
  RI_dia18_9horas[i,1] <- (9.8/theta_dia18_9horas[i,1])*((deltatheta_dia18_9horas[i,1]*deltaz_dia18_9horas[1,1])/((deltau_dia18_9horas[i,1])**2))
  RI_dia18_9horas[i,2] <- (9.8/theta_dia18_9horas[i,2])*((deltatheta_dia18_9horas[i,2]*deltaz_dia18_9horas[1,2])/((deltau_dia18_9horas[i,2])**2))
  RI_dia18_9horas[i,3] <- (9.8/theta_dia18_9horas[i,3])*((deltatheta_dia18_9horas[i,3]*deltaz_dia18_9horas[1,3])/((deltau_dia18_9horas[i,3])**2))
  RI_dia18_9horas[i,4] <- (9.8/theta_dia18_9horas[i,4])*((deltatheta_dia18_9horas[i,4]*deltaz_dia18_9horas[1,4])/((deltau_dia18_9horas[i,4])**2))
  RI_dia18_9horas[i,5] <- (9.8/theta_dia18_9horas[i,5])*((deltatheta_dia18_9horas[i,5]*deltaz_dia18_9horas[1,5])/((deltau_dia18_9horas[i,5])**2))
}

#RI PARA 6 HORAS DO DIA 19
RI_dia19_6horas <-matrix(0,ncol=6,nrow=60)
for(i in 1:60){
  RI_dia19_6horas[i,1] <- (9.8/theta_dia19_6horas[i,1])*((deltatheta_dia19_6horas[i,1]*deltaz_dia19_6horas[1,1])/((deltau_dia19_6horas[i,1])**2))
  RI_dia19_6horas[i,2] <- (9.8/theta_dia19_6horas[i,2])*((deltatheta_dia19_6horas[i,2]*deltaz_dia19_6horas[1,2])/((deltau_dia19_6horas[i,2])**2))
  RI_dia19_6horas[i,3] <- (9.8/theta_dia19_6horas[i,3])*((deltatheta_dia19_6horas[i,3]*deltaz_dia19_6horas[1,3])/((deltau_dia19_6horas[i,3])**2))
  RI_dia19_6horas[i,4] <- (9.8/theta_dia19_6horas[i,4])*((deltatheta_dia19_6horas[i,4]*deltaz_dia19_6horas[1,4])/((deltau_dia19_6horas[i,4])**2))
  RI_dia19_6horas[i,5] <- (9.8/theta_dia19_6horas[i,5])*((deltatheta_dia19_6horas[i,5]*deltaz_dia19_6horas[1,5])/((deltau_dia19_6horas[i,5])**2))
  RI_dia19_6horas[i,6] <- (9.8/theta_dia19_6horas[i,6])*((deltatheta_dia19_6horas[i,6]*deltaz_dia19_6horas[1,6])/((deltau_dia19_6horas[i,6])**2))
}

#plot Ri
# Carregar a biblioteca ggplot2
library(ggplot2)

# Criar um conjunto de dados de exemplo com valores para 1 hora (60 minutos)
RI <- data.frame(
  Tempo = seq(as.POSIXct("2023-09-01 06:00:00"), as.POSIXct("2023-09-01 06:59:00"), by = "1 min"),
  Valores = RI_dia19_6horas[,6]
)

# Criar e combinar os gráficos do dia 17
RI1 <- data.frame(
  Tempo = seq(as.POSIXct("2023-09-01 10:00:00"), as.POSIXct("2023-09-01 10:59:00"), by = "1 min"),
  Valores1 = RI_dia17_10horas[,1],
  Valores2 = RI_dia17_10horas[,2],
  Valores3 = RI_dia17_10horas[,3],
  Valores4 = RI_dia17_10horas[,4]
)

ggplot(RI1, aes(x = Tempo)) +
  geom_line(aes(y = Valores2, color = "968 hPa"), size = 0.5) +
  geom_line(aes(y = Valores3, color = "955 hPa"), size = 0.5) +
  geom_line(aes(y = Valores4, color = "925 hPa"), size = 0.5) +
  scale_color_manual(values = c( "968 hPa" = "orange", "955 hPa" = "blue", "925 hPa" = "green")) +
  labs(title = "Número de Richardson - 17/06/2023", x = "", y = "Número de Richardson") +
  coord_cartesian(ylim = c(-5, 5 )) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "15 min") + # Mostrar a cada 3 horas
  theme_minimal()



