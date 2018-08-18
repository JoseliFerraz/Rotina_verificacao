
# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - IPEA     | #
# |                    PROJETO IPEADATA                   | #
# --------------------------------------------------------- #
# |   COORDENADOR:                    | #
# --------------------------------------------------------- #
# |   PROGRAMADORA: Joseli Moreira Ferraz                 | #
# |   PROGRAMADORA: Gabriella do Nascimento Pinheiro      | #
# --------------------------------------------------------- #
# |   Rotina de verificacao                               | #
# |   Incio 14/8/2018                                     | #
# --------------------------------------------------------- #

# -----------------------------------------------

# Construcao do algoritmo para verificacao delimitando 
# a regiao Brasil
# -----------------------------------------------

# -----------------------------------------------
# Pacotes  
#install.packages("ipeaData")
#install.packages("data.table")
#install.packages("knitr") #pacote para bordas de tabela 
#install.packages("gridExtra")
library(ipeaData)

library(data.table)
library(knitr)
library(gridExtra)
# Fixar todas as casas decimais dos dados
options(digits=17)
# -----------------------------------------------

# Funciona BPC_defi

# -----------------------------------------------
# Pra aAnalise Regional social
# -----------------------------------------------
data <- ipeadata('DISOC_PPY')

# Selecao por: Brasil
#data_brasil=data[data$TERCODIGO==0]
data_brasil=data[data$TERCODIGO==0]



# -----------------------------------------------
# Pra aAnalise Macroeconomica
# -----------------------------------------------
data_brasil=data[data$TERNOME=="Brasil"]
data_brasil=data


# -----------------------------------------------
# Analise de consistencia : Padrao da Planilha
# -----------------------------------------------

# Calculo dos desvios
media=mean(data_brasil$VALVALOR,na.rm=T)
dp1=sd(data_brasil$VALVALOR,na.rm=T)
dp2=2*dp1
dp3=3*dp1
dp4=4*dp1
dp5=5*dp1

# Gerando tabela 
# kable(table(media,dp1,dp2,dp3,dp4,dp5))
grid.table(c(media,dp1,dp2,dp3,dp4,dp5),c("media","+1DP", "+2DP", "+3DP","+4DP","+5DP"))

# DPs
# Media menos os desvios
dp_menos1=media-dp1
dp_menos2=media-dp2
dp_menos3=media-dp3
dp_menos4=media-dp4
dp_menos5=media-dp5


# Media mais o desvio
dp_mais1=media+dp1
dp_mais2=media+dp2
dp_mais3=media+dp3
dp_mais4=media+dp4
dp_mais5=media+dp5

# -----------------------------------------------------
# Tabela Calculos consistencia (Desvios)
# -----------------------------------------------------

# Limpando area de plotagem
dev.off()

# Criando nomes 
#kable(table(dp_menos5,dp_menos4,dp_menos3,dp_menos2,dp_menos1,media,dp_mais1,dp_mais2,dp_mais3,dp_mais4,dp_mais5))
names1=c("-5DP", "-4DP", "-3DP",
         "-2DP", "-1DP", "media",
         "+1DP", "+2DP", "+3DP","+4DP","+5DP")

# Gerando tabela 
grid.table(c(dp_menos5,dp_menos4,dp_menos3,dp_menos2,dp_menos1,media,dp_mais1,dp_mais2,dp_mais3,dp_mais4,dp_mais5),names1)


# -----------------------------------------------------------
# Tabela 5 ponto da Mediana
# -----------------------------------------------------------
# Limpando area de plotagem
dev.off()


descritiva=summary(data_brasil$VALVALOR)
mediana=as.numeric(descritiva[3]) # Mediana
quartil_1=as.numeric(descritiva[2]) # 1 Quartil
quartil_3=as.numeric(descritiva[5]) # 3 Quartil

# LI
LI=mediana-(1.5*(quartil_3-quartil_1));LI
Ls=mediana+(1.5*(quartil_3-quartil_1));Ls

# --------------------------------
# Por resumo dos 5 pontos
# --------------------------------


data_brasil$VALVALOR=data_brasil$VALVALOR[is.na(data_brasil$VALVALOR)]=9999999999999
# write.csv2(data_brasil,"dados3.csv")

cinco_ponto_na=c()
cinco_ponto=c() # variavel para armazenar valores
for(i in 1:length(data_brasil$VALVALOR)){
  if(data_brasil$VALVALOR[i]==9999999999999){
    cinco_ponto_na[i]=9999999999999
  }else
    
  # +ou- 3 dp
  if(data_brasil$VALVALOR[i]<LI && data_brasil$VALVALOR[i]>Ls)
    cinco_ponto[i]=1
  else{
    cinco_ponto[i]=0
  }
  #soma_cinco=sum(cinco_ponto,rm.na=T)
}
soma_cinco=sum(cinco_ponto,rm.na=T);soma_cinco




# --------------------------------
# Contagem de Missin :Verificar em serie que possui vazio
# --------------------------------
contagem_missing=c()



for(i in 1:length(data_brasil$VALVALOR)){
if(data_brasil$VALVALOR[i]==""){
  contagem_missing[i]=1
}
}
soma_missing=sum(contagem_missing)
# ------------------
#Dados totais
# ------------------
total=length(data_brasil$VALVALOR);total


# -----------------------------------------------------------
# 
# -----------------------------------------------------------
# Variaveis que receberao o calculo da quantidade de desvios
qdt_3dp=c();qdt_4dp=c();qdt_5dp=c()

# Construcao do algoritmo
for(i in 1:length(data_brasil$VALVALOR)){
  # +ou- 3 dp
  if(data_brasil$VALVALOR[i]<dp_menos3 && data_brasil$VALVALOR[i]>dp_menos3)
    qdt_3dp[i]=1
  else{
    qdt_3dp[i]=0
  }
  # +ou- 4 dp
  if(data_brasil$VALVALOR[i]<dp_menos4 && data_brasil$VALVALOR[i]>dp_menos4)
    qdt_4dp[i]=1
  else{
    qdt_4dp[i]=0
  }
  # +ou- 5 dp
  if(data_brasil$VALVALOR[i]<dp_menos5 && data_brasil$VALVALOR[i]>dp_menos5)
    qdt_5dp[i]=1
  else{
    qdt_5dp[i]=0
  }
}
som3qtd=sum(qdt_3dp);som4qtd=sum(qdt_4dp);som5qtd=sum(qdt_5dp)

names2=c("+-3DP","+-4DP","+-5DP","Por 5 pts","Missing","Dados Total")
grid.table(c(som3qtd,som4qtd,som5qtd,soma_cinco,soma_missing,total),names2)

# -----------------------------------------------------
# -----------------------------------------------------
# Para mais tarde utilizar com tecnicas de textmining
# -----------------------------------------------------
# -----------------------------------------------------
data1=ipeadata('BPC_defi', type = "data.table")


get_metadata('BPC_defi', type = "data.table")


# -----------------------------------------------------
# -----------------------------------------------------
# Conteudo de pesquisa
# -----------------------------------------------------
# -----------------------------------------------------

# https://cran.r-project.org/web/packages/ipeaData/ipeaData.pdf
# -----------------------------------------
# - Pacote para bordas em Tabelas
# https://stackoverflow.com/questions/20456480/how-do-i-make-borders-in-a-contingency-table-built-in-r
# https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
# -----------------------------------------

# search_serie(term = NULL, fields_search = c("SERNOME"),
#              type = "data.table")
# 
# get_metadata(serie = NULL, type = "data.table")
# rm(i,pacotes)
