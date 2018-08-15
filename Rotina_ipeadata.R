# -----------------------------------------------
# Programacao rotina de verificacao
# Incio 14/8/2018
# 
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

# Funciona
data <- ipeadata('BPC_defi')

# Selecao por: Brasil
data_brasil=data[data$TERCODIGO==0]

# -----------------------------------------------
# Analise de consistencia : Padrao da Planilha
# -----------------------------------------------

# Calculo dos desvios
media=mean(data_brasil$VALVALOR)
dp1=sd(data_brasil$VALVALOR)
dp2=2*dp1
dp3=3*dp1
dp4=4*dp1
dp5=5*dp1
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
#kable(table(dp_menos5,dp_menos4,dp_menos3,dp_menos2,dp_menos1,media,dp_mais1,dp_mais2,dp_mais3,dp_mais4,dp_mais5))
names1=c("-5DP", "-4DP", "-3DP",
         "-2DP", "-1DP", "media",
         "+1DP", "+2DP", "+3DP","+4DP","+5DP")

grid.table(c(dp_menos5,dp_menos4,dp_menos3,dp_menos2,dp_menos1,media,dp_mais1,dp_mais2,dp_mais3,dp_mais4,dp_mais5),names1)


# -----------------------------------------------------
# Resumo
missing3dp=c()
for(i in 1:length(data_brasil$VALVALOR)){
  # +ou- 3 dp
  if(data_brasil$VALVALOR[1]<dp_menos3 && data_brasil$VALVALOR[1]>dp_menos3)
    missing3dp[1]=1
  else{
    missing3dp[1]=0
  }
}



# -----------------------------------------------------
# -----------------------------------------------------
# Para mais tarde utilizar com tecnicas de textmining
# -----------------------------------------------------
# -----------------------------------------------------
data1=ipeadata('BPC_defi', type = "data.table")


get_metadata('BPC_defi', type = "data.table")
# -----------------------------------------------

# -----------------------------------------------
# Conteudo de pesquisa
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
