#Carregar pacotes necessários
library(data.table)   # Para manipulação de dados
library(knitr)        
library(dplyr)        # Para manipulação de dados
library(tidyr)        # Para manipulação de dados
library(PNADcIBGE)    # Para carregar dados da PNAD Contínua
library(tidyverse)    # Para manipulação de dados
library(ggplot2)      # Para visualização de dados
library(stargazer)    # Para gerar tabelas de regressão
library(tidyverse)    # Para manipulação de base de dados
library(broom)        # Para converter regressões em base de dados
library(data.table)   # Para gerar data frames
library(randomizr)    # Para randomização
library(RCT)          # Para análise de ensaios clínicos randomizados
library(xtable)       # Para gerar tabelas em LaTeX
library(MatchIt)      # Pacote para matching
library(cobalt)       # Para balanceamento de covariáveis
library(lfe)          # Para regressões com efeitos fixos
set.seed(2024) # Definindo uma semente para reprodutibilidade


## Leitura e processamento dos dados da PNAD Contínua 2024

#A biblitoeca pnadcIBGE permite baixar os dados da PNAD Contínua diretamente do site do IBGE. A seguir, são baixados os dados dos quatro trimestres de 2024 e combinados em uma única base de dados.

pnadc20241tri <- get_pnadc(year = 2024, quarter = 1, design = FALSE)
pnadc20242tri <- get_pnadc(year = 2024, quarter = 2, design = FALSE)
pnadc20243tri <- get_pnadc(year = 2024, quarter = 3, design = FALSE)
pnadc20244tri <- get_pnadc(year = 2024, quarter = 4, design = FALSE)

#Combinando os trimestres em uma única base de dados
#pnadc2024 <- rbind(pnadc20241tri, pnadc20242tri, pnadc20243tri, pnadc20244tri) # O rbind() funciona bem desde que os data.frames tenham exatamente as mesmas colunas, na mesma ordem.

pnadc2024 <- bind_rows(pnadc20241tri, pnadc20242tri, pnadc20243tri, pnadc20244tri) #bind_rows() alinha as colunas pelo nome, mesmo que a ordem seja diferente, e insere NA se alguma variável estiver ausente em um dos data frames.



## Data wrangling

#Tratando os dados

#table(pnadc2024c$ANOS_ESTUDO, useNA = "ifany") # dica --> para verificar a distribuição de uma variável 

pnadc2024c = pnadc2024 %>%
  mutate(
    UPA = as.numeric(UPA), #
    V1008 = as.numeric(V1008), #Número do domicílio
    V1014 = as.numeric(V1014), #Grupo da amostra
    V2003 = as.numeric(V2003),
    CODIGO = UPA * 1000 + V1008 * 10 + V1014, # código do domicílio
    CODPES = UPA *1000 + V1008 *100 + V1014 *10 + V2003, # código da pessoa
    UF_char = as.character(UF),  # converte o fator em string
    cod_uf = recode(UF_char,
                    "Rondônia" = 11,
                    "Acre" = 12,
                    "Amazonas" = 13,
                    "Roraima" = 14,
                    "Pará" = 15,
                    "Amapá" = 16,
                    "Tocantins" = 17,
                    "Maranhão" = 21,
                    "Piauí" = 22,
                    "Ceará" = 23,
                    "Rio Grande do Norte" = 24,
                    "Paraíba" = 25,
                    "Pernambuco" = 26,
                    "Alagoas" = 27,
                    "Sergipe" = 28,
                    "Bahia" = 29,
                    "Minas Gerais" = 31,
                    "Espírito Santo" = 32,
                    "Rio de Janeiro" = 33,
                    "São Paulo" = 35,
                    "Paraná" = 41,
                    "Santa Catarina" = 42,
                    "Rio Grande do Sul" = 43,
                    "Mato Grosso do Sul" = 50,
                    "Mato Grosso" = 51,
                    "Goiás" = 52,
                    "Distrito Federal" = 53))%>%
  rename(
    CLT = V4029, # contrato CLT
    RPM = V403412, # renda mensal
    FAIXA_SM =  V403411, # faixa de salário mínimo
    DIA_NASC = V2008, # dia de nascimento,
    MES_NASC = V20081, # mês de nascimento
    ANO_NASC = V20082, # ano de nascimento
    OCUPADO = V4001, # ocupado
    ANOS_ESTUDO = VD3005,
    IDADE = V2009,
    SEXO = V2007,
    HORAS_TRABALHO = V4039
  )%>%
  
  #transformando anos de estudo em numérico
  mutate(
    ANOS_ESTUDO = as.numeric(ANOS_ESTUDO), # convertendo ANOS_ESTUDO para numérico
    ANOS_ESTUDO = (ANOS_ESTUDO -1),# subtraindo 1 para ajustar a escala
  )%>%
  
  #removendo NA e transformando a horas de estudo semanal em mensal
  drop_na(HORAS_TRABALHO)%>% # Retirando valores faltantes de HORAS_TRABALHO
  mutate(
    HORAS_TRABALHO = as.numeric(HORAS_TRABALHO), # convertendo HORAS_TRABALHO para numérico
    HORAS_TRABALHO = HORAS_TRABALHO*4 # multiplicando por 4 para obter o total mensal
  )%>%
  filter(OCUPADO == "Sim")%>% # Filtrando apenas os ocupados
  
  #Criando binárias de região
  mutate(
    NORDESTE = ifelse(UF %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia"), 1, 0),
    NORTE = ifelse(UF %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins"), 1, 0),
    SUDESTE = ifelse(UF %in% c("Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo"), 1, 0),
    SUL = ifelse(UF %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul"), 1, 0),
    CENTRO_OESTE = ifelse(UF %in% c("Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"), 1, 0))%>%
  
  #Criando outras binárias
  mutate(
    CLT = ifelse(CLT == "Sim", 1, 0), # Convertendo CLT para numérico
    MULHER = ifelse(SEXO == "Mulher", 1, 0), # Convertendo Mulher para numérico
    NAO_BRANCO = ifelse(V2010 != "Branca", 1, 0), # Convertendo Não Branco para numérico
    
    # Proxy da experiência: no Brasil normalmente as crinaças iniciam sua alfabetização aos 6 anos. Nesse caso, para construtir proxy da experiência, temos:
    EXPER = IDADE - ANOS_ESTUDO - 6,
    EXPER = if_else(EXPER < 0, 0, EXPER) # Garantindo que a experiência não seja negativa
  ) %>%
  
  # Filtrando apenas os indivíduos com idade entre 18 e 60 anos
  filter(
    IDADE >=18 & IDADE <= 60 # Filtrando apenas indivíduos com idade entre 18 e 60 anos
  )%>%
  drop_na(CLT)


## Propensity Score Matching (PSM)

### Selecionando as variaveis de interesse
psm_clt = pnadc2024c %>%
  select(UF, NORDESTE, NORTE, SUDESTE, SUL, CENTRO_OESTE, CLT, RPM, FAIXA_SM, HORAS_TRABALHO, IDADE, EXPER, ANOS_ESTUDO, MULHER, NAO_BRANCO, OCUPADO)

psm_clt = as.data.frame(psm_clt) # Convertendo para data.table para melhor performance


#Estatísticas Descritivas
stargazer(psm_clt, 
          type = "text",
          title = "Estatísticas Descritivas")


### 1 - Teste de  balanceamento (filtrando apenas as variáveis do modelo) antes do PSM
df_balance_antes = psm_clt %>%
  select(NORDESTE, NORTE, SUDESTE, SUL, CENTRO_OESTE, CLT, RPM, HORAS_TRABALHO, IDADE, ANOS_ESTUDO, MULHER, NAO_BRANCO) 


teste_bal_antes = balance_table(df_balance_antes, "CLT")

teste_bal_antes = teste_bal_antes%>%
  mutate(
    p_value1 = round(p_value1, 7)
  )

teste_bal_antes
print(teste_bal_antes, row.names = FALSE)
#exporta para latex
#print.xtable(xtable(teste_bal_antes, longtable = TRUE))





### 2 - Estimando o Propensity Score

#Para estimar o escore de propensão, é necessário realizar uma regressão logística (logit) onde a variável dependente é o tratamento (CLT) e as variáveis independentes são as covariáveis selecionadas. O escore de propensão é a probabilidade de um indivíduo receber o tratamento (ter carteira assinada) dado suas características observáveis.
pscore_fs = glm(CLT ~ HORAS_TRABALHO + IDADE + ANOS_ESTUDO + MULHER + NAO_BRANCO + NORDESTE + NORTE + SUL + CENTRO_OESTE, family = binomial(link = logit), data= psm_clt)
summary(pscore_fs)

### 3 - PSM - Modelo 1: Pareamento por vizinho mais próximo
psm1 = matchit(CLT ~ HORAS_TRABALHO + IDADE + ANOS_ESTUDO + MULHER + NAO_BRANCO + NORDESTE + NORTE + SUL + CENTRO_OESTE, data = psm_clt, method = "nearest", ratio = 1, discard = "both")

#summary(psm1)
plot(psm1, type = "hist") #histrograma dos escores de propensão antes e após

#Love plot (gráfico de balanço das covariáveis)
love.plot(psm1, stats = "mean.diffs", threshold = 0.1) #Mostra a diferença média padronizada entre os grupos tratado e controle antes e depois do pareamento.

df_matched1 = match.data(psm1) # Extraindo os dados pareados





### 3 - PSM - Modelo 2: Pareamento por vizinho mais próximo limitado ao caliper de 0.25**
psm2 = matchit(CLT ~ HORAS_TRABALHO + IDADE + ANOS_ESTUDO + MULHER + NAO_BRANCO + NORDESTE + NORTE + SUL + CENTRO_OESTE, data = psm_clt, method = "nearest", ratio = 1, caliper = 0.25, discard = "both")


#summary(psm1)
plot(psm2, type = "hist") #histrograma dos escores de propensão antes e após


#Love plot (gráfico de balanço das covariáveis)
love.plot(psm2, stats = "mean.diffs", threshold = 0.1) #Mostra a diferença média padronizada entre os grupos tratado e controle antes e depois do pareamento.

df_matched2 = match.data(psm2) # Extraindo os dados pareados




### 3 - PSM - Modelo 3: Pareamento por vizinho mais próximo limitado ao caliper de 0.005**
psm3 = matchit(CLT ~ HORAS_TRABALHO + IDADE + ANOS_ESTUDO + MULHER + NAO_BRANCO + NORDESTE + NORTE + SUL + CENTRO_OESTE, data = psm_clt, method = "nearest", ratio = 1, caliper = 0.005, discard = "both", )

#summary(psm1)
plot(psm3, type = "hist") #histrograma dos escores de propensão antes e após

#Love plot (gráfico de balanço das covariáveis)
love.plot(psm3, stats = "mean.diffs", threshold = 0.1) #Mostra a diferença média padronizada entre os grupos tratado e controle antes e depois do pareamento.

df_matched3 = match.data(psm3) # Extraindo os dados pareados



### 4 - Testes de balanceamento após o pareamento
df_balance1 = df_matched1 %>%
  select(NORDESTE, NORTE, SUDESTE, SUL, CENTRO_OESTE, CLT, RPM, HORAS_TRABALHO, IDADE, ANOS_ESTUDO, MULHER, NAO_BRANCO)


df_balance2 = df_matched2 %>%
  select(NORDESTE, NORTE, SUDESTE, SUL, CENTRO_OESTE, CLT, RPM, HORAS_TRABALHO, IDADE, ANOS_ESTUDO, MULHER, NAO_BRANCO)


df_balance3 = df_matched3 %>%
  select(NORDESTE, NORTE, SUDESTE, SUL, CENTRO_OESTE, CLT, RPM, HORAS_TRABALHO, IDADE, ANOS_ESTUDO, MULHER, NAO_BRANCO)


#Gera a tabela com médias dos grupos de trabalhadores tratados (CLT) e não tratados após o pareamento

teste_bal1 = balance_table(df_balance1, "CLT")
teste_bal1 = teste_bal1%>%
  mutate(
    p_value1 = round(p_value1, 7)
  )
teste_bal1



teste_bal2 = balance_table(df_balance2, "CLT")
teste_bal2 = teste_bal2%>%
  mutate(
    p_value1 = round(p_value1, 7)
  )
teste_bal2


teste_bal3 = balance_table(df_balance3, "CLT")
teste_bal3 = teste_bal3%>%
  mutate(
    p_value1 = round(p_value1, 7)
  )
teste_bal3


## 5 - Regressão

#Ao invés de realizar um simples teste de médias, utiliza-se a regressão para estimar o impacto.

#Utilizando as bases de dados pareados, podemos estimar a mesma regressão linear sob os diferentes critérios:
  
  
#Modelo0: regressão com a base original (sem pareamento)
#Modelo1: regressão com a base pareada pelo vizinho mais próximo sem caliper
#Modelo2: regressão com a base pareada pelo vizinho mais próximo com caliper de 0.25
#Modelo3: regressão com a base pareada pelo vizinho mais próximo com caliper de 0.005


reg0 = lm( RPM ~ CLT + HORAS_TRABALHO + IDADE + I(IDADE^2) + ANOS_ESTUDO + MULHER + NAO_BRANCO + NORDESTE + NORTE + SUL + CENTRO_OESTE, data = psm_clt)

reg1 = lm( RPM ~ CLT + HORAS_TRABALHO + IDADE + I(IDADE^2)  + ANOS_ESTUDO + MULHER + NAO_BRANCO + NORDESTE + NORTE + SUL + CENTRO_OESTE, data = df_matched1)

reg2 = lm( RPM ~ CLT + HORAS_TRABALHO + IDADE + I(IDADE^2) + ANOS_ESTUDO + MULHER + NAO_BRANCO + NORDESTE + NORTE + SUL + CENTRO_OESTE, data = df_matched2)

reg3 = lm( RPM ~ CLT + HORAS_TRABALHO + IDADE + I(IDADE^2) + ANOS_ESTUDO + MULHER + NAO_BRANCO + NORDESTE + NORTE + SUL + CENTRO_OESTE, data = df_matched3)


#exportar os resultados 
  stargazer(reg0, reg1, reg2, reg3,
            type = "text",
            title = "Resultados das Regressões",
            dep.var.labels = "Renda Pessoal Mensal (RPM)",
            omit.stat = c("f", "ser"),
            no.space = TRUE,
            add.lines = list(c("Observações", nrow(psm_clt), nrow(df_matched1), nrow(df_matched2), nrow(df_matched3))))

