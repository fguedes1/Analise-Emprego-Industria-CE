library(tidyverse)

# Importando os dados da RAIS
dados = read.delim2('C:/Users/Fillipe Guedes/Downloads/RAIS_VINC_PUB_NORDESTE.txt',
                    header = T, sep = ';')
head(dados)

# Selecionando apenas os dados do CE
dados$UF = substr(dados$Município, 1,2)
dados_CE = dados %>%
  filter(UF == '23')
rm(dados)

saveRDS(dados_CE, 'C:/Users/Fillipe Guedes/OneDrive/Teste FIEC/Base/DADOS_CE.RDs')

dados_CE = read_rds('C:/Users/Fillipe Guedes/OneDrive/Teste FIEC/Base/DADOS_CE.RDs')

# Filtrando apenas 31-12
dados_CE = dados_CE %>%
  filter(Vínculo.Ativo.31.12 == 1)

dados_CE = dados_CE %>%
  filter(Vl.Remun.Dezembro.Nom>0)

# Filtrando a Ind de Calcados
dados_CE_calcados = dados_CE %>%
  filter(str_detect(CNAE.2.0.Classe, '^153'))

#dados_CE_calcados = dados_CE_calcados %>%
#  filter(!str_detect(CNAE.2.0.Classe, '^1539'))

# Vinculos Por Sexo
dados_CE_calcados = dados_CE_calcados %>%
  mutate(Sexo.Trabalhador_NEW = ifelse(Sexo.Trabalhador == 1, 'Masculino', 'Feminino'))



# Municipios
library(readxl)
RELATORIO_DTB_BRASIL_MUNICIPIO <- read_excel("C:/Users/Fillipe Guedes/OneDrive/Teste FIEC/Base/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

RELATORIO_DTB_BRASIL_MUNICIPIO =  RELATORIO_DTB_BRASIL_MUNICIPIO %>%
  select(`Código Município Completo`, Nome_Município)
RELATORIO_DTB_BRASIL_MUNICIPIO$`Código Município Completo` = substr(RELATORIO_DTB_BRASIL_MUNICIPIO$`Código Município Completo`, 1,6)
names(RELATORIO_DTB_BRASIL_MUNICIPIO)[1] = 'Município'

dados_CE_calcados$Município = as.character(dados_CE_calcados$Município)

dados_CE_calcados = left_join(dados_CE_calcados, RELATORIO_DTB_BRASIL_MUNICIPIO)

mun = dados_CE_calcados %>%
  count(Nome_Município)

mun = mun %>%
  arrange(desc(n))

mun = mun %>% 
  mutate(pct = prop.table(n))

mun_TOP10 = mun %>%
  slice(1:5)

mun_TOP10 = mun_TOP10 %>%
  mutate(pct = pct * 100)

mun_TOP10$pct = round(mun_TOP10$pct, 1)

mun_TOP10 %>% 
  ggplot(aes(x=reorder(Nome_Município,n), y=n)) +
  geom_col(fill = 'light blue', color = 'black')+ 
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.5, size = 5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) +
  coord_flip() + 
  labs(title = '',
       subtitle = '',
       caption = '',
       y = 'Total de Vínculos Formais',
       x = '') + 
  ylim(0,15000) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "plain"))

  

######### SALARIOS
dados_CE_calcados_rem = dados_CE_calcados %>%
  group_by(Sexo.Trabalhador_NEW, Nome_Município) %>%
  summarise(Rem_Media= mean(Vl.Remun.Dezembro.Nom))

nomes = mun_TOP10$Nome_Município

dados_CE_calcados_rem = dados_CE_calcados_rem %>%
  filter(Nome_Município %in% nomes)

dados_CE_calcados_rem_2 = dados_CE_calcados %>%
  group_by(Nome_Município) %>%
  summarise(Rem_Media= mean(Vl.Remun.Dezembro.Nom))

dados_CE_calcados_rem_2 = dados_CE_calcados_rem_2 %>%
  filter(Nome_Município %in% nomes)

dados_CE_calcados_rem_3 = dados_CE_calcados %>%
  summarise(Rem_Media= mean(Vl.Remun.Dezembro.Nom))

dados_CE_calcados_rem_4 = dados_CE_calcados %>%
  group_by(Sexo.Trabalhador_NEW) %>%
  summarise(Rem_Media= mean(Vl.Remun.Dezembro.Nom))

############## OCUPACOES
ocups = dados_CE_calcados %>%
  count(CBO.Ocupação.2002)

ocups = ocups %>%
  arrange(desc(n))


ocups = ocups %>% 
  mutate(pct = prop.table(n))

ocups_top4 = ocups %>%
  slice(1:4)


ocups_top4 = ocups_top4 %>%
  mutate(pct = pct * 100)

ocups_top4$pct = round(ocups_top4$pct,1)

ocups_top4[1,"CBO.Ocupação.2002"] = 'Trabalhador polivalente da confecção de calçados'
ocups_top4[2,"CBO.Ocupação.2002"] = 'Costurador de calçados, a máquina'
ocups_top4[3,"CBO.Ocupação.2002"] = 'Alimentador de linha de produção'
ocups_top4[4,"CBO.Ocupação.2002"] = 'Preparador de calçados'


ocups_top4 %>% 
  ggplot(aes(x=reorder(CBO.Ocupação.2002,n), y=n)) +
  geom_col(fill = 'light blue', color = 'black')+ 
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.5, size = 5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) +
  coord_flip() + 
  labs(title = '',
       subtitle = '',
       caption = '',
       y = 'Total de Vínculos Formais',
       x = '') + 
  ylim(0,30000) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "plain"))


######### SALARIOS OCUP
ocups = dados_CE_calcados %>%
  count(CBO.Ocupação.2002)

ocups = ocups %>%
  arrange(desc(n))


ocups = ocups %>% 
  mutate(pct = prop.table(n))

ocups_top4 = ocups %>%
  slice(1:4)

nomes = ocups_top4$CBO.Ocupação.2002

dados_CE_calcados_rem_ocup = dados_CE_calcados %>%
  filter(CBO.Ocupação.2002 %in% nomes)

dados_CE_calcados_rem_2 = dados_CE_calcados_rem_ocup %>%
  group_by(CBO.Ocupação.2002, Sexo.Trabalhador_NEW) %>%
  summarise(Rem_Media= mean(Vl.Remun.Dezembro.Nom))

dados_CE_calcados_rem_2 = dados_CE_calcados_rem_ocup %>%
  group_by(CBO.Ocupação.2002) %>%
  summarise(Rem_Media= mean(Vl.Remun.Dezembro.Nom))


##########

dados_CE_calcados = dados_CE_calcados %>%
  mutate(ESCOLARIDADE = ifelse(Escolaridade.após.2005<5, 'Fundamental Incompleto',
                               ifelse(Escolaridade.após.2005==5, 'Fundamental Completo',
                                      ifelse(Escolaridade.após.2005==6, 'Médio Incompleto',
                                             ifelse(Escolaridade.após.2005==7|Escolaridade.após.2005==8, 'Médio Completo',
                                                    ifelse(Escolaridade.após.2005>8, 'Superior Completo', 'a'
                               ))))))


escol = dados_CE_calcados %>% 
  count(ESCOLARIDADE)

escol = escol %>%
  arrange(desc(n))

escol = escol %>%  
  mutate(pct = prop.table(n))

escol = escol %>%
  mutate(pct = pct * 100)

escol$pct = round(escol$pct,1)


escol %>% 
  ggplot(aes(x=reorder(ESCOLARIDADE,n), y=n)) +
  geom_col(fill = 'light blue', color = 'black')+ 
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.5, size = 5,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) +
  coord_flip() + 
  labs(title = '',
       subtitle = '',
       caption = '',
       y = 'Total de Vínculos Formais',
       x = '') + 
  ylim(0,40000) +
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "plain"))


