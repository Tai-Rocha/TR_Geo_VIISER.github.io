#################################################################
## VII Seminário Internacional de Estatística com R (SER)
## Manipulação e estatística básica de dados geoespaciais no R
## Autora: Tainá Rocha
## 24 - 26 Maio 2023
################################################################

# Pacotes/bibliotecas


library(corrgram)     # Gráficos para matriz de correlação
library(dplyr)        # Manipulação de dataframe / tibble (estruturas tabulares)  
library(geobr)        # Conjuntos de dados espaciais oficiais do Brasil
library(ggplot2)      # Visualização de dados 
library(ggthemes)     # Temas para visualização de dados  
library(raster)       # Manipulação de dados matriciais (raster)
library(RColorBrewer) # Paleta de cores para visualização de dados
library(readr)        # Leitura/carregamento de tabelas (csv, tsv, fwf)
library(sf)           # simple feature manipulação de dados espaciais
library(stats)        # Funções para cálculos estatísticos e geração de números aleatórios
library(stringr)      # Manipulação (limpeza) de strings
library(tidyr)        # Manipulação de dataframe / tibble (estruturas tabulares)  
library(tmap)         # Edição e visualização de mapas
library(viridis)      # Paleta de cores para visualização de dados


################################################################################################################################
# Estudo de caso 1: Espacialização e visualização de dados de supressão da vegetação ao longo dos anos usando dados do MapBiomas (https://mapbiomas.org/estatisticas)
################################################################################################################################


## Limites estaduais brasileiros (geobr) 

br_states = read_state(code_state = "all", year=2019)

## Classe do objeto br_states 

class(br_states)

## Nomes das variáveis (colunas) 

names(br_states)


# Visualização de dados (ggplo2)

# Datavis 1
ggplot(br_states) + 
  geom_sf(aes(fill = name_region)) 

## Datavis 2
ggplot(br_states) + 
  geom_sf(aes(fill = name_region)) + 
  labs(fill="Região")

## Datavis 3

legenda = "Regiões"
ggplot(br_states) + 
  geom_sf(aes(fill = name_region)) +   
  scale_fill_manual(legenda, values=c("orange","red", "blue", "gray", "yellow"))


## Limites municipais brasileiros (geobr) 

br_municipios = read_municipality(code_muni = "all", year=2020, simplified = TRUE)


## Limites biomas brasileiros (geobr) e limpeza de NAs (stats)

bioma = geobr::read_biomes(year = 2019,  simplified = TRUE) |> 
  stats::na.omit()
  
## Lendo/carregando o arquivo csv com os dados do Mapbiomas | readr 

mapbiomas = read_csv("data-raw/TABELA_GERAL_COL7_MAPBIOMAS_DESMAT_VEGSEC_UF_Muni_Biome_aba.csv") 


## Manipulação do objeto mapbiomas | dplyr, tidyr, stats

mapbiomasmuni = mapbiomas |>  
  tidyr::separate(city, into = c("city", "state", "biome"), sep = " - ") |> # Separando a coluna 'city' em três novas colunas
  dplyr::rename("name_muni" = city) |> # Renomeando a coluna city para name_muni
  dplyr::filter(dr_class_name == c("Supressão Veg. Primária"))  |>  # Filtrando as linhas de "Supressão Veg. Primária"
  dplyr::select(-color) |> # Removendo a coluna color 
  tidyr::pivot_longer(cols = starts_with(c("19", "20")), names_to = "year", values_to = "hectare") |>  # Transformando os anos de colunas para linhas
  dplyr::group_by(YEAR = year, name_muni) |> # Agrupando municípios por anos
  dplyr::summarize(Ha = sum(hectare)) |> # Somando o hectare 
  dplyr::mutate_at('YEAR', as.numeric)  |> # Transformando a coluna YEAR em numérica
  stats::na.omit()  # Removendo os NAs


## Unindo o objeto "mapbiomasmuni" com o objeto "br_municipios" | dplyr

mapmuniJOIN= left_join(mapbiomasmuni, br_municipios, by = "name_muni", multiple = "all") |> 
  st_as_sf()

## Visualizando o resultado final | dplyr, ggplot2

mapmuniJOIN |>
  # Filtrando para o recorte temporal de 2013 a 2020
  filter(YEAR >= 2013 & YEAR <= 2020) |>  
  ggplot() +
  # Adicionando o contorno dos biomas
  geom_sf(data = bioma, color = "gray", fill = NA, size = 0.1)  +
  # Definindo hectare como a variável a ser mostrada
  geom_sf(aes(fill = Ha)) +
  # Adicionando escala de cor viridis
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Facets por ano
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "_",
       #subtitle = "1997-2000",
       fill = "Ha")  +
  theme_void()

# Limpando o Environment e Plots

rm(list=ls()) 
dev.off(dev.list()["RStudioGD"])

################################################################# 
# Estudo de caso 2 - Estatísticas básicas para a Precipitação BR
################################################################# 

# Dados de precipitação do wordlim (precipitation (mm) 5 minutes)- https://www.worldclim.org/data/worldclim21.html#google_vignette


# Listando os arquivos necessários | R-base 
precip_list = list.files("data-raw/wc2.1_5m_prec", pattern = ".tif", full.names = T)

# Criando stack, ou seja, uma coleção com estes arquivos. Necessitam ter mesma extensão e resolução | raster
precip_stack = stack(precip_list)
plot(precip_stack)


# Carregando dado vetorial com o limite do Brasil | geobr

br = read_country(year = 2010, simplified = TRUE, showProgress = TRUE)

# Cortando a variável para o Brasil com base no objeto "br"

mask_prec_bioma = mask(x = precip_stack, mask = br) # aplicando a máscara (shape) pela função mask do pacote raster 
plot(mask_prec_bioma) # plote 

bioma_prec = crop(x = mask_prec_bioma, y = extent(br)) #agora corte por essa máscara
plot(bioma_prec)

## Estatísticas básicas | raster

# Média 
Mean_Prec = calc(bioma_prec, fun = mean)
plot(Mean_Prec)

# SD
SD_Prec = calc(bioma_prec, fun = sd)
plot(SD_Prec)

# Soma 
Sum_Prec = calc(bioma_prec, fun = sum)
plot(Sum_Prec)

# Pontos aletórios

randon =  sampleRandom(Mean_Prec, size=500, cells=TRUE, sp=TRUE)

# Extraindo os valores dos pixels a partir de dos pontos

envs_values_in_points = raster::extract(precip_stack, randon@coords)

# Matriz de correlação

matix_correlation = corrgram(envs_values_in_points, lower.panel=panel.pts, upper.panel=panel.cor,diag.panel=panel.density,cor.method="spearman", pch=18, main="Correlation matrix") 
