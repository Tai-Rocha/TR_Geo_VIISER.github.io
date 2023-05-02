#################################################################
## VII Seminário Internacional de Estatística com R (SER)
## Manipulação e estatística básica de dados geoespaciais no R
## Autora: Tainá Rocha
################################################################

# Estudo de caso 1 : Espacialização e vizualização de dados demstamento ao longo dos anos

# Pacotes/bibliotecas a s

library(dplyr) # Manipualação de dataframes 
library(geobr) # 
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(readr)
library(sf)
library(stringr)
library(tidyr)
library(tmap)
library(viridis)



## Limites estaduais brasileiros

br_states = geobr::read_state(code_state = "all", year=2019)

## Classe do objeto br_states 

class(br_states)

## Nomes das variáveis (colunas) 

names(br_states)


## Visualização de dados 

# Dataviz 1
ggplot2::ggplot(br_states) + 
  ggplot2::geom_sf(ggplot2::aes(fill = name_region)) 

## Dataviz 2

ggplot2::ggplot(br_states) + 
  ggplot2::geom_sf(ggplot2::aes(fill = name_region)) + 
  ggplot2::labs(fill="Região")

## Dataviz 3

legenda = "Regiões"
ggplot2::ggplot(br_states) + 
  ggplot2::geom_sf(ggplot2::aes(fill = name_region)) +   
  ggplot2::scale_fill_manual(legenda, values=c("orange","red", "blue", "gray", "yellow"))


## Limites municipais brasileiros

br_municipios = geobr::read_municipality(code_muni = "all", year=2020, simplified = TRUE)


## Limites biomas brasileiros

bioma = geobr::read_biomes(year = 2019,  simplified = TRUE) |> 
  stats::na.omit()
  

## Read mapbiomas data 

mapbiomas = readr::read_csv("data-raw/TABELA_GERAL_COL7_MAPBIOMAS_DESMAT_VEGSEC_UF_Muni_Biome_aba.csv") 


# split the 'city' column into three separate columns
mapbiomasmuni = mapbiomas |> 
  tidyr::separate(city, into = c("city", "state", "biome"), sep = " - ") |>
  dplyr::rename("name_muni" = city) |> 
  dplyr::filter(dr_class_name == c("Supressão Veg. Primária"))  |> 
  dplyr::select(-color) |>
  tidyr::gather(year, hectare, starts_with("19"), starts_with("20")) |> 
  dplyr::group_by(YEAR = year, name_muni) |> 
  dplyr::summarize(Ha = sum(hectare)) |> 
  dplyr::mutate_at('YEAR', as.numeric)  |> 
  stats::na.omit()


## Join geobr and mapbiomasmuni 

mapmuniJOIN= left_join(mapbiomasmuni, br_municipios, by = "name_muni", multiple = "all") |> 
  st_as_sf()


mapmuniJOIN |>
  filter(YEAR >= 2013 & YEAR <= 2020) |> 
  ggplot() +
  # Add the biomes lines contours
  geom_sf(data = bioma, color = "gray", fill = NA, size = 0.1)  +
  # Add the kgLiquido per municipalities
  geom_sf(aes(fill = Ha)) +
  # Add the legend for the kgLiquido scale
  scale_fill_viridis_c(option = "B", direction = -1) +
  # Add the facet wrap for each year
  facet_wrap(~YEAR, ncol = 4) +
  labs(title = "_",
       #subtitle = "1997-2000",
       fill = "Ha")  +
  theme_void()

# Estudo de caso 2 - Precipitação BR

# Dados de precipitação do wordlim (precipitation (mm) 5 minutes)- https://www.worldclim.org/data/worldclim21.html#google_vignette

library(corrgram)
library(sf) # simple feature uma maneira padronizada de codificar dados vetoriais espaciais 
library(raster)
library(rgdal)
library(terra)          
library(ggplot2)   
library(dplyr) # manipulação de dataframes
library(ggplot2) # visualização de dados
library(sf) # simple feature uma maneira padronizada de codificar dados vetoriais espaciais


precip_list = list.files("data-raw/wc2.1_5m_prec", pattern = ".tif", full.names = T)

precip_stack = stack(precip_list)
plot(precip_stack)


### Mask Crop  Solar Radiation

br = geobr::read_country(year = 2010, simplified = TRUE, showProgress = TRUE)

mask_prec_bioma = mask(x = precip_stack, mask = br) #aplicando a máscara (shape) pela função mask do pacote raster 
plot(mask_prec_bioma) # plote 

bioma_prec = crop(x = mask_prec_bioma, y = extent(br)) #agora corte por essa máscara
plot(bioma_prec)

## Stats

Mean_Prec = mean(bioma_prec)
plot(Mean_Prec)


## random points 

randon =  sampleRandom(Mean_Prec, size=500, cells=TRUE, sp=TRUE)

envs_values_in_points <- raster::extract(precip_stack,
                                         randon@coords,method="bilinear")

matix_correlation <- corrgram(envs_values_in_points, lower.panel=panel.pts, upper.panel=panel.cor,diag.panel=panel.density,cor.method="spearman", pch=18, main="Correlation matrix") 
