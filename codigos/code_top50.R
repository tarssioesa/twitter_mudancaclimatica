# Retorna os 50 usuarios mais citados na internet depois da limpeza
# feita em redes_usuarios.R

# Quanteda

library(quanteda)
library(tidytext)
library(tidyverse)
library(stringi)

# opt ----

opt <- theme_bw()+
  theme(axis.title = element_text(face = "bold", color = "black", size = 20),
        axis.text.x = element_text(face = "plain", color = "black", 
                                   size = 18, angle = 90),
        axis.text.y = element_text(face = "plain", color = "black", size = 18),
        legend.text=element_text(size=20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        strip.text.x = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))

#chuvaRJ

load("chuvarj.RData")

range(chuvarj$created_at)

bairros <- read.delim2("bairros.txt")

bairros <- stri_trans_general(bairros$Bairro, "Latin-ASCII") %>% 
  tolower()


# Range 

range(chuvarj$created_at)


text <- chuvarj$text %>% 
  tolower() %>% 
  as.tibble() %>% 
  mutate(value = stri_trans_general(value, "Latin-ASCII"))

# Hashtag all

  
all <- quanteda::corpus(text$value)
  
  all_dfm <- dfm(all, remove_punct = TRUE,
                 remove = c(quanteda::stopwords("portuguese"),
                            "t.co", "https", "rt", "amp", "http", "t.c", "can", "u",
                            "vc", "?", "pra", "nessa", "ta", "existe", "pode",
                            "levar", "vai", "coisa", "todos", "ent?o", "puder", 
                            "dando", "pq", "porra", "qualquer", "nao", "estao", 
                            "todo", "so", "aqui", "serve", "menos", "ja", 
                            "conta", "durante", "sao", "#btsinbangkokday2", 
                            "#weathercloud", "#boywithluv", "#eldebatedeverdad", 
                            "#boywithluvteaser2", "#venezuela", "#tercadetremurasdv",
                            "#ecpoliticasinfluyentes", "#puertolacruz", "#anzoategui", 
                            "#serviciopublico","#lollabrnomultishow", 
                            "#obrasparaelchaco",
                            "#educacionchaco", "#fimdoforoprivilegiado", 
                            "#lulalivre", "#bbb19","#80tiros", "#vidasnegrasimportam", 
                            "#alvarodias", "#difundir", "#welcometobraziltwentyonepilots", 
                            "@bts_twt", "@veranoelia", "@carolinapinoc", "@irene_montero_", 
                            "@animalparaiso", "@blanquitamontt", "@canelapali", "@marianimalovers",
                            "@lawera33", "@salvandopatitas", "@perrososmx" , "@refugiodhermes",
                            "@tonkatp", "@perdidogsmx", "@paraperros", "@gm5mascotas", 
                            "@por_perros", "@animalitoscdmx", "@yaz_animalista","@kattykowaleczko",
                            "@ppencontradosmx", "@mascotassismo", "@_patriciaanaya", 
                            "@apoyo_croqueton", "@apamdif", "@pedfig", "@martinezpmsc", 
                            "@uol", "@uolesporte", "@uolnoticias", 
                            "@balbithiago", "@coronelrochase", "@apfcristiano",
                            "@bolsonarosp", "@sergiusdourado", "@srgioaparecid19",
                            "@allantercalivre", "@foxalisson", "@ary_antipt", "@lollapaloozabr",
                            "@niteroiradar", "@youtube", "@patriaamada_br", "@santosfc",
                            "@cleberamaral_", "@romquentin", "@leonardogcohen"
                            )) %>%
    dfm_trim(min_termfreq = 10)
  
  tag_dfm <- dfm_select(all_dfm, ('@*'))
  toptag <- names(topfeatures(tag_dfm, 50))
  head(toptag,50)
  
  tag_fcm <- fcm(tag_dfm, tri = F)
  
  toptag_fcm <- fcm_select(tag_fcm, toptag)
  
  textplot_network(toptag_fcm, min_freq = 10, edge_alpha = 0.8, edge_size = 2, 
                   vertex_labelsize = 4,vertex_labelcolor = "blue", 
                   edge_color = "orange")

## função para filtro

twitter_lda <- text %>%  
  filter(str_detect(value, toptag))

# Salvar

save(twitter_lda, toptag, file = "dados.RData" )


