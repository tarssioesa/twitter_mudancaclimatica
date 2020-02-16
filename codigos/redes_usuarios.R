# Descrição 

# Este código cria as primeiras redes de usuários que se referem
# a #chuvarj. Foi utilizado para fazer uma primeira limpeza dos
# dados, excluindo aqueles que não possuem relação com o estudo


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
                          "@apoyo_croqueton", "@apamdif", "@pedfig", "@martinezpmsc")) %>%
  dfm_trim(min_termfreq = 10)

tag_dfm <- dfm_select(all_dfm, ('#*'))
toptag <- names(topfeatures(tag_dfm, 50))
head(toptag,50)

tag_fcm <- fcm(tag_dfm, tri = F)

toptag_fcm <- fcm_select(tag_fcm, toptag)


png("redes1.png", width=3200, height=1800, res = 300)

textplot_network(toptag_fcm, min_freq =0.1, edge_alpha = 0.8, edge_size = 2, 
                 vertex_labelsize = 4,vertex_labelcolor = "blue", 
                 edge_color = "orange")
dev.off()

# @ all

user_dfm <- dfm_select(all_dfm, ('@*'))
topuser <- names(topfeatures(user_dfm, 50))
head(topuser,50)

user_fcm <- fcm(user_dfm, tri = F)

topuser_fcm <- fcm_select(user_fcm, topuser)


png("user1.png", width=3200, height=1800, res = 300)

textplot_network(topuser_fcm, min_freq =0.1, edge_alpha = 0.8, edge_size = 2, 
                 vertex_labelsize = 4,vertex_labelcolor = "blue", 
                 edge_color = "orange")
dev.off()


# Bairros ---- 

bairros_ <- text %>% 
  filter(str_detect(value, bairros)) %>% 
  filter(!str_detect(value, "lollapalooza"))

bairros_corpus <- quanteda::corpus(bairros_$value)

bairros_dfm <- dfm(bairros_corpus, remove_punct = TRUE,
               remove = c(quanteda::stopwords("portuguese"),
                          "t.co", "https", "rt", "amp", "http", "t.c", "can", "u",
                          "vc", "?", "pra", "nessa", "ta", "existe", "pode",
                          "levar", "vai", "coisa", "todos", "ent?o", "puder", 
                          "dando", "pq", "porra", "qualquer", "nao", "estao", 
                          "todo", "so", "aqui", "serve", "menos", "ja", 
                          "conta", "durante", "sao")) %>%
  dfm_trim(min_termfreq = 3)

#topfeatures(bairros_dfm, 25)

png("bairros.png", width=6400, height=1800, res = 300)


textplot_wordcloud(bairros_dfm, adjust = TRUE, max_words = 50, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"), 
                   random_order = FALSE, min_size = 0.75, fixed_aspect = TRUE)
dev.off()

# ABRIGO ---- 

casa <- c("lar", "casa", "barraco", "favela", "apartamento", "condominio")

lar <- text %>% 
  filter(str_detect(value, casa))

lar_corpus <- quanteda::corpus(lar$value)

lar_dfm <- dfm(lar_corpus, remove_punct = TRUE,
                  remove = c(quanteda::stopwords("portuguese"),
                             "t.co", "https", "rt", "amp", "http", "t.c", "can", "u",
                             "vc", "?", "pra", "nessa", "ta", "existe", "pode",
                             "levar", "vai", "coisa", "todos", "ent?o", "puder", 
                             "dando", "pq", "porra", "qualquer", "voce", "nao",
                             "so", "saco", "la", "devido"))
topfeatures(lar_dfm, 25)

png("casa.png", width=6400, height=1800, res = 300)


textplot_wordcloud(lar_dfm, adjust = TRUE, max_words = 50, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"), 
                   random_order = FALSE, min_size = 0.75, fixed_aspect = TRUE)
dev.off()


# Desastre ---- 

desastre <- c("alagamento", "alagou", "enchente", "deslizamento", "soterramento", 
              "arvore", "tempestade", "raio", "temporal", "enchentes")

desastre <- text %>% 
  filter(str_detect(value, desastre))

desastre_corpus <- quanteda::corpus(desastre$value)

desastre_dfm <- dfm(desastre_corpus, remove_punct = TRUE,
                  remove = c(quanteda::stopwords("portuguese"),
                             "t.co", "https", "rt", "amp", "http", "t.c", "can", "u",
                             "vc", "?", "pra", "nessa", "ta", "existe", "pode",
                             "levar", "vai", "coisa", "todos", "ent?o", "puder", 
                             "dando", "pq", "porra", "qualquer", "el", "la", 
                             "y", "en", "durante", "es","una", "sea", "un",
                             "tudo", "t?", "ver", "the", "nada", "falando", 
                             "gente", "nao", "voce", "so"))

topfeatures(desastre_dfm, 25)

png("desastre.png", width=6400, height=1800, res = 300)


textplot_wordcloud(desastre_dfm, adjust = TRUE, max_words = 50, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"), 
                   random_order = FALSE, min_size = 0.75, fixed_aspect = TRUE)
dev.off()


# morbidade ---- 

morb <- c("mortes", "morto", "morte", "hospital", "ferido", "afogado", "soterrado")

morb <- text %>% 
  filter(str_detect(value, morb))

morb_corpus <- quanteda::corpus(morb$value)

morb_dfm <- dfm(morb_corpus, remove_punct = TRUE,
                    remove = c(quanteda::stopwords("portuguese"),
                               "t.co", "https", "rt", "amp", "http", "t.c", "can", "u",
                               "vc", "?", "pra", "nessa", "ta", "existe", "pode",
                               "levar", "vai", "coisa", "todos", "entao", "puder", 
                               "dando", "pq", "porra", "qualquer", "el", "la", 
                               "y", "en", "durante", "es","una", "sea", "un",
                               "tudo", "ta", "ver", "the", "nada", "falando", 
                               "gente", "nao", "agbbtrjhn3", "diz", "10", "ser"))


textplot_wordcloud(morb_dfm, adjust = TRUE, max_words = 50, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"), 
                   random_order = FALSE, min_size = 0.75, fixed_aspect = TRUE)


topfeatures(morb_dfm, 25)
