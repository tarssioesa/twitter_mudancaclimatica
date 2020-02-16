# Quanteda

library(quanteda)
library(tidytext)
library(tidyverse)
library(stringi)
library(readxl)
library(magrittr)
library(tm)
library(lubridate)

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

{ 
load("chuvarj.RData")

ref <- read_xlsx("ref.xlsx")

bairros <- read_xlsx("Chuva.xlsx")

bairro_ref <- bairros$Bairro %>% 
  tolower() %>% 
  as.tibble()

bairro_ref <- cbind(bairro_ref, bairros$Chuva)

colnames(bairro_ref) <- c("Bairro", "Chuva")

bairros %<>% bind_rows(bairro_ref)
  
} 


# selecionando aqueles que se referem a algum bairro no twitter
# Bairros ---- 

{ 

bairros_ <- chuvarj %>% 
  mutate(bairro = str_extract(text, bairros$Bairro)) %>% 
  select(created_at, text, bairro, screen_name, hashtags) %>% 
  na.omit()

bairros_n <- bairros_ %>% 
  group_by(bairro) %>% 
  summarise(n = n()) %>% 
  left_join(bairros, by = c("bairro" = "Bairro")) %>% 
  group_by(Chuva) %>% 
  summarise(ref = sum(n)) %>%
  left_join(ref, by = c("Chuva" = "Chuva"))

# Gráfico Citações e chuva total

ggplot(bairros_n, aes(x = fct_reorder(Bairro, -Chuva), y = ref)) +
  geom_col() +
  labs(x = "Bairros ordenados pela quantidade de chuva de forma descrescente",
       y = "Porcentagem de citações") +
  opt

# ggsave(file = "twitter_bairros.jpeg", width = 40, height = 20, units = "cm", dpi = 350)
} 

# As palavras mais citadas para alguns bairros

{ 

stop_words <- stopwords(kind = "pt") %>% 
  as.tibble()

filter_bairros <- c("Barra", "barra","Copacabana", "copacabana",
                    "Rocinha", "rocinha", "Jardim Botânico", "Jardim Botanico",
                    "jardim botânico", "jardim botanico")

text_bairros <- bairros_ %>% 
  filter(bairro %in% filter_bairros) %>% 
  tidytext::unnest_tokens(word, text, token = "words") %>% 
  anti_join(stop_words, by= c("word" = "value")) %>% 
  filter(!word %in% c("rt", "https", "t.co", "zona", "pra",
                      "barra", "sul", "copacabana", 
                      "durante", "jardim", "botânico", 
                      "rocinha", "e", "kvl38rc5lh", "botanico", 
                      "é", "tá", "sendo", "fácil", 
                      "9f6gcklbsm")) %>% 
  group_by(bairro) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() %>% 
  mutate(bairro = fct_recode(bairro,
                             Barra = "barra", 
                             Copacabana = "copacabana",
                             'Jardim Botânico' = "Jardim Botanico", 
                             'Jardim Botânico' = "jardim botânico",
                             Rocinha = 'rocinha')) %>% 
  group_by(bairro) %>% 
  top_n(10, n) 


# Palavras mais citadas em cada um dos bairros
ggplot(text_bairros,aes(x = fct_reorder(word,n), y = n)) +
  geom_col() +
  facet_wrap(~bairro, ncol = 2, scales = "free_y") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Termos") +
  opt

  
  
# ggsave(file = "words_bairros.jpeg", width = 40, height = 20, units = "cm", dpi = 350)

} 

# horario com mais publicações

{ 

df <- bairros_ %>%  
  filter(bairro %in% filter_bairros) %>% 
  mutate(bairro = fct_recode(bairro,
                             Barra = "barra", 
                             Copacabana = "copacabana",
                             'Jardim Botânico' = "Jardim Botanico", 
                             'Jardim Botânico' = "jardim botânico",
                             Rocinha = 'rocinha'))

point_time <- function(df, n_word, inicio, fim){ 
  
df_hash <- df %>% 
  mutate(created_at1 = with_tz(created_at, 
                              tz = "America/Bahia")) %>% 
  mutate(date = floor_date(created_at1, "hour")) %>%
  filter(date >= inicio) %>% 
  filter(date <= fim) %>% 
  group_by(date, bairro) %>% 
  summarise(n = n()) %>%  
  arrange(date)

# Determinando inicio e fim
  
  fim <- fim + days(1)
  
  datebreaks <- seq(floor_date(inicio, "day"), 
                    floor_date(fim, "day"), by = "4 hours")
  
  
 p2 <- ggplot(df_hash, aes(x = ymd_hms(date), y = n)) +
    geom_col() +
    scale_x_datetime(breaks= datebreaks)  +
    theme_minimal() + 
    opt 
 
 p1 <- ggplot(df_hash, aes(x = ymd_hms(date), y = n)) +
   geom_col() +
   scale_x_datetime(breaks= datebreaks)  +
   theme_minimal() + 
   facet_wrap(~ bairro) +
   opt
  

# Horários de PICO]

aux <- df_hash %>% 
   group_by(bairro) %>% 
   mutate(max = max(n)) %>% 
   arrange(max) %>% 
   filter(n == max)  

}


# ggsave(file = "time_tt.jpeg", width = 40, height = 20, units = "cm", dpi = 350)



# ggsave(file = "time_bairros.jpeg", width = 40, height = 20, units = "cm", dpi = 350)
