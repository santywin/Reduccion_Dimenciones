library(rvest)
library(tm)
library(dplyr)
library(lubridate)
library(readtext)
library(tidytext)
library(readxl)
library(magrittr)
library(ggplot2)
library(stringr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(readxl)
library(patchwork)

url <- "https://www.theguardian.com/environment/2015/jan/08/mayors-failure-clean-up-londons-air-pollution-risks-childrens-health"
# Read the HTML document using try to handle 404 errors
try(html_document <- read_html(url))

print(html_document) # does not provide us the information we want. It just shows the HTML code.


# Specify the xpath content for the headline in title_xpath
# Note that SelectorGadget provides you with: //*[contains(concat( " ", @class, " " ), concat( " ", "content__headline", " " ))], which is equivalent
title_xpath <- "//h1[contains(@class, 'content__headline')]"
title_text <- html_document %>%
  html_node(xpath = title_xpath) # Only provides the node.

# In order to get the information we want, we need html_text, which extracts attributes, text and tag name from html
title_text <- title_text %>%
  html_text(trim = T) # Stores title in title_text

# Access author information (CSS)
author_css <- ".tone-colour span" # Using SelectorGadget ('.byline span' does also work)
author_text <- html_document %>%
  html_node(css = author_css) %>%
  html_text(trim = T) # Stores author in author_text

# Access article text information (XPath)
body_xpath <- "//div[contains(@class, 'content__article-body')]//p" # '.js-article__body > p' is also possible, but needs css option in html_nodes
# The above location can be found when searching for the first two words of the article in the source code (or when inspecting the first to lines of the article).
# This provides you with the location information <div class="content__article-body"<p>
body_text <- html_document %>%
  html_nodes(xpath = body_xpath) %>%
  html_text(trim = T) %>%
  paste0(collapse = "\n")

# Access publishing date information (XPath)
date_xpath <- "//time" # '.content__dateline-wpd--modified' does not work for some reason, although it is the output of SelectorGadget. 
# In such a case just try to look for alternatives witht he other methods outlined above
# to handle date information (important for later analysis including time)
date_text <- html_document %>%
  html_node(xpath = date_xpath) %>%
  html_attr(name = "datetime") %>% # accesses the attribute information datetime in //time (different from html_text above)
  as.Date() %>% 
  parse_date_time(., "ymd", tz = "UTC") 

# Store all information in a data frame called article
article <- data.frame(
  url = url,
  date = date_text,
  title = title_text,
  author = author_text,
  body = body_text
)

print(as_tibble(article))


# Articulos ---------------------------------------------------------------

stop <- tibble::enframe(stopwords("spanish"), value="word", name = NULL) %>% 
  bind_rows(tibble(word=c("mas", "si")))

elcom <- tibble::enframe(readtext("arti/elcomercio.docx")$text, value="el.com", name = NULL) %>% 
  unnest_tokens(word, el.com) %>% 
  anti_join(stop, by="word") %>% 
  count(word) %>% filter(!str_detect(word, "[:digit:]+")) %>% rename(el.com=n)

eluni <- tibble::enframe(readtext("arti/eluniverso.docx")$text, value="el.uni", name = NULL) %>% 
  unnest_tokens(word, el.uni) %>% 
  anti_join(stop, by="word") %>% 
  count(word) %>% filter(!str_detect(word, "[:digit:]+")) %>% rename(el.uni=n)

bbc <- tibble::enframe(readtext("arti/bbcnews.docx")$text, value="bbc", name = NULL) %>% 
  unnest_tokens(word, bbc) %>% 
  anti_join(stop, by="word") %>% 
  count(word) %>% filter(!str_detect(word, "[:digit:]+")) %>% rename(bbc=n)

fran24 <- tibble::enframe(readtext("arti/france24.docx")$text, value="fran.24", name = NULL) %>% 
  unnest_tokens(word, fran.24) %>% 
  anti_join(stop, by="word") %>% 
  count(word) %>% filter(!str_detect(word, "[:digit:]+")) %>% rename(fran.24=n)

infobae <- tibble::enframe(readtext("arti/infobae.docx")$text, value="inf.bae", name = NULL) %>% 
  unnest_tokens(word, inf.bae) %>% 
  anti_join(stop, by="word") %>% 
  count(word) %>% filter(!str_detect(word, "[:digit:]+")) %>% rename(info.bae=n)

nyt <- tibble::enframe(readtext("arti/newyorktimes.docx")$text, value="nyt", name = NULL) %>% 
  unnest_tokens(word, nyt) %>% 
  anti_join(stop, by="word") %>% 
  count(word) %>% filter(!str_detect(word, "[:digit:]+")) %>% rename(nyt=n)

covid <- eluni %>% 
  full_join(elcom, by="word") %>% 
  full_join(bbc, by="word") %>% 
  full_join(fran24, by="word") %>% 
  full_join(infobae, by="word") %>% 
  full_join(nyt, by="word") %>% 
  replace_na(list(el.uni=0, el.com=0, bbc=0, 
                  fran.24=0, info.bae=0, nyt=0)) %>% 
  mutate(tot=el.uni+el.com+bbc+fran.24+info.bae+nyt) %>% 
  filter(tot>=5) %>% 
  arrange(desc(tot)) %>% 
  select(-tot)  %>% 
  mutate(word=case_when(word %in% c("fallecido","muertos","muertes",
                                    "cadáveres", "cadáver", "cuerpo",
                                    "cuerpos") ~ "fallecidos",
                        word=="casas" ~ "casa",
                        word=="cifras" ~ "cifra",
                        word=="días" ~ "día",
                        word=="familiares" ~ "familia",
                        word=="hace" ~ "hacer",
                        word=="hospitales" ~ "hospital",
                        word=="levantar" ~ "levantamiento",
                        word=="semanas" ~ "semana",
                        word=="coronavirus" ~ "covid",
                        word=="nacional" ~ "país",
                        TRUE ~ word)) %>% 
  gather(key, value, -word) %>% 
  group_by(word, key) %>% summarise(value=sum(value)) %>% 
  spread(key, value, fill=0) 

readr::write_excel_csv2(covid, "data/covid.csv")

cov_mat <- as.matrix(covid[,-1])
rownames(cov_mat) <- as.matrix(covid[,1])

cov_ca <- CA(cov_mat, graph = FALSE)

p1 <- fviz_ca_row(cov_ca, repel=TRUE, geom.col = c("text"))
p2 <- fviz_ca_col(cov_ca, repel = TRUE, geom.col = "text")

p1 + p2

summary(cov_ca)

str(cov_ca)


cov_ca$col$contrib