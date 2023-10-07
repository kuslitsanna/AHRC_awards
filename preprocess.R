library(pacman)

p_load("seededlda",
       "quanteda",
       "quanteda.textstats",
       "openxlsx",
       "dplyr",
       "tidyverse",
       "lexicon")


corpus <- read.xlsx("gtr_scraper/text_for_topic_modeling.xlsx")
corpus <- corpus %>% mutate("docid_field"=ProjectReference, 
                            "text_field"=AllText,
                            .keep="none") 
corp <- corpus(corpus, docid_field = "docid_field", text_field = "text_field")

summary(corp)

docvars <- read.xlsx("gtr_scraper/master_data.xlsx") %>% 
  dplyr::select("ProjectReference", "Title", "StartDate", "EndDate",
                "AwardPounds", "LeadROName", "Region") %>%
  mutate(StartDate = as.Date(StartDate, origin = "1899-12-30"), EndDate = as.Date(EndDate, origin = "1899-12-30"))
docvars <- docvars %>% mutate(LeadROName=as.factor(LeadROName), 
                              Region=as.factor(Region)
                              )

#tokenise and remove stopwords and punctuation

toks_abstracts <- tokens(corp, remove_punct = TRUE, padding = TRUE) %>% tokens_remove(stopwords("en"), padding = TRUE)

?tokens

head(toks_abstracts)

#lemmatise

lemma_abstracts <- tokens_replace(toks_abstracts, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>% 
  tokens_split(separator = "'s$", valuetype = "regex", remove_separator = TRUE)

#create compound tokens from proper names

prop_names <- tokens_select(lemma_abstracts, 
                               pattern = "^[A-Z]",
                               valuetype = "regex",
                               case_insensitive = FALSE, 
                               padding = TRUE)

tstat_prop_names <- textstat_collocations(prop_names, min_count = 6, tolower = FALSE)

head(tstat_prop_names)
tail(tstat_prop_names)

lemma_abstracts_names_comp <- tokens_compound(lemma_abstracts, 
                             pattern = tstat_prop_names[tstat_prop_names$z > 3,], 
                             case_insensitive = FALSE)

#convert to lower case

lemma_abstracts_names_comp <- tokens_tolower(lemma_abstracts_names_comp, keep_acronyms = TRUE)

#create compound tokens from frequent expressions (n-grams)

tstat_ngrams <- textstat_collocations(lemma_abstracts_names_comp, min_count = 150)
head(tstat_ngrams, 20)
tail(tstat_ngrams, 20)

lemma_abstracts_ngrams_names_comp <- tokens_compound(lemma_abstracts_names_comp,
                                                     pattern = tstat_ngrams)

#examine most frequent tokens

dfmat <- dfm(lemma_abstracts_ngrams_names_comp)


topfeatures(dfmat, 100)

#create list of custom stopwords from most frequent tokens that are too vague to aid analysis

custom_stopwords <- c("", "project", "research", "work", "use", "new", "also",
                      "can", "good", "make", "study", "way", "within", "aim",
                      "focus", "across", "key", "create", "one", "may", "two",
                      "issue", "role")

clean_abstracts <- tokens_remove(lemma_abstracts_ngrams_names_comp, 
                                 pattern = custom_stopwords)



save.image(file = "backup_workspace_preprocess_1.RData")

# remove everything except needed objects 

rm(list=setdiff(ls(),c("clean_abstracts",
                       "docvars")))

cat("\014")  

save.image(file = "backup_workspace_preprocess_2.RData")
