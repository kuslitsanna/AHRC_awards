library(pacman)

p_load("seededlda",
       "quanteda",
       "dplyr",
       "tidyverse",
      )

#keeping the top 80% of features that appear in less then 50% of all documents

dfmat_abstracts <- dfm(clean_abstracts) %>% 
  dfm_trim(min_termfreq = 0.2, termfreq_type = "quantile",
           max_docfreq = 0.5, docfreq_type = "prop")


tmod_lda <- textmodel_lda(dfmat_abstracts, k = 20, max_iter=100)

lda1 <- terms(tmod_lda, 10)%>% as.data.frame()

tmod_lda2 <- textmodel_lda(dfmat_abstracts, k = 25, max_iter=100)

lda2 <- terms(tmod_lda2, 10)%>% as.data.frame()

tmod_lda3 <- textmodel_lda(dfmat_abstracts, k = 25, max_iter=300)

lda3 <- terms(tmod_lda3, 10) %>% as.data.frame()

tmod_lda4 <- textmodel_lda(dfmat_abstracts, k = 30, max_iter=300)

lda4 <- terms(tmod_lda4, 10)%>% as.data.frame()

tmod_lda5 <- textmodel_lda(dfmat_abstracts, k = 30, max_iter=200, alpha = 0.8, beta = 0.05)

lda5 <- terms(tmod_lda5, 15)%>% as.data.frame()

tmod_lda6 <- textmodel_lda(dfmat_abstracts, k = 30, max_iter=300, alpha = 1, beta = 0.01)

lda6 <- terms(tmod_lda6, 15)%>% as.data.frame()

save.image(file = "backup_workspace_unseeded_model_1.RData")

# remove everything except needed objects 

rm(list=setdiff(ls(),c("clean_abstracts",
                       "docvars")))

cat("\014")  

save.image(file = "backup_workspace_unseeded_model_2.RData")


