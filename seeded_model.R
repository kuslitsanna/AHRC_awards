library(pacman)

p_load("seededlda",
       "quanteda",
       "dplyr",
       "tidyverse",
       "LDAvis"
)

#create dictionary of topics to seed

topics <- read.csv(file = "topic_library.txt", sep = ",", header = FALSE)

topic_list <- as.list(tokens(topics$V2))

names(topic_list) <- topics$V1

dict_topic <- dictionary(topic_list)

head(dict_topic)

#create dfm keeping the top 80% of features that appear in less then 50% of all documents

dfmat_abstracts <- dfm(clean_abstracts) %>% 
  dfm_trim(min_termfreq = 0.2, termfreq_type = "quantile",
           max_docfreq = 0.5, docfreq_type = "prop")

#fit model 1

slda1 <- textmodel_seededlda(dfmat_abstracts, dictionary = dict_topic, max_iter = 200)
slda1_topics <- terms(slda1, 50) %>% as.data.frame()



write.csv(slda1_topics, "slda1_topics.csv", row.names = FALSE)

head(slda1$theta)

head(topics(slda1), 15)


#fit model 2

slda2 <- textmodel_seededlda(dfmat_abstracts, dictionary = dict_topic, max_iter = 700)
slda2_topics <- terms(slda2, 20) %>% as.data.frame()

write.csv(slda2_topics, "slda2_topics.csv", row.names = FALSE)



head(topics(slda2), 15)


json1 <- createJSON(phi = slda1$phi, 
                   theta = slda1$theta, 
                   doc.length = rowSums(dfmat_abstracts), 
                   vocab = colnames(dfmat_abstracts), 
                   term.frequency = colSums(dfmat_abstracts)
)



serVis(json1)



json2 <- createJSON(phi = slda2$phi, 
                    theta = slda2$theta, 
                    doc.length = rowSums(dfmat_abstracts), 
                    vocab = colnames(dfmat_abstracts), 
                    term.frequency = colSums(dfmat_abstracts)
)

serVis(json2)

#clean workspace

rm(list=setdiff(ls(),c("clean_abstracts",
                       "docvars",
                       "dfmat_abstracts",
                       "slda1",
                       "slda1_topics",
                       "json1",
                       "slda2",
                       "slda2_topics",
                       "json2")))

cat("\014")  

save.image(file = "backup_workspace_seeded_model.RData")


