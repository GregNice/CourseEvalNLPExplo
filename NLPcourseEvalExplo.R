#load up all the packages ----
library(readr)
library(dplyr)
library(tm)
library(qdap)
library(tidyr)
library(ngram)
library(SnowballC)
library(wordcloud)
library(plotrix)
library(stringr)

#Prepare the course evaluation data, starting with F16 ----
#Read in F16 course eval responses, keep term name, banner id, and text responses as F16CourseEval ----

F16CourseEvals <- read_csv(".csv")
F16CE <- F16CourseEvals %>%
  select(TermName = term_name, StudId = bannerid, like = starts_with("Q4"), dislike = starts_with("Q5"), gencomm = starts_with("Q6")) %>%
  filter(!is.na(StudId)) %>%
  
#Reshape F16 CourseEvals by transposing comments by id and uniting together all the text columns into a single column----
  
  gather(key = "question",
         value = "text",
         -c(TermName, StudId)) %>%
  select(-question) %>%
  group_by(StudId) %>%
  mutate(IdReviewIndex = rank(StudId, ties.method = "first")) %>%
  spread(IdReviewIndex, text) %>%
  unite( text, 3:38, sep = " ", remove = TRUE)

#Repeat for F17 course eval responses ----
#Note:read_csv was being a dick reading NULL values, read problem vars to characters

F17CourseEvals <- read_csv(".csv", col_types = cols(BannerID = col_integer(), 'First Name_1' = col_character(), 'Last Name_1' = col_character(), FilloutDate = col_character()))
F17CE <- F17CourseEvals %>%
  select(TermName = term_name, StudId = BannerID, like = starts_with("Q4_Which aspects"), dislike = starts_with("Q5_What suggestions"), gencomm = starts_with("Q6_Please comment")) %>%
  filter(!is.na(StudId)) %>%
  
#Reshape F17 CourseEvals by transposing comments by id and uniting together all the text columns into a single column ----

  gather(key = "question",
       value = "text",
       -c(TermName, StudId)) %>%
  select(-question) %>%
  group_by(StudId) %>%
  mutate(IdReviewIndex = rank(StudId, ties.method = "first")) %>%
  spread(IdReviewIndex, text) %>%
  unite( text, 3:56, sep = " ", remove = TRUE)

# Now that the CE data is in good shape, let's prep the retention dataset ----
RetentionData <- read_csv(".csv")

#Create tbl F16Cohorts for eponymous cohort roster and retention flags----
F16Cohorts <- RetentionData %>%
  select(TermName, StudId, Retained) %>%
  filter(TermName == "Fall 2016",
        !is.na(Retained))

#Create tbl F17Cohorts for eponymous cohort roster and retention flags----
F17Cohorts <- RetentionData %>%
  select(TermName, StudId, Retained) %>%
  filter(TermName == "Fall 2017",
         !is.na(Retained))

#inner_join CE & Cohort F16 
F16CohortCE <- inner_join(F16CE, F16Cohorts, by = "StudId") %>%
  select(TermName = TermName.x, StudId, Retained, text)
  
#what's the ratio of non-retainer's to retainer's? 
F16CohortCE %>% 
  group_by(Retained) %>%
  summarise(n())

#inner_join CE & Cohort F17 
F17CohortCE <- inner_join(F17CE, F17Cohorts, by = "StudId") %>%
  select(TermName = TermName.x, StudId, Retained, text)

#what's the ratio of non-retainer's to retainer's? 
F17CohortCE %>% 
  group_by(Retained) %>%
  summarise(n())

#let's put the two CohortCE datasets together
AllCohortCE <- union(F16CohortCE, F17CohortCE)

#Get the tbl in the DataFrameSource shape - unique docID, Text, Meta----
##combine term and studid into one column
AllCohortCE_uniqueId <- AllCohortCE %>%
  ungroup() %>%
  unite(id_term, c(StudId, TermName), remove = FALSE)

#Get into DFS shape
AllCohortCE_uniqueId_preDFS <- AllCohortCE_uniqueId %>%
  select(doc_id = id_term, text, Retained, StudId, TermName)


#Remove illegal characters from text 
AllCohortCE_uniqueId_preDFS$text <- iconv(AllCohortCE_uniqueId_preDFS$text, 'latin1', 'UTF-8', sub = ' ')

#Create source objects and volatile corpus for NLP!
AllCohortCE_dfs <- DataframeSource(AllCohortCE_uniqueId_preDFS)
AllCohortCE_vc <- VCorpus(AllCohortCE_dfs)

#Run the tm preprocessing functions on the VCorpus
AllCohort_vc_clean <- tm_map(AllCohortCE_vc, removeWords, c(stopwords(kind = "en"), "D/A", "NA", "class", "course", "the"))
AllCohort_vc_clean <- tm_map(AllCohort_vc_clean, removePunctuation)
AllCohort_vc_clean <- tm_map(AllCohort_vc_clean, content_transformer(tolower))
##//AllCohort_vc_clean <- tm_map(AllCohort_vc_clean, stemDocument)//

#Now create two corpora, one for retain, one for not
retain_index <- meta(AllCohort_vc_clean, "Retained") == '1'
retain <- AllCohort_vc_clean[retain_index]
notRetain <- AllCohort_vc_clean[-retain_index]

#check a text record in each
retain[[1]][1]
notRetain[[1]][1]

#One mo' 'gain: check out one of the texts
content(retain[[123]])
content(notRetain[[123]])

#Basic Frequency Analyses 1-grams----
#Create TermDocumentMatrix
AllStudentDocsTDM <- TermDocumentMatrix(AllCohort_vc_clean)
AllStudentDocsTDM_m <- as.matrix(AllStudentDocsTDM)

#inspect TermDocumentMatrix
dim(AllStudentDocsTDM_m)
AllStudentDocsTDM_m[100:105, 100:105]

#Top 10 most frequent terms
FreqTerms_TDM_m <- rowSums(AllStudentDocsTDM_m) %>%
  sort(decreasing = TRUE)
barplot(FreqTerms_TDM_m[1:10], col = "blue", las = 2)

#Basic WordCloud
AllStudentDocsTDM_df <- data.frame(term = names(FreqTerms_TDM_m), num = FreqTerms_TDM_m)
wordcloud(AllStudentDocsTDM_df$term, AllStudentDocsTDM_df$num, max.words = 50, colors = "red")

#Comparison Cloud ----
#convert the cleansed corpus with all retention types back to a dataframes
Retained_clean_df <- as_data_frame(retain, validate = TRUE)
NotRetained_clean_df <- as_data_frame(notRetain, validate = TRUE)

#combine all docs in each corpora
Retained_clean_df_textVec <- paste(Retained_clean_df$text, collapse = " ")
NotRetained_clean_df_textVec <- paste(NotRetained_clean_df$text, collapse = " ")

#combine each corpora into a single vector
allRetainStatuses_clean_v <- c(Retained_clean_df_textVec, NotRetained_clean_df_textVec)
allRetainStatuses_clean_vs <- VectorSource(allRetainStatuses_clean_v)
allRetainStatuses_clean_vc <- VCorpus(allRetainStatuses_clean_vs)
allRetainStatuses_TDM <- TermDocumentMatrix(allRetainStatuses_clean_vc)

#convert to matrix
allRetainStatuses_TDM_m <- as.matrix(allRetainStatuses_TDM)

#print commonality cloud
commonality.cloud(allRetainStatuses_TDM_m, colors = "steelblue1", max.words = 100)

#comparison cloud
colnames(allRetainStatuses_TDM) <- c("Retained", "NotRetained")
allRetainStatuses_TDM_m_colnames <- as.matrix(allRetainStatuses_TDM)
# Create comparison cloud
comparison.cloud(allRetainStatuses_TDM_m_colnames, 
                 colors = c("orange", "blue"), 
                 max.words = 50)

#Polarized tag plot-----
#Remind ourselves where we're at
allRetainStatuses_TDM_m_colnames[150:155, ]

##Keep common terms and calculate the differences in fequencies, take top 25
top25Terms_df <- allRetainStatuses_TDM_m_colnames %>%
  # Convert to data frame
  as_data_frame(rownames = "word") %>% 
  # Keep rows where word appears in both documents
  filter_all(all_vars(. > 0)) %>%
  # Get difference in counts
  mutate(difference = abs(Retained - NotRetained)) %>% 
  # Arrange by descending difference
  top_n(25, wt = difference) %>%
# Keep rows with biggest difference
  arrange(desc(difference))

#Create Pyramid Plot
pyramid.plot(
  #Retained Count
  top25Terms_df$Retained,
  #Not Retained Count
  top25Terms_df$NotRetained,
  #labels and styling
  labels = top25Terms_df$word,
  top.labels = c("retained", "Word", "not retained"),
  main = "Words in Common",
  unit = NULL,
  gap = 200
)

#Do all this shit again, using bigrams----
#Load Rweka package
library(RWeka)

#make tokenizer function
tokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}

#create trigram_dtm
bigram_dtm <- DocumentTermMatrix(AllCohort_vc_clean, control = list(tokenize = tokenizer))

#create trigram_dtm_m
bigram_dtm_m <- as.matrix(bigram_dtm)

#create freq
freq <- colSums(bigram_dtm_m)

#create bi_words
bi_words <- names(freq)

#Examine part of bi_words
str_subset(bi_words, "^she")

#plot a wordcloud
wordcloud(bi_words, freq, max.words = 25)

#Create unigram TDMs, with tfidf: term frequency inverse document frequency-----
#Create TDM and TDM_m
uni_tfIdf_tdm <- TermDocumentMatrix(AllCohort_vc_clean, control = list(weighting = weightTfIdf))
uni_tfIdf_tdm_m <- as.matrix(uni_tfIdf_tdm)

#inspect TermDocumentMatrix
dim(uni_tfIdf_tdm_m)
uni_tfIdf_tdm_m[100:105, 100:105]

#Top 10 terms scored with tfIdf
FreqTerms_uni_tfIdf_tdm_m <- rowSums(uni_tfIdf_tdm_m) %>%
  sort(decreasing = TRUE)
barplot(FreqTerms_uni_tfIdf_tdm_m[1:10], col = "blue", las = 2)

#Basic WordCloud
uni_tfIdf_tdm_m_df <- data.frame(term = names(FreqTerms_tfIdf_tdm_m), num = FreqTerms_tfIdf_tdm_m)
wordcloud(tfIdf_tdm_m_df$term, tfIdf_tdm_m_df$num, max.words = 20, colors = "red")

#Comparison cloud
#Create a TDM and matrix with the allRetainStatuses_clean_VC, with the tfIdf and trigram control parameters
allRetainStatuses_tfIdf_TDM <- TermDocumentMatrix(allRetainStatuses_clean_vc, control = list(weighting = weightTfIdf))
allRetainStatuses_tfIdf_TDM_m <- as.matrix(allRetainStatuses_tfIdf_TDM)

#print commonality cloud
commonality.cloud(allRetainStatuses_tfIdf_TDM_m, colors = "steelblue1", max.words = 10)

#comparison cloud
colnames(allRetainStatuses_tfIdf_TDM) <- c("Retained", "NotRetained")
allRetainStatuses_tfIdf_TDM_m_colnames <- as.matrix(allRetainStatuses_tfIdf_TDM)
# Create comparison cloud
comparison.cloud(allRetainStatuses_tfIdf_TDM_m_colnames, 
                 colors = c("orange", "blue"), 
                 max.words = 25)

#Polarized tag plot-----
#Remind ourselves where we're at
allRetainStatuses_tfIdf_TDM_m_colnames[150:155, ]

#Keep common terms and calculate the differences in fequencies, take top 25
top25Terms_tfIdf_df <- allRetainStatuses_tfIdf_TDM_m_colnames %>%
  # Convert to data frame
  as_data_frame(rownames = "word") %>% 
  # Keep rows where word appears in both documents
  filter_all(all_vars(. > 0)) %>%
  # Get difference in counts
  mutate(difference = abs(Retained - NotRetained)) %>% 
  # Arrange by descending difference
  top_n(25, wt = difference) %>%
  # Keep rows with biggest difference
  arrange(desc(difference))

#Create Pyramid Plot
pyramid.plot(
  #Retained Count
  top25Terms_df$Retained,
  #Not Retained Count
  top25Terms_df$NotRetained,
  #labels and styling
  labels = top25Terms_df$word,
  top.labels = c("retained", "Word", "not retained"),
  main = "Words in Common",
  unit = NULL,
  gap = 200
)

#using tfIdf, polygrams don't seem appropriate for the data set -- too much information reduction, perhaps?

