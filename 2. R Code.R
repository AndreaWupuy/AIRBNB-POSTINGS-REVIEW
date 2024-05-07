#Andrea Wupuy#################################################
##############################################################

##############################################################
#Libraries####################################################
##############################################################

#installing and loading the mongolite library to download the Airbnb data
#install.packages("mongolite") #need to run this line of code only once and then you can comment out
#install.packages("textcat")
#install.packages("cld3")
#install.packages("ggraph")
library(cld3)
library(dplyr)
library(ggraph)
library(ggplot2)
library(igraph)
library(janeaustenr)
library(mongolite)
library(radarchart)
library(reshape2)
library(scales)
library(stringr)
library(textcat)
library(textdata)
library(textTinyR)
library(tidyr)
library(tidytext)
library(tidytuesdayR)
library(tm)


##############################################################
#Importing Data###############################################
##############################################################

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://awupuy:flavia2001@atlascluster.o3rlbqr.mongodb.net/?retryWrites=true&w=majority&appName=AtlasCluster'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)
#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory
airbnb_all <- airbnb_collection$find()
colnames(airbnb_all)[5] <- "text"


##############################################################
#Languages####################################################
##############################################################

# As there are several languages, we will create a column to detect the language
detected_languages <- cld3::detect_language(airbnb_all$text)
#language_identification <- textcat(airbnb_all$text)
#language_frequencies<-table(language_identification)

#Now we join in to the df
airbnb_all$language <- detected_languages

# View the updated dataframe
head(airbnb_all)

##Ploting the languages
ggplot(airbnb_all, aes(x = as.character(language))) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3, color = "black") + # Add data point labels +
  labs(x = "Language", y = "Count") +
  ggtitle("Distribution of Languages") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#4203 out of 5555 reviews are in english (76%) so for the analysis we will only remain with that scope.

#Filtering the df by listings that have english language
airbnb_all_english <- airbnb_all %>%
  filter(language == "en")


##############################################################
#EDA##########################################################
##############################################################

##Country
ggplot(airbnb_all_english, aes(x = address$country)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3, color = "black") + # Add data point labels +
  labs(x = "Country", y = "Count") +
  ggtitle("Distribution of Countries")

##Room types
ggplot(airbnb_all_english, aes(x = room_type)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3, color = "black") + # Add data point labels +
  labs(x = "Room Type", y = "Count") +
  ggtitle("Distribution of Room Types")

##Property types
ggplot(airbnb_all_english, aes(x = property_type)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Property Type", y = "Count") +
  ggtitle("Distribution of Property Types")

#Top Property Types
#Calculate counts for each property type
property_type_counts <- table(airbnb_all_english$property_type)
# Sort property types by count in descending order
sorted_property_types <- names(sort(property_type_counts, decreasing = TRUE))
# Select top 3 property types
top_3_property_types <- sorted_property_types[1:3]
# Filter dataframe to include only top 5 property types
airbnb_top_3 <- airbnb_all_english[airbnb_all_english$property_type %in% top_3_property_types, ]
# Create a bar chart of top 5 property types
ggplot(airbnb_top_3, aes(x = factor(property_type, levels = top_3_property_types))) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3, color = "black") + # Add data point labels +
  labs(x = "Property Type", y = "Count") +
  ggtitle("Top 3 Property Types")

#Number of reviews vs Review Scores Rating - Scatter plot  
#Calculate average review score rating
avg_rating <- mean(airbnb_all_english$review_scores$review_scores_rating, na.rm = TRUE)
# Filter out rows where review_scores_rating is not null
filtered_data <- airbnb_all_english[!is.na(airbnb_all_english$review_scores$review_scores_rating), ]
# Create scatter plot
ggplot(airbnb_all_english, aes(x = number_of_reviews, y = review_scores$review_scores_rating)) +
  geom_point() +
  geom_hline(yintercept = avg_rating, linetype = "dashed", color = "red") +
  labs(x = "Number of Reviews", y = "Review Scores Rating") +
  ggtitle("Scatter Plot of Number of Reviews vs Review Scores Rating")

#Price vs Review Scores Rating - Scatter plot  
#Find the maximum price
max_price <- max(filtered_data$price)
# Create scatter plot
ggplot(filtered_data, aes(x = price, y = review_scores$review_scores_rating)) +
  geom_point() +
  geom_hline(yintercept = avg_rating, linetype = "dashed", color = "red") +
  geom_vline(xintercept = max_price, linetype = "dotted", color = "blue") +  # Adding a vertical line at the maximum price
  labs(x = "Price", y = "Review Scores Rating") +
  ggtitle("Scatter Plot of Price vs Review Scores Rating")

##############################################################
#1-Word Frequency#############################################
##############################################################
frequencies_tokens <- airbnb_all_english %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
print(frequencies_tokens)

freq_hist <- airbnb_all_english %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  mutate(word=reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

#Let's see what is in the middle
freq_hist2 <- airbnb_all_english %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 700 & n < 1000) %>%
  mutate(word = reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
print(freq_hist2)

#DTM
airbnb_all_dtm <- airbnb_all_english %>%
  unnest_tokens(word, text) %>%
  count(property_type, word) %>%
  cast_dtm(property_type, word, n)

airbnb_all_dtm
#Sparsity of 93%, is good because is high

##############################################################
#Frequency####################################################
##############################################################
airbnb_all_token <- airbnb_all_english %>%
  unnest_tokens(word, text) %>%
  count(property_type, word, sort=TRUE) %>%
  ungroup()

total_words <- airbnb_all_token %>%
  group_by(property_type) %>%
  summarize(total=sum(n))

airbnb_all_words <- left_join(airbnb_all_token, total_words)%>%
  filter(property_type %in% c("Apartment", "House", "Condominium"))

print(airbnb_all_words)

ggplot(airbnb_all_words, aes(n/total, fill = property_type))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~property_type, ncol=2, scales="free_y")

##############################################################
#ZIPF's law###################################################
##############################################################

freq_by_rank <- airbnb_all_words %>%
  group_by(property_type) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=property_type))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################
airbnb_tf_token <- airbnb_all_english %>%
  unnest_tokens(word, text) %>%
  count(property_type, word, sort=TRUE) %>%
  ungroup()

total_words <- airbnb_tf_token %>%
  group_by(property_type) %>%
  summarize(total=sum(n))

airbnb_all_token_words <- left_join(airbnb_tf_token, total_words)%>%
  filter(property_type %in% c("Apartment", "House", "Condominium"))

airbnb_words <- airbnb_all_token_words %>%
  bind_tf_idf(word, property_type, n)
airbnb_words 

airbnb_words %>%
  arrange(desc(tf_idf))
#now we get the best tokens

# looking at the graphical approach:
airbnb_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(property_type) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=property_type))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~property_type, ncol=2, scales="free")+
  coord_flip()

##############################################################
#Tidy: Property Type##########################################
##############################################################
#Creating a tidy format for Apartment
apt <- airbnb_all_english %>%
  filter(property_type== "Apartment")

tidy_apt <- apt %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_apt)

#Creating a tidy format for House
house <- airbnb_all_english %>%
  filter(property_type== "House")

tidy_house <- house %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_house)

#Creating a tidy format for Condominium
condo <- airbnb_all_english %>%
  filter(property_type== "Condominium")

tidy_condo <- condo %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_condo)

#We want to combine all the datasets and do frequencies 
frequency <- bind_rows(mutate(tidy_apt, author="Apartment"),
                       mutate(tidy_house, author= "House"),
                       mutate(tidy_condo, author="Condo")
)%>% #closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `House`, `Condo`)

#let's plot the correlograms:
ggplot(frequency, aes(x=proportion, y=`Apartment`, 
                      color = abs(`Apartment`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Apartment", x=NULL)


##############################################################
#Bigram Frequency#############################################
##############################################################
airbnb_all_bigrams <-  airbnb_all_english %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

airbnb_all_bigrams #We want to see the bigrams (words that appear together, "pairs")

airbnb_all_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
bigrams_separated <- airbnb_all_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  filter(property_type %in% c("Apartment", "House", "Condominium")) %>% 
  count(property_type, bigram) %>%
  bind_tf_idf(bigram, property_type, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf


# Arrange the data frame in descending order of TF-IDF
bigram_tf_idf <- bigram_tf_idf %>%
  arrange(desc(tf_idf))

# Subset the data to include only the top 15 bigrams by TF-IDF value for each property type
top_15_bigrams <- bigram_tf_idf %>%
  group_by(property_type) %>%
  top_n(15) %>%
  ungroup()

# Convert the bigram column to a factor and reverse the levels
top_15_bigrams <- top_15_bigrams %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))

# Create the ggplot
ggplot(top_15_bigrams, aes(bigram, tf_idf, fill = property_type)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") +
  facet_wrap(~property_type, ncol = 2, scales = "free") +
  coord_flip()


##############################################################
#Bigram Network#############################################
##############################################################
bigram_graph <- bigram_counts %>%
  filter(n>100) %>%
  graph_from_data_frame()

bigram_graph

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


##############################################################
#Quadro-gram Frequency########################################
##############################################################
quadrogram <- airbnb_all_english %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>% 
  count(word1, word2, word3,word4, sort = TRUE)

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  filter(property_type %in% c("Apartment", "House", "Condominium")) %>% 
  count(property_type, quadrogram) %>%
  bind_tf_idf(quadrogram, property_type, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf

#Top 20 Quadrograms
top_20_quadrograms <- head(quadrogram_tf_idf, 20)
ggplot(top_20_quadrograms, aes(x = reorder(quadrogram, tf_idf), y = tf_idf, fill = property_type)) +
  geom_bar(stat = "identity", position = "dodge") +
    labs(title = "TF-IDF of Quadrograms by Property Type",
       x = "Quadrogram",
       y = "TF-IDF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################
#Quadrogram Network###########################################
##############################################################
quadrogram_graph <- quadrogram_counts %>%
  filter(n>6) %>%
  graph_from_data_frame()

quadrogram_graph

ggraph(quadrogram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


##############################################################
#Sentiment####################################################
##############################################################
airbnb_all_token <- airbnb_all_english %>%
  unnest_tokens(word, text)

#Now we will check the sentiments that match the analysis
sentiments <- get_sentiments("nrc")
unique_values <- unique(sentiments$sentiment)
print(unique_values)
#We are going to use "trust" because we are analyzing descriptions so this can help us determine how trusworthy are those descriptions.

nrctrust <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")

#Inner joining Apartment 
airbnb_all_token %>%
  filter(property_type== "Apartment") %>%
  inner_join(nrctrust) %>%
  count(word, sort=T)

apartment <- airbnb_all_token %>%
  filter(property_type== "Apartment")

afinn <- apartment %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  apartment%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  apartment%>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")
#afinn: 30,000+ / bing:20,000+ / NRC= 25,000+

#Inner joining House 
airbnb_all_token %>%
  filter(property_type== "House") %>%
  inner_join(nrctrust) %>%
  count(word, sort=T)

house <- airbnb_all_token %>%
  filter(property_type== "House")

afinn <- house %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  house%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  house %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")
#afinn: 6,000+ / bing:3,000+ / NRC= 4,000+

#Inner joining Condominium
airbnb_all_token %>%
  filter(property_type== "Condominium") %>%
  inner_join(nrctrust) %>%
  count(word, sort=T)

condo <- airbnb_all_token %>%
  filter(property_type== "Condominium")

afinn <- condo %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  condo%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  condo %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")
#afinn: 4,000+ / bing:2,000+ / NRC= 3,000+


##############################################################
#NRC Analysis#################################################
##############################################################
# NRC Analysis
nrc <- lexicon_nrc()

# Perform Inner Join with the tokenized Airbnb data and the NRC lexicon
nrcSent <- inner_join(airbnb_all_token_words, nrc, by = c('word')) %>%
  # Focus only on the three property types of interest
  filter(property_type %in% c("Apartment", "House", "Condominium")) %>%
  # Drop positive/negative sentiments to focus only on emotions
  filter(!sentiment %in% c("positive", "negative"))

# Create a contingency table of sentiment by property type
nrcSentRadar <- table(nrcSent$sentiment, nrcSent$property_type)

# Convert to a data frame for plotting
plotDF <- as.data.frame.matrix(nrcSentRadar)
plotDF <- cbind(Emotion = rownames(plotDF), plotDF)
rownames(plotDF) <- NULL

# Melt the data frame into a long format expected by chartJSRadar
plotDF_long <- melt(plotDF, id.vars = "Emotion")

# Now, let's draw the radar chart
chartJSRadar(plotDF)


##############################################################
#Exporting to CSV#############################################
##############################################################

#Checking for duplicates
duplicates <- airbnb_all[duplicated(airbnb_all$listing_url), ]
# Show the duplicate values
print(duplicates)

#Creating a df with listing url of english subset to use for Tableau.
#This will be used as a filter for the csv given.
df_listing_url <- airbnb_all_english$listing_url
df_listing_url <- airbnb_all_english[, "listing_url", drop = FALSE]

#Setting working directory and exporting csv
setwd("~/Andrea Wupuy/HULT/Estudios/13. Spring Term 2024/Unstructured Datat")
write.csv(df_listing_url, file = "df_listing_url.csv", row.names = FALSE)

#Exporting Bigram & Quadrogram
write.csv(bigram_tf_idf, file = "bigram_tf_idf.csv", row.names = FALSE)
write.csv(quadrogram_tf_idf, file = "quadrogram_tf_idf.csv", row.names = FALSE)






