library(shiny)

# Load bigram, trigram, and quadrigram lookup tables
#setwd("C:/Users/rzhang124/Documents/GitHub/Capstone_Word_Prediction")

totalTrainBiLookup <- read.table(file = "totalTrainBiLookup.csv", header = T)
totalTrainTriLookup <- read.table(file = "totalTrainTriLookup.csv", header = T)
totalTrainQuadLookup <- read.table(file = "totalTrainQuadLookup.csv", header = T)
totalTrainUniLookup <- read.table(file = "totalTrainUniLookup.csv", header = T, stringsAsFactors = F)

# Create function for creating corpus of a text file
createCorpus <- function(x) {
    corpusFile <- Corpus(VectorSource(x))
    corpusFile <- tm_map(corpusFile, content_transformer(tolower))
    corpusFile <- tm_map(corpusFile, removeNumbers)
    corpusFile <- tm_map(corpusFile, removePunctuation)
    corpusFile <- tm_map(corpusFile, stemDocument)
    corpusFile <- tm_map(corpusFile, stripWhitespace)
    return(corpusFile)
}


library(tm); library(SnowballC)
library(RWeka)

# Create unigram tokenizer function
unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

# Set up n-gram lookup functions

# Quadrigram lookup
quadrigramLookup <- function(gram1, gram2, gram3) {
    totalLookup <- totalTrainQuadLookup[totalTrainQuadLookup$gram1 == gram1,]
    totalLookup <- totalLookup[totalLookup$gram2 == gram2,]
    totalLookup <- totalLookup[totalLookup$gram3 == gram3,]    
    totalLookup$newsPercent <- sapply(totalLookup$newsFreq, function(x) x/sum(totalLookup$newsFreq))
    totalLookup$blogPercent <- sapply(totalLookup$blogFreq, function(x) x/sum(totalLookup$blogFreq))
    totalLookup$twitterPercent <- sapply(totalLookup$twitterFreq, function(x) x/sum(totalLookup$twitterFreq))
    totalLookup$sumPercent <- rowSums(totalLookup[, c("newsPercent", "blogPercent", "twitterPercent")])
    totalLookup <- totalLookup[order(totalLookup$sumPercent, decreasing = T),]
    topTarget <- as.character(totalLookup$target[1])
    
    return(topTarget)
}


# Trigram lookup
trigramLookup <- function(gram2, gram3) {
    totalLookup <- totalTrainTriLookup[totalTrainTriLookup$gram2 == gram2,]
    totalLookup <- totalLookup[totalLookup$gram3 == gram3,]
    totalLookup$newsPercent <- sapply(totalLookup$newsFreq, function(x) x/sum(totalLookup$newsFreq))
    totalLookup$blogPercent <- sapply(totalLookup$blogFreq, function(x) x/sum(totalLookup$blogFreq))
    totalLookup$twitterPercent <- sapply(totalLookup$twitterFreq, function(x) x/sum(totalLookup$twitterFreq))
    totalLookup$sumPercent <- rowSums(totalLookup[, c("newsPercent", "blogPercent", "twitterPercent")])
    totalLookup <- totalLookup[order(totalLookup$sumPercent, decreasing = T),]
    topTarget <- as.character(totalLookup$target[1])

    return(topTarget)
}

# Bigram lookup
bigramLookup <- function(gram3) {
    totalLookup <- totalTrainBiLookup[totalTrainBiLookup$gram3 == gram3,]
    totalLookup$newsPercent <- sapply(totalLookup$newsFreq, function(x) x/sum(totalLookup$newsFreq))
    totalLookup$blogPercent <- sapply(totalLookup$blogFreq, function(x) x/sum(totalLookup$blogFreq))
    totalLookup$twitterPercent <- sapply(totalLookup$twitterFreq, function(x) x/sum(totalLookup$twitterFreq))
    totalLookup$sumPercent <- rowSums(totalLookup[, c("newsPercent", "blogPercent", "twitterPercent")])
    totalLookup <- totalLookup[order(totalLookup$sumPercent, decreasing = T),]
    topTarget <- as.character(totalLookup$target[1])
    
    if(is.na(topTarget)) {
        rand <- sample(1:10, 1)
        topTarget <- totalTrainUniLookup[rand, ]
    }
    return(topTarget)
}


# Clean input text
cleanInput <- function(input) {
    inputCorpus <- createCorpus(input)
    inputClean <- unigramTokenizer(inputCorpus[[1]])
    return(inputClean)
}

ngramLookup <- function(input) {
    inputClean <- cleanInput(input)
    gram1 <- inputClean[length(inputClean)-2]
    gram2 <- inputClean[length(inputClean)-1]
    gram3 <- inputClean[length(inputClean)]
    
    wordSuggest <- quadrigramLookup(gram1, gram2, gram3)
    if (input == "") {wordSuggest <- ""}
    if(is.na(wordSuggest) == F) {
        return(wordSuggest)
    } else {
        wordSuggest <- trigramLookup(gram2, gram3)
        if(is.na(wordSuggest) == F) {
            return(wordSuggest)
        } else {
            wordSuggest <- bigramLookup(gram3)
            return(wordSuggest)
        }
    }
}

# How to return just the value of wordSuggest, without the index?

# Actual server function
shinyServer(
    function(input,output) {
        output$wordSuggest <- renderPrint({ngramLookup(input$inputText)})
        }
)