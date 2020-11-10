#Setting the working directory
setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
#Load files into corpus
setwd('pp4data/artificial/')

filenames<-list.files(path = getwd(), pattern = "")
temp1<-list.files(path = getwd(), pattern = "*.csv")
filenames<-setdiff(filenames, temp1)
myFiles.sorted <- sort(filenames)
split <- strsplit(myFiles.sorted, ' ') 
split <- as.numeric(split)
myFiles.correct.order <- myFiles.sorted[order(split)]

# Read files into a list of docs
artificial_data<-suppressWarnings(lapply(myFiles.correct.order, readLines))

Whole_code_starts_artificial <- Sys.time()
result_final_artificial<-Main_function(artificial_data,K=2,top_words = 3)
Whole_code_ends_artificial <- Sys.time()
run_time_artificial<- Whole_code_ends_artificial - Whole_code_starts_artificial

#Loading 20 Newsgroups Data
setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
setwd('pp4data/20newsgroups/')

filenames<-list.files(path = getwd(), pattern = "")
temp1<-list.files(path = getwd(), pattern = "*.csv")
filenames<-setdiff(filenames, temp1)
myFiles.sorted <- sort(filenames)
split <- strsplit(myFiles.sorted, ' ') 
split <- as.numeric(split)
myFiles.correct.order <- myFiles.sorted[order(split)]

# Read files into a list of docs
Twenty_newsgroup_data<-suppressWarnings(lapply(myFiles.correct.order, readLines))

Whole_code_starts_Twenty_newsgroup <- Sys.time()
result_final_Twenty_newsgroup<-Main_function(Twenty_newsgroup_data,K=20,top_words = 5)
Whole_code_ends_Twenty_newsgroup <- Sys.time()
run_time_Twenty_newsgroup<- Whole_code_ends_Twenty_newsgroup - Whole_code_starts_Twenty_newsgroup

#Task 2: Classification

#Importing data for logistic regression
label_data<-read.csv('index.csv', header = F)
label_data<-label_data[2]

#LDA CLASSIFICATION using this label data
start_time_lr1 <- Sys.time()
sums_lr1<-replicate(30,W_Map_test(result_final_Twenty_newsgroup[[1]],label_data))
end_time_lr1 <- Sys.time()

#Plots LDA 
setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))
png(file="DocumentTopicRepresentation.png")
GraphPlot(sums_lr1,label_data, "Document Topic Representation")
graphics.off()


#BOW CLASSIFICATION using this label data
start_time_lr2 <- Sys.time()
sums_lr2<-replicate(30,W_Map_test(result_final_Twenty_newsgroup[[2]],label_data))
end_time_lr2 <- Sys.time()

#Plots bag of words
png(file="BagOfWords.png")
GraphPlot(sums_lr2,label_data,"Bag of words Representation")
graphics.off()


