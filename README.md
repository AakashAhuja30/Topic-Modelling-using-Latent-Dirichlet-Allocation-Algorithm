# Topic-Modelling-using-Latent-Dirichlet-Allocation-Algorithm from scratch

**About the Project:** We have the famous 20Newsgroups Dataset here with 200 documents present in it with words present in each document. Now with such huge volume of data, it will be very difficult to go through the entire data and categorise the data into few broad topics. This is what the LDA Algorithm does using Gibbs Sampling. Each topic has a few frequent words based on which we can guess what that topic represents. The algorithm will help us build 2 key matrices:

Document Topic Count Matrix:
Word Topic Count Matrix
(We’ll be taking the number of topics to be 20.)

This code is compiled in 2 files which have to be run in the same order:

1. LDA_logreg_functions.R
This file has all the functions that are used for implementation. 
The first function is the Main_function() which takes in 3 parameters, namely data file(list of documents), Number of Topics(K) and number of top words that i want in each topic. In artificial i only wanted 3 words. Whereas in 20newsgroups data i wanted the top 5 words for each topic. 

The 2nd function is W_map_test() function which we built to carry out logistic regression from scratch. It takes in 2 parameters, namely the train set and the test set. 

The 3rd function is my GraphPlot() function used for building the accuracy plots from the results of the logistic regression function above and the test data


2.Implementation_data.R
This file loads the  Artificial data and the 20newsgroups data and then runs the Main_function defined on the LDA_logreg_functions.R file to give the top 3 words for Artificial dataset and top 5 words for the 20 Newsgroups dataset. It also gives the document topic distribution and the bag of words matrix.

This document topic distribution and bag of words matrix are put into our logistic regression function named W_map_test() with the label data individually which gives the accuracy for each of these 2 models. 

Then we use the GraphPlot() function defined in the first file to plot the graphs of accuracy of these 2 models on our label data present in the index.csv file. 
