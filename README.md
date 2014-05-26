Amazon.com-Product-Review-Authorship-Learning-Model
===================================================

A machine learning Natural Language Processing application that predicts authorship of Amazon.com product reviews using previous product reviews as a learning template.

The dataset for this project consists of 1500 observations of 10000 attributes and a classification of fifty authors. Each of the attributes is a representation of words of a user review posted on Amazon.com from fifty of the most active reviewers. Each observation represents one review from one author, giving us thirty reviews from each of the fifty authors. The attributes range from two or three characters of the word to entire phrases of up to three words in the review. The data are count data that consist of the frequency of appearance of words in user reviews classified by each author. The purpose of our research has been to develop computer intensive classification models that can be used to predict the author of the Amazon.com reviews in the data set with an acceptable level of accuracy. We feel that the results we obtained in the course of our research have applications beyond authorship classification. The types of models that we explain in this paper have applications in fraud detection, Internet search engines, medical imaging, handwriting detection, and other forms of pattern recognition. Evaluating classification models is more of objective task than with other types of predictive models, such as general linear models, since the predicted class is either correct or it is not. To assess the accuracy of our models, we divided the 1500 observations into a training set and a test set, with 1000 observations making up the training set (twenty reviews per author), and 500 observations making up the test set (ten reviews per author).  We then assessed the accuracy of each model by determining the percentage of test set observations that were correctly classified by each model.

In the course of evaluating our models we evaluated many variants of classification models ranging from simple J48 classifiers in Weka to much more complicated ensembles of support vector machines. Our first step was exploring the data, and some early models using basic algorithms to get an understanding of what we were working with.  Once that was done, we created thirteen models, using five classifiers with multiple settings on some of the classifiers.  We then ran those thirteen models with different seeds, for a total of three runs per classifier.  From this data, we chose the five models that performed the best, and ran each of these models ten times using different training test splits each time.  We then selected our final model based on the classifier that had the highest mean score on the ten runs. We focused the majority of our efforts on random forest models and an ensemble of support vector machines after testing many different modeling methods and found they had the highest accuracy rates in general. In comparison with the alternative modeling methods that we evaluated, there was little competition to the random forest and SVM models that we evaluated. We provided examples of a J48 decision tree model and a K-nearest neighbor model for comparison, but their accuracy rates were disappointing, at 38.5% and 25.1%, respectively.  We also attempted to create a neural network-based model, but the size of the dataset was so large that our computers were unable to process the model in R without crashing. Our choice of random forest and ensemble SVM as appropriate models for this data is also supported by previous researchers that have written about classification problems of the same complexity as the authors data in this study. In the initial stages of testing our models we employed a feature reduction method of using only the 1,000 most frequent features that appeared in the data set. The feature reduction reduced the processing time required while allowing us to retain as much predictive power as possible.  We also created models using the 2,000 and 5,000 most frequently occurring features. We ultimately arrived at our final conclusions regarding model performance through running the various modeling methods and evaluating their accuracy rates. We did not eliminate alternative modeling methods based on any factors other than empirically poor accuracy rate when compared to our random forest and ensemble SVM models. We did experience some minor setbacks due to the computer processing time required to calculate some of our classification models, and some modeling techniques were significantly quicker than others and incurred only a small decline in accuracy rate. 

Of the two primary modeling methods that we employed, the random forest was the least computer intensive in terms of processing times. A random forest classifier uses bootstrapped samples to build hundreds, or even thousands, of decision trees and then classifies a given observation in accordance with the majority-vote of the decision trees that make up the forest.  As an example, with a dataset that has three classes (A, B, and C), if a 500-tree random forest has 300 trees that classify an observation as A, 150 that classify the observation as B, and fifty that classify the observation as C, then the random forest classifier would classify the observation as belonging to class A.

The modeling method that was most successful for us was an ensemble of support vector machines. A support vector machine is a classifier that attempts to separate the data with a hyperplane in such a way as to maximize the separation between classes.  For data that is not linearly separable, the data is transformed using kernel functions into a form that can be separated with one or more hyperplanes. A normal SVM classifier requires a great deal of time and memory to process large datasets.  We found that R on our computers would lock up if the number of variables in the dataset exceeded 1000.  To attempt to solve this problem, we created an ensemble method using fifty different SVM classifiers (one for each author).  Each classifier only had to judge whether or not each observation was written by the single author that it was trained to predict.  Once each of the fifty classifiers had made their predictions, we had them output the probabilities associated with those predictions into a matrix, and then predicted the author for each observation based on which classifier had outputted the highest probability of the observation having been written by the single author that that classifier was trained to recognize.  
