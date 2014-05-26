> require(RWeka)
Loading required package: RWeka
> 
> d1 <- date()
> 
> # Reading the dataset from my working directory
> full <- read.arff("basicset.arff")
> 
> # Editing one variable's name from "..." to "elipsis" because R has trouble with "..."
> colnames(full)[8448] <- "elipsis"
> 
> # Keep only the 5000 most used variables (removes 5000 columns)
> v <- full[,1:10000] # selecting all of the columns except the class variable
> v2 <- rbind(v,colSums(v)) # creating a new row at the bottom that is the sum of each column, which gives us the total frequency for each variable across the entire dataset
> v3 <- order(v2[1501,], decreasing = TRUE) # determining the order to sort by frequency
> v4 <- v2[,v3] # sorting the columns by frequency
> v5 <- v4[1:1500,1:5000] # selecting all of the rows except the colSums row, and only the 5000 most frequent variables
> v6 <- cbind(v5,full$author_class) # adding the class variable column back on
> colnames(v6)[5001] <- "author_class" # renaming the class variable column to it's original name
> mainset <- v6 # renaming the dataframe to something more descriptive
> 
> # Saving the number of columns before we add the author binary indicators
> colnum <- ncol(mainset)
> 
> # Creating a vector that contains the name of each author in the set
> authorlist <- as.vector(unique(mainset$author_class))
> 
> # Creating a function that creats a new vector of 1's and 0's denoting if, given an author_class, each observation is from that class or not
> authorbinary <- function(x) {
+ 	assign(x,ifelse((mainset$author_class == x),1,0))
+ 	}
> 
> # Recording the number of authors as a variable (for use in the next function)
> len <- length(authorlist)	
> 
> # Creating factors for each author with the authorbinary function, then adding them as columns to our main dataframe, and naming then with the author_class name 	
> for (i in 1:len) {
+ 	x <- ncol(mainset) # determines the current number of columns in the dataframe
+ 	mainset <- cbind(mainset,factor(authorbinary(authorlist[i]),levels=c(0,1),labels=c("NO","YES"))) #adds a new column using the authorbinary function
+ 	colnames(mainset)[x+1] <- authorlist[i] # names the new column for the author it's related to
+ 	}
> 	
> # Splitting the dataset into a training set and a test set.
> require(caret)
Loading required package: caret
Loading required package: lattice
Loading required package: reshape
Loading required package: plyr

Attaching package: ‘reshape’

The following object(s) are masked from ‘package:plyr’:

    rename, round_any

Loading required package: cluster
Loading required package: foreach
foreach: simple, scalable parallel programming from Revolution Analytics
Use Revolution R for scalability, fault tolerance and more.
http://www.revolutionanalytics.com
> set.seed(5000) #setting a seed so that this exact split is reproducable
> index <- createDataPartition(mainset$author_class, p=.66, list=FALSE, times=1) # Doing a 2/3s training / 1/3s test split
> training <- mainset[index,] # creating a new dataframe with the training set
> test <- mainset[-index,] # creating a new dataframe with the test set
> 
> # Creating an object for use in the next formula
> probmodel <- 0 #this is to allow the next formula to use cbind each time though the loop to create the probabilty matrix
> 
> # Creating an SVM model for each authorbinary, then outputing the probabilty of "YES" from each model to a data frame
> require(kernlab)
Loading required package: kernlab
> for (i in 1:len) {
+ 	x <- training[,c(1:colnum-1,colnum+i)] # creating a subset of training that only has access to the predictor variables and the authorbinary for one author, so that the ksvm() can't access the author_class or the other authorbinaries
+ 	set.seed(5000) # setting a seed to make sure this is reproducable
+ 	colnames(x)[colnum] <- "author" #naming the authorbinary column that the function is working with "author" for use in the next line
+ 	svm <- ksvm(author~.,data=x,prob.model=TRUE) # creating an SVM classifier for the authorbinary
+ 	authprob <- predict(svm, test, type = "probabilities") # exporting the probabilities for "YES" and "NO" for each observation in the test set using the model created in the previous line
+ 	probmodel <- cbind.data.frame(probmodel,authprob[,2]) # adding the "YES" probability column to the probmodel matrix
+ 	}
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
Using automatic sigma estimation (sigest) for RBF or laplace kernel 
> 
> # Removing the column of 0's that I used to create it
> probmodel <- probmodel[,-1]
> 
> # Naming each column of the probmodel matrix for the author it refers to
> colnames(probmodel) <- authorlist 
> 
> # Creating columns showing predicted author and actual author
> probmodel$predicted_author <- authorlist[max.col(probmodel)] # for each row, calculating the column with the highest probility of "YES", then using that data to create a new column with the name of the predicted author for each row
> probmodel$actual_author <- as.character(test$author_class) # listing the actual author for each row
> 
> # Creating a new column where the value is equal to 1 if predicted_author = actual_author, or 0 if they are not equal
> probmodel$correct <- ifelse((probmodel$predicted_author == probmodel$actual_author),1,0)
> 
> # Calculating the accuracy of the predictor by taking the mean of the probmodel$correct column, which will show the percentage correct
> Accuracy <- mean(probmodel$correct)
> Accuracy
[1] 0.83
> 
> # Building a confusion matrix for the model
> conf <- confusionMatrix(probmodel$predicted_author,probmodel$actual_author)
Loading required package: class

Attaching package: ‘class’

The following object(s) are masked from ‘package:reshape’:

    condense

> conf
Confusion Matrix and Statistics

             Reference
Prediction    Agresti Ashbacher Auken Blankenship Brody Brown Bukowsky Calvinnme CFH Chachra Chandler Chell Cholette Comdet Corn Cutey Davisson Dent Engineer Goonan Grove Harp
  Agresti          10         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        2      0     0    0
  Ashbacher         0        10     0           0     0     0        1         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Auken             0         0     7           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Blankenship       0         0     0          10     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Brody             0         0     0           0     8     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Brown             0         0     0           0     0     7        0         0   0       0        0     0        1      0    0     0        0    0        0      0     0    0
  Bukowsky          0         0     2           0     0     0        8         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Calvinnme         0         0     0           0     0     1        0         8   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  CFH               0         0     0           0     0     0        0         0   6       0        0     0        1      0    0     0        0    0        0      0     0    0
  Chachra           0         0     0           0     1     0        0         0   0      10        0     1        0      2    0     0        0    0        0      0     0    0
  Chandler          0         0     0           0     0     0        0         0   0       0        9     0        0      1    0     1        0    0        0      0     0    0
  Chell             0         0     0           0     0     0        0         0   0       0        0     7        0      0    0     0        0    0        0      0     0    0
  Cholette          0         0     0           0     0     0        0         0   0       0        0     0        8      0    0     0        0    0        0      0     0    0
  Comdet            0         0     0           0     0     0        0         0   0       0        0     0        0      6    0     0        0    0        0      0     0    0
  Corn              0         0     0           0     0     0        0         0   0       0        0     1        0      0    7     0        0    0        0      0     0    0
  Cutey             0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     9        0    0        0      0     0    0
  Davisson          0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        7    0        0      0     0    0
  Dent              0         0     0           0     0     0        0         1   0       0        1     1        0      1    1     0        0   10        0      0     0    0
  Engineer          0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        8      0     0    0
  Goonan            0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0     10     0    0
  Grove             0         0     0           0     0     0        0         0   1       0        0     0        0      0    0     0        0    0        0      0     8    0
  Harp              0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    8
  Hayes             0         0     0           0     0     1        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Janson            0         0     0           0     1     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Johnson           0         0     0           0     0     0        0         0   0       0        0     0        0      0    1     0        0    0        0      0     0    0
  Koenig            0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Kolln             0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Lawyeraau         0         0     1           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Lee               0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        2    0        0      0     0    0
  Lovitt            0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        1    0        0      0     0    0
  Mahlers2nd        0         0     0           0     0     0        0         0   1       0        0     0        0      0    1     0        0    0        0      0     0    0
  Mark              0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  McKee             0         0     0           0     0     0        0         1   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Merritt           0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Messick           0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Mitchell          0         0     0           0     0     1        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Morrison          0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Neal              0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Nigam             0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Peterson          0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Power             0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Riley             0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Robert            0         0     0           0     0     0        0         0   1       0        0     0        0      0    0     0        0    0        0      0     0    0
  Shea              0         0     0           0     0     0        0         0   1       0        0     0        0      0    0     0        0    0        0      0     1    0
  Sherwin           0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     1    0
  Taylor            0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Vernon            0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Vision            0         0     0           0     0     0        1         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
  Walters           0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    2
  Wilson            0         0     0           0     0     0        0         0   0       0        0     0        0      0    0     0        0    0        0      0     0    0
             Reference
Prediction    Hayes Janson Johnson Koenig Kolln Lawyeraau Lee Lovitt Mahlers2nd Mark McKee Merritt Messick Mitchell Morrison Neal Nigam Peterson Power Riley Robert Shea Sherwin
  Agresti         0      0       0      1     0         0   1      0          0    0     0       0       0        0        0    2     1        0     0     0      0    0       0
  Ashbacher       0      0       0      0     0         0   0      1          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Auken           0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Blankenship     0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Brody           1      0       0      0     0         0   0      0          0    0     0       0       0        0        0    1     0        0     0     0      0    0       0
  Brown           0      0       0      0     0         1   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Bukowsky        0      0       0      0     0         1   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Calvinnme       0      1       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  CFH             0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Chachra         0      0       1      0     0         0   0      0          0    0     0       0       1        0        0    0     0        0     0     0      0    0       0
  Chandler        0      0       0      0     0         0   0      0          0    1     0       0       0        0        0    0     0        0     0     0      0    0       0
  Chell           0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Cholette        0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Comdet          0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Corn            0      0       0      0     0         0   0      0          0    0     1       0       0        0        0    1     1        0     0     0      0    0       0
  Cutey           0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Davisson        0      0       2      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Dent            0      0       0      0     0         0   0      0          1    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Engineer        0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Goonan          0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Grove           0      0       1      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Harp            0      0       0      0     0         0   1      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Hayes           9      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Janson          0      8       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Johnson         0      0       6      0     0         0   0      0          0    0     0       0       0        0        0    1     0        0     0     0      0    0       0
  Koenig          0      0       0      7     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Kolln           0      0       0      0    10         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Lawyeraau       0      0       0      0     0         7   0      0          0    0     0       0       0        0        0    0     1        0     0     0      0    0       0
  Lee             0      0       0      0     0         0   6      0          0    0     0       0       0        0        0    0     1        0     0     0      0    0       0
  Lovitt          0      0       0      0     0         0   0      9          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Mahlers2nd      0      0       0      0     0         0   0      0          8    0     0       0       0        0        0    0     0        0     0     1      0    0       0
  Mark            0      0       0      0     0         0   0      0          0    9     0       0       0        0        0    0     0        0     0     0      0    0       0
  McKee           0      0       0      0     0         0   0      0          0    0     9       0       0        0        0    0     0        0     0     0      0    0       0
  Merritt         0      0       0      0     0         0   0      0          0    0     0       9       0        0        0    0     0        0     0     0      0    0       0
  Messick         0      0       0      0     0         0   0      0          0    0     0       0       8        0        0    0     0        0     0     0      0    0       0
  Mitchell        0      1       0      0     0         0   0      0          1    0     0       0       0       10        0    0     0        0     0     1      0    0       0
  Morrison        0      0       0      0     0         1   0      0          0    0     0       0       0        0       10    0     0        0     0     0      0    0       0
  Neal            0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    5     0        0     0     0      0    0       0
  Nigam           0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     4        0     0     0      0    1       0
  Peterson        0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        9     0     0      0    0       0
  Power           0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0    10     0      0    0       0
  Riley           0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        1     0     8      0    0       0
  Robert          0      0       0      0     0         0   0      0          0    0     0       0       1        0        0    0     0        0     0     0     10    0       0
  Shea            0      0       0      1     0         0   0      0          0    0     0       0       0        0        0    0     1        0     0     0      0    9       0
  Sherwin         0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0      10
  Taylor          0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Vernon          0      0       0      1     0         0   1      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
  Vision          0      0       0      0     0         0   1      0          0    0     0       0       0        0        0    0     1        0     0     0      0    0       0
  Walters         0      0       0      0     0         0   0      0          0    0     0       1       0        0        0    0     0        0     0     0      0    0       0
  Wilson          0      0       0      0     0         0   0      0          0    0     0       0       0        0        0    0     0        0     0     0      0    0       0
             Reference
Prediction    Taylor Vernon Vision Walters Wilson
  Agresti          0      0      0       0      0
  Ashbacher        0      0      0       0      0
  Auken            0      0      0       0      0
  Blankenship      0      0      0       0      0
  Brody            0      0      0       0      0
  Brown            0      0      0       0      0
  Bukowsky         0      0      0       0      0
  Calvinnme        0      0      0       0      0
  CFH              0      0      0       0      0
  Chachra          0      0      1       0      0
  Chandler         0      0      0       0      0
  Chell            0      0      0       0      0
  Cholette         0      0      0       0      0
  Comdet           0      0      0       0      0
  Corn             0      0      0       0      0
  Cutey            0      0      0       0      0
  Davisson         0      0      0       0      0
  Dent             0      0      0       0      0
  Engineer         0      0      0       0      0
  Goonan           0      0      0       0      0
  Grove            0      0      0       0      0
  Harp             0      0      0       0      0
  Hayes            0      0      0       0      0
  Janson           0      0      0       0      0
  Johnson          0      0      0       0      0
  Koenig           0      0      0       0      0
  Kolln            0      0      0       0      0
  Lawyeraau        0      0      0       0      0
  Lee              1      0      0       0      1
  Lovitt           0      0      0       0      0
  Mahlers2nd       0      1      0       0      0
  Mark             0      0      0       0      0
  McKee            0      0      0       0      0
  Merritt          0      0      0       0      0
  Messick          0      0      0       0      0
  Mitchell         1      0      0       0      0
  Morrison         0      0      0       0      0
  Neal             0      0      0       0      0
  Nigam            0      0      0       0      0
  Peterson         0      0      0       0      0
  Power            0      0      0       0      0
  Riley            0      0      0       0      0
  Robert           0      0      0       0      0
  Shea             0      0      1       0      0
  Sherwin          0      0      0       0      0
  Taylor           8      0      0       0      0
  Vernon           0      9      0       0      0
  Vision           0      0      8       0      0
  Walters          0      0      0      10      0
  Wilson           0      0      0       0      9

Overall Statistics
                                          
               Accuracy : 0.83            
                 95% CI : (0.7941, 0.8619)
    No Information Rate : 0.02            
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.8265          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: Agresti Class: Ashbacher Class: Auken Class: Blankenship Class: Brody Class: Brown Class: Bukowsky Class: Calvinnme Class: CFH Class: Chachra
Sensitivity                  1.0000           1.0000       0.7000               1.00       0.8000       0.7000          0.8000           0.8000     0.6000         1.0000
Specificity                  0.9857           0.9959       1.0000               1.00       0.9959       0.9959          0.9939           0.9959     0.9980         0.9857
Pos Pred Value               0.5882           0.8333       1.0000               1.00       0.8000       0.7778          0.7273           0.8000     0.8571         0.5882
Neg Pred Value               1.0000           1.0000       0.9939               1.00       0.9959       0.9939          0.9959           0.9959     0.9919         1.0000
Prevalence                   0.0200           0.0200       0.0200               0.02       0.0200       0.0200          0.0200           0.0200     0.0200         0.0200
Detection Rate               0.0200           0.0200       0.0140               0.02       0.0160       0.0140          0.0160           0.0160     0.0120         0.0200
Detection Prevalence         0.0340           0.0240       0.0140               0.02       0.0200       0.0180          0.0220           0.0200     0.0140         0.0340
                     Class: Chandler Class: Chell Class: Cholette Class: Comdet Class: Corn Class: Cutey Class: Davisson Class: Dent Class: Engineer Class: Goonan Class: Grove
Sensitivity                   0.9000       0.7000          0.8000        0.6000      0.7000        0.900          0.7000      1.0000          0.8000          1.00       0.8000
Specificity                   0.9939       1.0000          1.0000        1.0000      0.9918        1.000          0.9959      0.9878          1.0000          1.00       0.9959
Pos Pred Value                0.7500       1.0000          1.0000        1.0000      0.6364        1.000          0.7778      0.6250          1.0000          1.00       0.8000
Neg Pred Value                0.9980       0.9939          0.9959        0.9919      0.9939        0.998          0.9939      1.0000          0.9959          1.00       0.9959
Prevalence                    0.0200       0.0200          0.0200        0.0200      0.0200        0.020          0.0200      0.0200          0.0200          0.02       0.0200
Detection Rate                0.0180       0.0140          0.0160        0.0120      0.0140        0.018          0.0140      0.0200          0.0160          0.02       0.0160
Detection Prevalence          0.0240       0.0140          0.0160        0.0120      0.0220        0.018          0.0180      0.0320          0.0160          0.02       0.0200
                     Class: Harp Class: Hayes Class: Janson Class: Johnson Class: Koenig Class: Kolln Class: Lawyeraau Class: Lee Class: Lovitt Class: Mahlers2nd Class: Mark
Sensitivity               0.8000        0.900        0.8000         0.6000        0.7000         1.00           0.7000     0.6000         0.900            0.8000       0.900
Specificity               0.9980        0.998        0.9980         0.9959        1.0000         1.00           0.9959     0.9898         0.998            0.9918       1.000
Pos Pred Value            0.8889        0.900        0.8889         0.7500        1.0000         1.00           0.7778     0.5455         0.900            0.6667       1.000
Neg Pred Value            0.9959        0.998        0.9959         0.9919        0.9939         1.00           0.9939     0.9918         0.998            0.9959       0.998
Prevalence                0.0200        0.020        0.0200         0.0200        0.0200         0.02           0.0200     0.0200         0.020            0.0200       0.020
Detection Rate            0.0160        0.018        0.0160         0.0120        0.0140         0.02           0.0140     0.0120         0.018            0.0160       0.018
Detection Prevalence      0.0180        0.020        0.0180         0.0160        0.0140         0.02           0.0180     0.0220         0.020            0.0240       0.018
                     Class: McKee Class: Merritt Class: Messick Class: Mitchell Class: Morrison Class: Neal Class: Nigam Class: Peterson Class: Power Class: Riley Class: Robert
Sensitivity                 0.900          0.900         0.8000          1.0000          1.0000      0.5000       0.4000           0.900         1.00       0.8000        1.0000
Specificity                 0.998          1.000         1.0000          0.9898          0.9980      1.0000       0.9980           1.000         1.00       0.9980        0.9959
Pos Pred Value              0.900          1.000         1.0000          0.6667          0.9091      1.0000       0.8000           1.000         1.00       0.8889        0.8333
Neg Pred Value              0.998          0.998         0.9959          1.0000          1.0000      0.9899       0.9879           0.998         1.00       0.9959        1.0000
Prevalence                  0.020          0.020         0.0200          0.0200          0.0200      0.0200       0.0200           0.020         0.02       0.0200        0.0200
Detection Rate              0.018          0.018         0.0160          0.0200          0.0200      0.0100       0.0080           0.018         0.02       0.0160        0.0200
Detection Prevalence        0.020          0.018         0.0160          0.0300          0.0220      0.0100       0.0100           0.018         0.02       0.0180        0.0240
                     Class: Shea Class: Sherwin Class: Taylor Class: Vernon Class: Vision Class: Walters Class: Wilson
Sensitivity               0.9000         1.0000        0.8000        0.9000        0.8000         1.0000         0.900
Specificity               0.9898         0.9980        1.0000        0.9959        0.9939         0.9939         1.000
Pos Pred Value            0.6429         0.9091        1.0000        0.8182        0.7273         0.7692         1.000
Neg Pred Value            0.9979         1.0000        0.9959        0.9980        0.9959         1.0000         0.998
Prevalence                0.0200         0.0200        0.0200        0.0200        0.0200         0.0200         0.020
Detection Rate            0.0180         0.0200        0.0160        0.0180        0.0160         0.0200         0.018
Detection Prevalence      0.0280         0.0220        0.0160        0.0220        0.0220         0.0260         0.018
> 
> # Saving the current time at project completion
> d2 <- date()
> 
> # Looking at both date() results to see how long the program took to run
> d1
[1] "Sun Nov 25 17:12:38 2012"
> d2
[1] "Sun Nov 25 17:34:58 2012"
> 
> #end
> 