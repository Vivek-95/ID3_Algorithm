require(data.tree)
require(caret)
require(randomForest)

#Function used to calculate entropy.
calc_entropy <- function(data){
  #Calculate entropy using the formula p(x)*log2(1/p(x))
  ent <- data/sum(data)*log2(sum(data)/data)
  #Hard Code the blank spaces to 0 indicating that the entropy is 0.
  ent[data==0] <- 0
  sum(ent)
}

#Function used to calculate information gain
info_gain <- function(data){
  #First we find out the entropy of the label then the conditional entropy of the label with respect to each of the attributes.
  #Divide data to get probability
  data<-data/sum(data)
  #Calculate class label's entropy
  label_entropy <- calc_entropy(colSums(data))
  #Store the probability of each row
  row_prob <- rowSums(data)
  #Calculate the conditional probability by summing up the multiplication of the row's probability by it's entropy. 
  cond <- sum(row_prob*apply(data,MARGIN = 1,calc_entropy))
  #return the information gain as the difference between the label's entropy and the conditional entropy.
  label_entropy - cond  
}

iterate <- function(data,node,initial_col_names,initial_data){
  
    
    #Storing the information gain of all the atributes in a vector and selecting the maximum gain thereby determining
    #which attribute the split should take place in.
    info_gain_table <-c()
    for(i in 1:(ncol(data)-1)){
      
      info_gain_table[i] <- info_gain(table(data[,i],data[,ncol(data)]))
    }
    
    #Finds out the position of the attribute.
    pos <- which.max(info_gain_table)
    
    #Replace modified names in the subsetted dataframe with the column names present in the original dataset.
    for(i in 1:length(initial_col_names)){
      names(data)[which(grepl(initial_col_names[i],names(data)))]=initial_col_names[i]
    }
    
    #Name of the attribute to split on.
    name_of_attribute <- names(data)[pos]
    #Vector of attribute to split on present in the original dataset so that all factor levels of the attribute can be accounted for.
    new_att <- initial_data[,name_of_attribute]

    #Setting threshold for information gain.
    if(max(info_gain_table < 0.01)){
      #This sets the minimum information gain limit. If the maximum information gain obtained by the remaining atributes is lesser
      #than a particular value, then a child node contaning the name of the attribute is created and the majority class label 
      #of the attribute is attached as the child node to the attribute indicating that if that particular attribute is encountered in 
      #the tree then it belongs to the selected Type of SalePrice.
      whether_to_split <-  data.frame(table(data[,pos],data[,ncol(data)]))
      values <- whether_to_split[whether_to_split$Freq == max(whether_to_split$Freq),]
      node <- node$AddChild(values[1,2])
      data <- data[,-pos]
      return(node)}
    
    #Removing the atribute that the split is going to take place on.
    data <- data[,-pos]
    #Contains the maximum frequency of the class label. This value is present to add as the child node if certain factor levels
    #of the attribute to split on are not present in the subset of the data after the split. 
    max_count <- data.frame(table(data[,ncol(data)]))
    max_count <- max_count[max_count$Freq ==max(max_count$Freq),]
    #Split the dataframe into a list based on the levels present in the attribute to split on.
    data <- split(data,new_att,drop=TRUE)
    
    #Add the name of the attribute the split is occuring on as the child node.
    attribute_name <- node$AddChild(name_of_attribute)
    
    
    #For every factor level in the attribute to split add a child node to the parent node and call the iterate 
    #function recursively.
    for(i in 1:length(data)){
      decide_traverse <- data.frame(data[i])
      
      #If there is no data in the subset, then add the value of SalePrice stored in max_count variable as the child node 
      #of the particular split attribute's factor level.
      if(nrow(decide_traverse)==0){
        child <- attribute_name$AddChild(i)
        child <- child$AddChild(max_count[1,1])
      }
      #Otherwise Add the factor level as the child node of the attribute to split on and recursively call the iterate function.
      else{
      child <- attribute_name$AddChild(names(data)[i])
      iterate(data.frame(data[i]),child,initial_col_names,initial_data)
      }
    
  }
  node
}


id3 <- function(data){
  initial_col_names <-names(data)
  root <- Node$new(names(data)[ncol(data)])
  return(iterate(data,root,initial_col_names,initial_data = data))
}

#Predict function
predict2<-function(tree,test){
  child <- tree
  #Call while loop until the height of the tree reduces down to 2. When the height of the tree=2, then the tree contains 
  #the SalePrice type as the child node which can be returned.
  while(child$height>2){
    #Traverse the tree by it's children.
    child <- child$children[[1]]
    #The value of the attribute to be checked upon(by referring the tree) present in the instance of the testing dataset is 
    #stored in the variable named value.
      value <- test[1,child$name]
      #Traverse the tree through it's children by using the value of the particular attribute obtained in the testing instance. 
      child <- child$children[[value]]
  }
  #Now the height of the tree is 2. The SalePrice type is the child node. Return it. 
    child <- child$children[[1]]
    return(child$name)
}

#Final Prediciton function. Predict2 function returns a predicted value for a single instance in the testing dataset.final_predict
#function calls the Predict2 function over all instances in the testing dataset.
final_predict <- function(tree,test){
  #store the result in an empty dataframe.
  result <- data.frame()
  for(i in 1:nrow(test)){
    final <- predict2(tree,test[i,])
  result[i,1] <- final
  }
  return(result)
}

#Training Dataset.
#Considered the first 1000 instances to be the training dataset and the rest 460 are the testing instances.
tree <- id3(train2[1:1000,])
prediction <- final_predict(tree,train2[1001:1460,])
confusionMatrix(as.factor(prediction[,1]),factor(train2$SalePrice[1001:1460]))

#Using the inbuilt randomForest package
model <- randomForest(SalePrice~.,train2[1:1000,],ntree=50)
p <- predict(model,train2[1001:1460,])
confusionMatrix(as.factor(p),as.factor(train2$SalePrice[1001:1460]))
