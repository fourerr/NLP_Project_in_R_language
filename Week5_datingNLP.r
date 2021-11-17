library(stringr)
library(dplyr)
library(ggplot2)
library(mosaic)
library(dplyr)
library(stringr)
library(xtable)
library(gridExtra)
library(stopwords)
library(quanteda)
library(caret)


profiles <- read.csv( file.path( 'okcupid_profiles.csv' ), header=TRUE, stringsAsFactors=FALSE)

str(profiles)


# Union all the essays
essays <- select(profiles, starts_with("essay"))
essays <- apply(essays, MARGIN = 1, FUN = paste, collapse=" ")



# Tokenize essay texts per word
all.tokens <- tokens(essays, what = "word",
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)
all.tokens[[1]]		
			   
# Remove all the stepwords//מילות יחס				   
all.tokens <- tokens_select(all.tokens, stopwords(),
                              selection = "remove")
							  
all.tokens[[1]]		
				  
# reduce stemming//שורשים 							 
all.tokens <- tokens_wordstem(all.tokens, language = "english")

all.tokens[[1]]


# create dfm(document-feature matrix)
all.tokens.dfm <- dfm(all.tokens, tolower = FALSE)

#return the tokens show 10000 at least
all.tokens.dfm2<-dfm_trim(all.tokens.dfm, min_termfreq =10000)

all.tokens.matrix2 <- as.matrix(all.tokens.dfm2)

all.tokens.matrix2[1,5]

if (all.tokens.matrix2[1, 5] > 0)
{
print(all.tokens.matrix2[1, 5])
}

all.tokens.matrix2.normal<-all.tokens.matrix2
#------------------------------------------------


##create_count_t_vector

count_t_vector<-c()

for (column in 1:ncol(all.tokens.matrix2)){

count<-0
for (row in 1:nrow(all.tokens.matrix2))
{
	if (all.tokens.matrix2[row,column]>0)
	{
		count <- count+1
	}#end if
	
} # end row
count_t_vector[column]<-count
} #end column
print(count_t_vector)

#--------------------------------------------------
idf<- function(column,metrix,count_vector){

	
	new_idf<-log2(nrow(metrix)/count_vector[column])

	return (new_idf)						  
}#end fun


tf<- function(num_txt,num_column,metrix,sum_of_row)
{
	return (metrix[num_txt,num_column]/sum_of_row)
}


sum_of_row<-function(metrix,num_row)
{
count<-0
for (column in 1:ncol(metrix))
{
	count<- count+metrix[num_row,column]
}
return (count)
}


n<-nrow(all.tokens.matrix2)#n in the number of rows
for (num_row in 1:n)
{
sor<-sum_of_row(all.tokens.matrix2,num_row)
for (num_col in 1:ncol(all.tokens.matrix2))
{
#tf<- function(num_txt,num_column,metrix,sum_of_row)
	t.f<-tf(num_row,num_col,all.tokens.matrix2,sor)
	
	id.f<-idf(num_col,all.tokens.matrix2,count_t_vector)
	
	all.tokens.matrix2.normal[num_row,num_col]<-(t.f*id.f)
}#end col

}#end row


# reduce the null values
all.tokens.matrix2.normal[which(is.na(all.tokens.matrix2.normal))]<-0

#matrix to data frame to evaluate the model
df.token<-as.data.frame(all.tokens.matrix2.normal, make.names = TRUE) 

#adding another column for target to classify 
df.token$labels=profiles$sex 

# cv with repeat 3 times
trainctrl <- trainControl(method="repeatedcv",
                          number=10,
                          repeats=3)

						  
x<-df.token[,-which(names(df.token) == "labels")]
names(x)
target<-df.token[,which(names(df.token) == "labels")]	

		  
#training the model using decision tree and cv from above
start_time <- Sys.time()
					  
model<-train(x,
             target,
             method="rpart",
             trControl=trainctrl)
			 
			 
end_time <- Sys.time()
print(model)

#print the time stamp of creating the model 
print(end_time - start_time)

# print confusion matrix and accuracy 
confusionMatrix(model,"none")


 
 
 #reduce the seperate words 
 batch.reduce<-names(model$finalModel$var)
#---------------------------------------------------------------------------------




#create new df without seperate words
df.cluster<-subset(df.token,select = -c(as.factor(batch.reduce)))

#reduce the target column for clustering
df.cluster.reduce.target<-df.cluster[, -which(names(df.cluster) == "labels")]

kmeans2<-kmeans(df.cluster.reduce.target,2)
kmeans3<-kmeans(df.cluster.reduce.target,3)
kmeans4<-kmeans(df.cluster.reduce.target,4)
kmeans10<-kmeans(df.cluster.reduce.target,10)

#adding new column represent the cluster of specific doc
df.cluster.reduce.target$clusters2<-kmeans2$cluster
df.cluster.reduce.target$clusters3<-kmeans3$cluster
df.cluster.reduce.target$clusters4<-kmeans4$cluster
df.cluster.reduce.target$clusters10<-kmeans10$cluster


#create pca for data frame
pca<-prcomp(df.cluster.reduce.target,scale=TRUE)

# create elbow represent the variance of all pca
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

#bar plot represent the variance of ten first pc's 
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

# take two first pca
relevant.pc<-pca$x[, 1:2]

#create data frame of two pc 
pca.df<-data.frame(relevant.pc[,],cluster=factor(df.cluster.reduce.target$clusters2[]))


#library("ggplot")
pdf("Week5_datingNLP_2_cluster.pdf")
#create plot represent the different pca's seperate by cluster
ggplot(pca.df,aes(x=pca.df[, 1], y=pca.df[,2])) +
      geom_point(aes(color = cluster), alpha = 1, size = 3) +
      xlab("PC1") +
      ylab("PC2") + 
      ggtitle("PCA 2 Clusters for text processing ") 

pca.df<-data.frame(relevant.pc[,],cluster=factor(df.cluster.reduce.target$clusters3[]))
dev.off()




pdf("Week5_datingNLP_3_cluster.pdf")
#for 
ggplot(pca.df,aes(x=pca.df[, 1], y=pca.df[,2])) +
      geom_point(aes(color = cluster), alpha = 1, size = 3) +
      xlab("PC1") +
      ylab("PC2") + 
      ggtitle("PCA 3 Clusters for text processing ") 
	  
	  
pca.df<-data.frame(relevant.pc[,],cluster=factor(df.cluster.reduce.target$clusters4[]))
dev.off()


pdf("Week5_datingNLP_4_cluster.pdf")
#for 
ggplot(pca.df,aes(x=pca.df[, 1], y=pca.df[,2])) +
      geom_point(aes(color = cluster), alpha = 1, size = 3) +
      xlab("PC1") +
      ylab("PC2") + 
      ggtitle("PCA 4 Clusters for text processing ")
dev.off()

pca.df<-data.frame(relevant.pc[,],cluster=factor(df.cluster.reduce.target$clusters10[]))



pdf("Week5_datingNLP_10_cluster.pdf")
#for 
ggplot(pca.df,aes(x=pca.df[, 1], y=pca.df[,2])) +
      geom_point(aes(color = cluster), alpha = 1, size = 3) +
      xlab("PC1") +
      ylab("PC2") + 
      ggtitle("PCA 10 Clusters for text processing ") 	  
	  
dev.off()


