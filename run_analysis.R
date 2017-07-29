#Written by DST on July 29, 2017 on a macbook
#To be submitted for Getting and Cleaning data
#This code reads data frames form test and traingin , combines them, and cleans tem up
#Writes down a tidydata
run_analysis<-function() {
	#The file names are very descriptive and the same pattern is followed for 
	#both training and test data. Typical training and test names differ only in the 
	#string "test" and "train", everything else is the same. 
	#Therefore first create a list of the files that have the test data by searching 
	#recursively in the directory
	testfilenames<-grep("test",list.files(recursive="TRUE"),value="TRUE")
	#now go through the list of the files that have training data
	count<-0
	for (name in testfilenames) {
		#the name of the file can also be used as a variable names after removing test, .txt, and _
		#these are the variable names of the dataframe
		varname<-gsub("test|.txt|_","",tail(unlist(strsplit(name,"/")),n=1))
		#read the testdata file
		testdata<-read.table(name) 
		#the only difference between the name of the test data file and traing data file is the string train
		#read the training data file
		traindata<-read.table(gsub("test","train",name))
		#calculate the number of variables in the test data set. It was verified separately that test and training datasets 
		#have the same number of variables.
	    framesize<-seq(length(names(testdata)))
	    #this will be the size of the new frame
		if(count==0) {
			#first combine traing data and test data
			#rbind is needed for that
			combinedframe<-rbind(testdata,traindata)	

			columnnames<-as.list(paste(rep(varname,length(names(combinedframe))), framesize, sep=""))
			colnames(combinedframe)<-columnnames
			dataframe<-combinedframe} else 
			combinedframe<-rbind(testdata,traindata)

			columnnames<-as.list(paste(rep(varname,length(names(combinedframe))), framesize, sep=""))

			colnames(combinedframe)<-columnnames
			#now combine different types of measurements
			dataframe<-cbind(dataframe,combinedframe)
			count<-count+1
	}
    #calculate mean of the dataset when everything was combined
	lapply(dataframe,mean)
	#calculate standara deviation of the dataset when wverythin was combined
	lapply(dataframe,sd)
	#now initialize a tidy data set
	results<-lapply(dataframe[dataframe$subject==1 &  dataframe$y == 1,],mean)
	results<-as.data.frame(do.call(cbind, results))
	
	tidydataframe<-results
	
	
	#give the the tidy dataset the same varibale names as the combined dataset
	

	
	for(i in c(2:30)) {
		for (j in c(2:6)) {
		#create tidy dataset
	    results<-lapply(dataframe[dataframe$subject==i &  dataframe$y == j, ],mean)
	    results<-as.data.frame(do.call(cbind, results))
	    #print(dim(results))
		tidydataframe<-rbind(tidydataframe,results)
	}
}
    #create activity lable in the tidy dataset and the combined dataset
	activitylabel <- as.data.frame(do.call(cbind,lapply(dataframe$y, label)))
    dataframe<-cbind(dataframe,activitylabel)
	activitylabel <- as.data.frame(do.call(cbind,lapply(tidydataframe$y, label)))
	tidydataframe<-cbind(tidydataframe,activitylabel)
	#write down the tidy data set
	print(dim(tidydataframe))
	
	write.table(tidydataframe, "tidydata.txt", sep="\t",row.names=FALSE)

}

#a function that creates activity lables

label <- function(x){
  if(x==1)
   return("walk")
  if(x==2)
    return("walkup")
  if(x==3)
    return("walkdown")
  if(x==4)
    return("sit")
  if(x==5)
    return("stand")
  if(x==6)
    return("lay")
}
