# extract only mean and standard deviation of measurements

# read features.txt
featureFile="UCI HAR Dataset/features.txt";
featureDf=read.table(featureFile,header=F);
ncol(featureDf)
nrow(featureDf)

indVect=numeric()
desVect=character()
count=1;
for(i in 1:nrow(featureDf))
{
  found=grep("mean|std",as.character(featureDf[i,2]),
             ignore.case=T)
  if(length(found)!=0)
  {
    indVect[count]=featureDf[i,1]
    desVect[count]=as.character(featureDf[i,2])
    count=count+1
  }
}


# read the X_train.txt file
xtFile="UCI HAR Dataset/train/X_train.txt"
xtDf=read.table(xtFile,header=F);
ncol(xtDf)
nrow(xtDf)

# read the X_test.txt file
xteFile="UCI HAR Dataset/test/X_test.txt"
xteDf=read.table(xteFile,header=F);
ncol(xteDf)
nrow(xteDf)

# merge qualified varaibles in xtDf and xteDf
x=data.frame(matrix(nrow=nrow(xtDf)+nrow(xteDf),ncol=length(indVect)))
x[1:nrow(xtDf),]=xtDf[,indVect]
x[(nrow(xtDf)+1):dim(x)[1],]=xteDf[,indVect]

# assign column names to x
names(x)=desVect

# create a folder to hold the final data
dir.create(file.path("UCI HAR Dataset", "merge"))
dir.create(file.path("UCI HAR Dataset/merge","dataset1"))

# write x to a text file
xFile="UCI HAR Dataset/merge/dataset1/X.txt"
write.table(x,xFile,row.names=F)

# read subject_train.txt file
stFile="UCI HAR Dataset/train/subject_train.txt"
stDf=read.table(stFile,header=F);
ncol(stDf)
nrow(stDf)

# read subject_test.txt file
steFile="UCI HAR Dataset/test/subject_test.txt"
steDf=read.table(steFile,header=F);
ncol(steDf)
nrow(steDf)

# merge stDf and steDf
subDf=data.frame(matrix(nrow=nrow(stDf)+nrow(steDf),ncol=1))
subDf[1:nrow(stDf),]=stDf[,]
subDf[(nrow(stDf)+1):dim(subDf)[1],]=steDf[,]

# write subject to a text file
names(subDf)="subject"
subjectFile="UCI HAR Dataset/merge/dataset1/subject.txt"
write.table(subDf,subjectFile,row.names=F)

# read y_train.txt file
ytFile="UCI HAR Dataset/train/y_train.txt"
ytDf=read.table(ytFile,header=F);
ncol(ytDf)
nrow(ytDf)

# read y_test.txt file
yteFile="UCI HAR Dataset/test/y_test.txt"
yteDf=read.table(yteFile,header=F);
ncol(yteDf)
nrow(yteDf)

# create a character vector to label activities
act=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS",
      "SITTING","STANDING","LAYING")

# merge ytDf and yteDf
y=data.frame(matrix(nrow=nrow(ytDf)+nrow(yteDf),ncol=1))
for (i in 1:nrow(ytDf))
{
  y[i,]=act[ytDf[i,1]];
}

for(i in (nrow(ytDf)+1):nrow(y))
{
  y[i,]=act[yteDf[(i-nrow(ytDf)),1]];
}

# write y to y.txt
names(y)="activity"
yFile="UCI HAR Dataset/merge/dataset1/y.txt"
write.table(y,yFile,row.names=F)

# create a data frame to hold all the data
allDf=data.frame(matrix(nrow=(nrow(xtDf)+nrow(xteDf)),
                        ncol=(ncol(xtDf)+2)))
# fill X data into allDf
allDf[1:nrow(xtDf),1:ncol(xtDf)]=xtDf[,]
allDf[(nrow(xtDf)+1):(nrow(xtDf)+nrow(xteDf)),1:ncol(xtDf)]=xteDf[,]

# fill subject data into allDf
allDf[,(ncol(xtDf)+1)]=subDf

# fill y(activity) data into allDf
allDf[1:nrow(ytDf),ncol(allDf)]=ytDf[,]
allDf[(nrow(ytDf)+1):(nrow(ytDf)+nrow(yteDf)),ncol(allDf)]=yteDf[,]

# sort allDf by subject and then by activity
sAllDf=allDf[order(allDf[,(ncol(allDf)-1)],allDf[,(ncol(allDf))]),]

tmpAllDf=data.frame(matrix(nrow=nrow(allDf),ncol=ncol(allDf)))
tmpAllDf[,ncol(allDf)-1]=sAllDf[,ncol(allDf)-1]
tmpAllDf[,ncol(allDf)]=sAllDf[,ncol(allDf)]

# calculate averages
for(i in 1:(ncol(allDf)-2))
{
  tmpAllDf[,i]=ave(sAllDf[,i],sAllDf[,ncol(allDf)-1],sAllDf[,ncol(allDf)])
}

# retrieve distince rows
finalAllDf=unique(tmpAllDf)

# write finalAllDf into X, subject, and y files
# add column names
colnames=c(as.character(featureDf[,2]),"subject","activity")
names(finalAllDf)=colnames

# create a folder to hold the data
dir.create(file.path("UCI HAR Dataset/merge","dataset2"))
x2File="UCI HAR Dataset/merge/dataset2/X.txt"
write.table(finalAllDf[,1:(ncol(allDf)-2)],x2File,row.names=F)

subject2File="UCI HAR Dataset/merge/dataset2/subject.txt"
write.table(finalAllDf[,ncol(allDf)-1],subject2File,row.names=F,col.names="subject")

y2File="UCI HAR Dataset/merge/dataset2/y.txt"
act2=data.frame(matrix(nrow=nrow(finalAllDf),ncol=1))
for(i in 1:nrow(finalAllDf))
{
  act2[i,1]=act[finalAllDf[i,ncol(allDf)]]
}
names(act2)="activity"
write.table(act2,y2File,row.names=F)

