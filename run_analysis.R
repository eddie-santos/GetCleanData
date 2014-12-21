# Loads all the relavant data for this assignment
X_train=read.table("~/Desktop/UCI HAR Dataset/train/X_train.txt")
X_test=read.table("~/Desktop/UCI HAR Dataset/test/X_test.txt")
y_train=read.table("~/Desktop/UCI HAR Dataset/train/y_train.txt",sep=",")
y_test=read.table("~/Desktop/UCI HAR Dataset/test/y_test.txt",sep=",")
subject_train=read.table("~/Desktop/UCI HAR Dataset/train/subject_train.txt",sep=",")
subject_test=read.table("~/Desktop/UCI HAR Dataset/test/subject_test.txt",sep=",")
features=read.table("~/Desktop/UCI HAR Dataset/features.txt")
activity_labels=read.table("~/Desktop/UCI HAR Dataset/activity_labels.txt")

# Gets rid of first column in each, which is the index
features=features[,-1]
activity_labels=as.factor(activity_labels[,-1])

# Combines train and test data
X=rbind(X_train,X_test)
y=rbind(y_train,y_test)
subject=rbind(subject_train,subject_test)

# Formats activity and subject data
act_type=data.frame(activity_labels[y[,1]])
subject=data.frame(as.factor(subject[,1]))
colnames(act_type)="activity"
colnames(subject)="subject"

# Gets the indices of mean and std columns
mean_ind=grep("mean",features)
meanFreq_ind=grep("meanFreq",features)
mean_ind=mean_ind[! mean_ind %in% meanFreq_ind]
std_ind=grep("std",features)
ind=sort(c(mean_ind,std_ind))

# Creates new X w/ only mean or std
X_new=X[,ind]

# Reformatting the names of the columns in X_new
colnames(X_new)=features[ind]
X_new_names=colnames(X_new)
name_splits=strsplit(X_new_names, split='-', fixed=TRUE)
is_mean_std=data.frame("",ncol=length(name_splits))

# Reorders names of columns in X_new
for(i in 1:length(name_splits)) is_mean_std[i]=name_splits[[i]][2]
is_mean_std=substr(is_mean_std,1,nchar(is_mean_std)-2)
for(i in 1:length(name_splits)) name_splits[[i]][2]=is_mean_std[i]
new_names=data.frame(",ncol=length(name_splits)")
for(i in 1:length(name_splits)) {
    if(length(name_splits[[i]])==2) new_names[i]=name_splits[[i]][1]
    if(length(name_splits[[i]])==3) new_names[i]=paste(name_splits[[i]][1],name_splits[[i]][3],sep="")
    new_names[i]=paste(new_names[i],name_splits[[i]][2],sep="-")
}
new_names=as.character(new_names)
colnames(X_new)=new_names

# Cleaned X data, writes to cleaned_data.txt
X_new=data.frame(subject,act_type,X_new)

# Generates new tiny data set with averages
nLevSub=length(levels(X_new$subject))
nLevAct=length(levels(activity_levels))
nRow=nLevSub*nLevAct
nCol=ncol(X_new)
X_tidy=X_new[1:nRow,]
r=1
for(i in 1:nLevSub) {
    for(j in 1:nLevAct) {
        
        X_tidy[r,1]=as.numeric(unique(subject)[[1]][i])
        X_tidy[r,2]=as.character(activity_labels[j])
        hold=X_new[X_new$subject==i & as.character(X_new$activity)==as.character(activity_labels[j]),]
        X_tidy[r,3:nCol]=colMeans(hold[,3:nCol])
        r=r+1
    }
}

# Writes the two data sets
write.table(X_new,"cleaned_data.txt")
write.table(X_tidy,"tiny_data.txt")

