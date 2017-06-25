
#load packages
libraries<-c("gbm","dplyr")
sapply(libraries,require,character.only=TRUE)


#####################################TRAIN DATA####################################################

raw_data<-read.csv("F:\\AnalyticsVidya\\BigmartSales\\data\\Train.csv",stringsAsFactors=TRUE)

table(raw_data$Outlet_Type,raw_data$Outlet_Size)
#from the table we can draw conclusion that all the grocery store of undefined size are small in size
train_missing_outlet_size_index_1<-which(raw_data$Outlet_Size==""&raw_data$Outlet_Type=="Grocery Store")
raw_data$Outlet_Size[train_missing_outlet_size_index_1]<-"Small"

table(raw_data$Outlet_Location_Type,raw_data$Outlet_Size)
#from the table we can draw conclusion that all undefined store size in Tier 2 cities are Small in size 
train_missing_outlet_size_index_2<-which(raw_data$Outlet_Size==""&raw_data$Outlet_Location_Type=="Tier 2")
raw_data$Outlet_Size[train_missing_outlet_size_index_2]<-"Small"

#Some EDA
raw_data$Outlet_Establishment_Year<- as.factor(raw_data$Outlet_Establishment_Year)
raw_data$Item_Fat_Content[raw_data$Item_Fat_Content=="LF"]<-"Low Fat"
raw_data$Item_Fat_Content[raw_data$Item_Fat_Content=="low fat"]<-"Low Fat"
raw_data$Item_Fat_Content[raw_data$Item_Fat_Content=="reg"]<-"Regular"

#From the table we can see that there are some missing values in item weight
temp1<-raw_data[order(raw_data$Item_Identifier),c(1,2)]
View(temp1)
#From table we can see that items with same identifier has same weight

#Rows containing missing Item Weight
temp_index<-which(is.na(raw_data$Item_Weight))


#manipulating  missing item weight
for(i in temp_index){
	identifier<-raw_data$Item_Identifier[i]
	temp<-which(raw_data$Item_Identifier==identifier)
	raw_data$Item_Weight[i]<-mean(raw_data$Item_Weight[temp],na.rm=TRUE)
}

#Still there are some item with missing weight
missing_weight_index<-which(is.na(raw_data$Item_Weight))

raw_data$Item_Weight[missing_weight_index]<-mean(raw_data$Item_Weight,na.rm = TRUE)


raw_data<-droplevels(raw_data)

#As 'Item_Weight' columns represents item identifier so we can remove 'Item_Identifier' 
raw_data_train<-raw_data[,-1]
##################################TEST DATA########################################################

test_data<-read.csv("F:\\AnalyticsVidya\\BigmartSales\\data\\Test.csv",stringsAsFactors=TRUE)

table(test_data$Outlet_Type,test_data$Outlet_Size)
#from the table we can draw conclusion that all the grocery store of undefined size are small in size
test_missing_outlet_size_index_1<-which(test_data$Outlet_Size==""&test_data$Outlet_Type=="Grocery Store")
test_data$Outlet_Size[test_missing_outlet_size_index_1]<-"Small"

table(test_data$Outlet_Location_Type,test_data$Outlet_Size)
#from table we can decide that All undefined outlets in tier 2 are small in size
test_missing_outlet_size_index_2<-which(test_data$Outlet_Size==""&test_data$Outlet_Location_Type=="Tier 2")
test_data$Outlet_Size[test_missing_outlet_size_index_2]<-"Small"

#Some EDA
test_data$Outlet_Establishment_Year<- as.factor(test_data$Outlet_Establishment_Year)
test_data$Item_Fat_Content[test_data$Item_Fat_Content=="LF"]<-"Low Fat"
test_data$Item_Fat_Content[test_data$Item_Fat_Content=="low fat"]<-"Low Fat"
test_data$Item_Fat_Content[test_data$Item_Fat_Content=="reg"]<-"Regular"

#Rows containing missing Item Weight
temp_index_test<-which(is.na(test_data$Item_Weight))
for(j in temp_index_test){
	identifier_test<-test_data$Item_Identifier[j]
	temp_test<-which(test_data$Item_Identifier==identifier_test)
	test_data$Item_Weight[j]<-mean(test_data$Item_Weight[temp_test],na.rm=TRUE)
}

#Rows containing missing Item Weight
missing_weight_index_test<-which(is.na(test_data$Item_Weight))
#Still there are some observations with missing item weight

k1<-group_by(test_data[-missing_weight_index_test,],Item_Type)
j1<-summarize(k1,mean_weight=mean(Item_Weight,na.rm=TRUE))

#manipulating  missing item weight
for(k in missing_weight_index_test ){
	item_type_identifier_test1<-test_data$Item_Type[k]
	temp_test_1<-which(j1$Item_Type==item_type_identifier_test1)
	test_data$Item_Weight[k]<-j1[[2]][[temp_test_1]]
}
#As 'Item_Weight' columns represents item identifier so we can remove 'Item_Identifier' 
test_data<-test_data[,-1]

test_data<-droplevels(test_data)
#########################Prediction########################################
#Here we are going to use boosting.after crossvalidation we have found the optimised hyperparameter value
set.seed(101)
gbm.predict<-gbm(Item_Outlet_Sales~.,data=raw_data_train,distribution="gaussian",n.trees =60 , interaction.depth =2,shrinkage =0.14)
value<-predict(gbm.predict,newdata =test_data,n.trees = 60)

#Some values are negative.so to reduce our prediction error we can assume that that item sales volume are zero
temp_value_index<-which(value<0)
value[temp_value_index]<-rep(0,length(temp_value_index))

#Writing output file
test_data_temp<-read.csv("F:\\AnalyticsVidya\\BigmartSales\\data\\Test.csv",stringsAsFactors=TRUE)
write_output<-cbind(test_data_temp[,c(1,7)],Item_Outlet_Sales=value)
output_file<-write.csv(write_output,"SampleSubmission.csv",row.names=FALSE)
