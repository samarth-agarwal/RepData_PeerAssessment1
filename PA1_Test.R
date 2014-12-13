data<-read.csv("C:/Documents and Settings/Samarth/RepData_PeerAssessment1/activity.csv")
dataset<-data[!is.na(data$steps),]

#Total number of steps taken per day

barplot(tapply(dataset$steps,dataset$date,sum))

means<-tapply(dataset$steps,dataset$date,sum)
means<-means[!is.na(means)]
mean<-mean(means)
mean

medians<-tapply(dataset$steps,dataset$date,sum)
medians<-medians[!is.na(medians)]
median<-median(medians)
median

#Average daily acivity pattern
dataset_daily<-data[!duplicated(data$interval),]
avg_steps=tapply(dataset$steps,dataset$interval,mean)
plot(dataset_daily$interval,avg_steps,type='l')

names(avg_steps[which.max(avg_steps)])

#Input missing values
sum(is.na(data$steps))

avg_steps_frame<-data.frame(interval=names(avg_steps),avg_steps)
dataset_input<-data
for(i in 1:17568)
{
    if(is.na(dataset_input[i,1]))
        dataset_input[i,1]<-avg_steps_frame[avg_steps_frame$interval==dataset_input[i,3],2]
}

##barplot(tapply(dataset$steps,dataset$date,sum))

barplot(tapply(dataset_input$steps,dataset_input$date,sum))

means_i<-tapply(dataset_input$steps,dataset_input$date,sum)
means_i<-means_i[!is.na(means_i)]
mean_i<-mean(means_i)
mean_i

medians_i<-tapply(dataset_input$steps,dataset_input$date,sum)
medians_i<-medians_i[!is.na(medians_i)]
median_i<-median(medians_i)
median_i

#Weekend vs Weekdays
day<-character(0)
for(i in 1:17568)
{
    date<-dataset_input[i,2]
    weekday<-weekdays(as.Date(date))
    
    if(weekday=="Saturday" || weekday=="Sunday")
        day<-rbind(day,"Weekend")
    
    else
        day<-rbind(day,"Weekday")    
}
dataset_weeks<-cbind(dataset_input,day)

wd<-data.frame(steps=numeric(0),data=character(0),interval=numeric(0), day=character(0))
we<-data.frame(steps=numeric(0),data=character(0),interval=numeric(0), day=character(0))

for(i in 1:17568)
{
    if(dataset_weeks[i,4]=="Weekday")
        wd<-rbind(wd,dataset_weeks[i,])
    
    else
        we<-rbind(we,dataset_weeks[i,])
}

avg_steps_wd=tapply(wd$steps,wd$interval,mean)
avg_steps_we=tapply(we$steps,we$interval,mean)

steps_wd<-data.frame(steps=avg_steps_wd,interval=names(avg_steps_wd))
steps_we<-data.frame(steps=avg_steps_we,interval=names(avg_steps_we))

steps_wd<-cbind(steps_wd,day="Weekday")
steps_we<-cbind(steps_we,day="Weekend")
steps_week<-rbind(steps_we,steps_wd)

xyplot(steps_week$steps~steps_week$interval|steps_week$day,type='l')
