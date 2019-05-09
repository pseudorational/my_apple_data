library(XML)
data = xmlParse('file:///C:/Users/vlala/Downloads/export/apple_health_export/export.xml')
#xmlAttrsToDataFrame(xml["//Record"])
df =  XML:::xmlAttrsToDataFrame(data["//Record"])
table(df$type)
write.csv(df,'iphoneData.csv')

df = read.csv('iphoneData.csv')

library(lubridate)
df$value = as.numeric(df$value)
df$endDate = ymd_hms(df$endDate,tz='America/New_York')
df$startDate = ymd_hms(df$startDate,tz='America/New_York')
df$creationDate = ymd_hms(df$creationDate,tz='America/New_York')

df$date = date(df$endDate)
df$year = year(df$endDate)
df$month = month(df$endDate)
df$month_label = month(df$endDate,label = T,abbr = T)
df$day_of_month = day(df$endDate)
df$day_of_week = wday(df$endDate,label = T,abbr = T)
df$hour = hour(df$endDate)
df$minute = minute(df$endDate)
df$second = second(df$endDate)

df$value = as.numeric(as.character(df$value))

levels(df$type) = unlist(lapply(X = levels(df$type), 
                                FUN = function(x)gsub(pattern = 'HKQuantityTypeIdentifier',replacement ='',x = x)))

library(dplyr)
table(df$day_of_week)
names(df)
levels(df$sourceName) = c('health','phone','watch')
levels(df$type) = c('StandHour','HighHeartRateEvent','MindfulSession','ActiveEnergyBurned','AppleExerciseTime','BasalEnergy')
df[df$type=='HKQuantityTypeIdentifierStepCount',c('type','value')]

df%>%
  filter(sourceName == 'watch'& type=='HKQuantityTypeIdentifierStepCount'& year>=2018)%>%
  group_by(date,day_of_week)%>%
  summarize(steps = sum(value))%>%
  ungroup()%>%
  group_by(day_of_week)%>%
  summarize(avg_steps = mean(steps))%>%
  ungroup()
  
df%>%
  filter(sourceName == 'watch'& type=='HKQuantityTypeIdentifierDistanceWalkingRunning'& year>=2018)%>%
  group_by(date,day_of_week)%>%
  summarize(steps = sum(value))%>%
  ungroup()%>%
  group_by(day_of_week)%>%
  summarize(avg_steps = mean(steps))%>%
  ungroup()

df%>%
  filter(sourceName == 'watch')%>%
  group_by(hour,type)%>%
  summarize(sum_value = sum(value,na.rm=T))%>%
  ungroup()%>%
  group_by(type,hour)%>%
  summarize(avg_value = mean(sum_value))%>%
  ungroup()%>%
  ggplot(aes(x=day_of_week,y=avg_value))+
            geom_bar(stat='dodge',stat='summary',fun.y=mean(na.rm=T))+
            facet_wrap(~type)


df%>%
  filter(sourceName == 'watch')%>%
  group_by(date,day_of_week,type)%>%
  summarize(sum_value = sum(value,na.rm=T))%>%
  ungroup()%>%
  group_by(type,day_of_week)%>%
  summarize(avg_value = mean(sum_value))%>%
  ungroup()%>%
  filter(type!='HKCategoryTypeIdentifierAppleStandHour'&type!='HKCategoryTypeIdentifierHighHeartRateEvent'&type!='HKCategoryTypeIdentifierMindfulSession')%>%
  ggplot(aes(x=day_of_week,y=avg_value))+
  geom_col()+facet_wrap(~type,scales = 'free_y')
           
           

df%>%
  filter(sourceName == 'watch')%>%
  group_by(date, month_label,type)%>%
  summarize(sum_value = sum(value,na.rm=T))%>%
  ungroup()%>%
  group_by(month_label,type)%>%
  summarize(avg_value = mean(sum_value))%>%
  ungroup()%>%
  filter(type!='HKCategoryTypeIdentifierAppleStandHour'&type!='HKCategoryTypeIdentifierHighHeartRateEvent'&type!='HKCategoryTypeIdentifierMindfulSession')%>%
  ggplot(aes(x=month_label,y=avg_value))+
  geom_col()+coord_flip()+facet_wrap(~type,scales = 'free_x')   

df%>%
  filter(sourceName == 'watch')%>%
  group_by(date,day_of_week,type)%>%
  summarize(sum_value = sum(value,na.rm=T))%>%
  ungroup()%>%
  group_by(type,day_of_week)%>%
  summarize(avg_value = mean(sum_value))%>%
  ungroup()%>%
  filter(type!='HKCategoryTypeIdentifierAppleStandHour'&type!='HKCategoryTypeIdentifierHighHeartRateEvent'&type!='HKCategoryTypeIdentifierMindfulSession')%>%
  ggplot(aes(x=day_of_week,y=avg_value))+
  geom_col()+coord_flip()+facet_wrap(~type,scales = 'free_x')   

df%>%
  filter(sourceName == 'watch')%>%
  group_by(hour,type)%>%
  summarize(sum_value = sum(value,na.rm=T))%>%
  ungroup()%>%
  group_by(type,hour)%>%
  summarize(avg_value = mean(sum_value))%>%
  ungroup()%>%
  filter(type!='HKCategoryTypeIdentifierAppleStandHour'&type!='HKCategoryTypeIdentifierHighHeartRateEvent'&type!='HKCategoryTypeIdentifierMindfulSession')%>%
  ggplot(aes(x=hour,y=avg_value,fill=cut(hour,breaks = 6*0:4,labels=c('<6am','6-12','1-6','>7pm'),right = F)))+
  geom_col()+coord_flip()+facet_wrap(~type,scales = 'free_x')+guides(fill=guide_legend(title = 'time'))
  

