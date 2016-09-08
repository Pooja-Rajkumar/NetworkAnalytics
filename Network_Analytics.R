# Step 1: Load data, give values
setwd("/Applications")
data = read.table("WAN_data.txt", header = FALSE, fill = TRUE, sep = "^")
colnames(data) = c("Hostname", "UNIXTimestamp", "InterfaceIndex", "Interface Name",
                   "InMulticastPkts", "InBroadcastPkts", "OutMulticastPkts","OutBroadcastPkts",
                   "HCInOctets", "HCInUcastPkts","HCInMulticastPkts", "HCInBroadcastPkts", 
                   "HCOutOctets", "fHCOutUcastPkts", "HCOutMulticastPkts", "HCOutBroadcastPkts", 
                   "LinkUpDownTrapEnable", "HighSpeed", "PromiscuousMode", "ConnectorPresent", 
                   "Alias", "CounterDiscontinuityTime")
# We only look at HCInOctets (8) && HCOutOctets (12)
# Step 2: Look at only 17 interface index. Clean up data. 
new_data = data[which(data$InterfaceIndex == 17),]

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Step 3: Calculate the utilization 
#  $inutil = int((($newinutil - $lastinutil) * 8) / ($newtime - $lasttime));
#  $oututil = int((($newoututil - $lastoututil) * 8) / ($newtime - $lasttime));
len = length(new_data[,"HCInOctets"]) - 1
new_data$in_utilization[1] = 0
new_data$out_utilization[1] = 0
#new_data$util[1] = 0
for(index in 1:len){
  new_inutil = new_data[index+1, "HCInOctets"]
  last_inutil = new_data[index, "HCInOctets"]
  new_time = new_data[index+1, "UNIXTimestamp"]
  last_time = new_data[index, "UNIXTimestamp"]
  new_oututil = new_data[index+1, "HCOutOctets"]
  last_oututil = new_data[index, "HCOutOctets"]
  new_data$in_utilization[index+1] = (((new_inutil - last_inutil)* 8*100)/ ((300)*155))
  new_data$out_utilization[index+1] = (((new_oututil - last_oututil)* 8*100)/ ((300)*155))
  #new_data$util[index+1] = round(100*(new_data$in_utilization[index+1]/ new_data$out_utilization[index+1]))
}

#Standardize the in utilization
new_data$in_utilization = round(new_data$in_utilization/1000000) 
#Standardize the out utilization 
new_data$out_utilization = round(new_data$out_utilization/1000000)

# Convert the time into date
new_data$Regulartime = as.POSIXlt(as.numeric(as.character(new_data$UNIXTimestamp)),origin="1970-01-01",tz="GMT")
# Find weekdays 
new_data$day = weekdays(as.Date(new_data$Regulartime))
# Filter by weekdays 
filtered_set = subset(new_data, new_data$day == "Monday" | new_data$day == "Tuesday" | new_data$day == "Wednesday" |
                      new_data$day == "Thursday" | new_data$day == "Friday")
# Filter by time
super_filtered = subset(filtered_set, format(filtered_set$Regulartime, '%H') %in% c('9','10','11','12','13','14', 
                                                              '15', '16', '17'))

library(ggplot2)
library(scales)
# Chart out the trends 
plot(super_filtered$Regulartime, super_filtered$in_utilization)

#Monthly Plot
ggplot(super_filtered, aes(super_filtered$Regulartime, super_filtered$in_utilization)) +  geom_line()+ scale_x_datetime(labels = date_format("%D")) + xlab("") + ylab("Daily Views")

# Look @ daily utilization 
daily_filtered = super_filtered[super_filtered$Regulartime %in% as.Date(c('2016-02-26')),]


ggplot(super_filtered, aes(super_filtered$in_utilization) )+ geom_histogram(binwidth = 0.5)
ggplot(super_filtered, aes(super_filtered$in_utilization)) + geom_density()
ggplot(data = super_filtered) + geom_histogram( aes(super_filtered$in_utilization, ..density..)) + 
    geom_density( aes(super_filtered$in_utilization, ..density..)) +
    geom_rug( aes(super_filtered$in_utilization))
# Convert the matrix to an excel file 
write.csv(super_filtered, "filtered.csv")

without_time = super_filtered
without_time$Regulartime = format(as.POSIXct(super_filtered$Regulartime, format = '%m/%d/%Y'), format = '%m/%d/%Y')

