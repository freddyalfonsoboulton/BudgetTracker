saveData <- function(data,sheet.key) {
  # Grab the Google Sheet
  sheet <- gs_key(sheet.key)
  # Add the data as a new row
  #browser()
  #Google Sheets counts date from 1900, R from 1970
  data[names(data) == "date"] <- as.character(as.numeric(data[names(data) == "date"]) + 25569)
  gs_add_row(sheet, input = data)
}


update_data <- function(store,category,
                        date,amount,notes,temp){
    tk <- data.frame(store = store, category = category,
               date = date,amount = amount,notes = notes)
    r <- rbind(temp,tk)
    write.csv(r,file = "Expenses.csv", row.names = F,col.names = F)
    return(r)
}

format_data <- function(df){
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  df$Category <- factor(df$Category,levels = c("Bills","Fun","Groceries","Clothes","Dining Out","Transportation","Other"))
  df$Store <- as.character(df$Store)
  df$Notes <- as.character(df$Notes)
  df$Notes[is.na(df$Notes)] <- ''
  df <- df[complete.cases(df),]
  return(df)
}

get_date_range <- function(){
  rewind <- switch(weekdays(Sys.Date()),
         Sunday = 0,
         Monday = -1,
         Tuesday = -2,
         Wednesday = -3,
         Thursday = -4,
         Friday = -5,
         Saturday = -6)
  forward <- switch (weekdays(Sys.Date()),
                     Sunday = 6,
                     Monday = 5,
                     Tuesday = 4,
                     Wednesday = 3,
                     Thursday = 2,
                     Friday = 1,
                     Saturday = 0)
  return(c(Sys.Date() + rewind,Sys.Date() + forward))
}

make.line.plot <- function(updated.data,checkbox.input = c("Bills","Fun","Groceries","Clothes","Dining Out",
                                         "Transportation","Other")){
  
  plot.df <- updated.data[updated.data$Category %in% checkbox.input,]
  plot.df$month <- as.Date(cut(plot.df$Date, breaks = "month"))
  plot.df <- plot.df %>% group_by(month) %>% summarize(total = sum(Amount), n = n())
  p <- ggplot(plot.df,aes(month,total)) + geom_point() + geom_line() +
    geom_text(aes(label = round(total)), vjust = -1.5) + ylim(0,max(plot.df$total)*1.2) +
    xlab("Month") + ylab("Total $ Spent") + ggtitle("Dollars Spent by Month")
  return(p)
}

make.bar.plot <- function(updated.data, date.range,include.bills = F, percentage = T){
  #browser()
  beginning <- as.Date(date.range[1])
  end <- as.Date(date.range[2])
  plot.df <- updated.data[updated.data$Date >= beginning & updated.data$Date <= end,]
  if(!include.bills){
    plot.df <- plot.df[plot.df$Category != "Bills",]
  }
  tot <- sum(plot.df$Amount)
  if(percentage){
    plot.df <- plot.df %>% group_by(Category) %>% summarise(total = sum(Amount)/tot)
  }
  else{
    plot.df <- plot.df %>% group_by(Category) %>% summarise(total = sum(Amount))
  }
  p <- ggplot(plot.df, aes(Category,total)) + geom_bar(stat = "identity") + 
    ggtitle(paste0("Amount Spend on Each Category from \n",
                                                        beginning," to ", end)) +
    xlab("Spending Category") + ylab("Total $ Spent")
  return(p)
}



