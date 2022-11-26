library(tidyverse)
library(RMySQL)

#2

df_func <- function(n1, n2){
  headers = read.table('Data/2020_Business_Academic_QCQ.txt', sep = ',', nrows = 1,
                       encoding = 'UTF-8')
  df = read.table('Data/2020_Business_Academic_QCQ.txt', sep = ',', skip = n1, nrows = n2 - n1, fileEncoding="latin1")
  colnames(df) = headers
  df = df %>% select('State', 'County Code', 
                'Employee Size (5) - Location', 
                'Sales Volume (9) - Location', 
                'Census Tract')
  colnames(df) = gsub('-', '', colnames(df))
  colnames(df) = gsub('\\(\\d\\)', '', colnames(df))
  colnames(df) = gsub(' ', '', colnames(df))
  colnames(df) = tolower(colnames(df))
  df <- df[complete.cases(df), ]
  
  return(df)
}

#3

df = data.frame()
for (i in seq(1, 300001, by = 20000)){
  if(i == 1){
    n1 = i
  }
  else{
    n2 = i
    df1 = df_func(n1, n2)
    n1 = n2
    df = rbind(df, df1)
  }
}

df

agg = df %>% group_by(censustract) %>%
  summarise(employeesize = sum(employeesizelocation),
            salesvolume = sum(salesvolumelocation))
df1 = data.frame(agg)

df1

#4

host_name = 'localhost'
port_no = 3306
db_name = 'HW3db'
u_id = 'root'

pwd = Sys.getenv('sqlpwd')

conn = RMySQL::dbConnect(RMySQL::MySQL(), host = host_name, port = port_no, user = u_id,
                         password = pwd, dbname = db_name)

RMySQL::dbWriteTable(conn, name = 'df1', df1, overwrite = TRUE, row.names = FALSE)

#5

res = dbGetQuery(conn, 'SELECT censustract, salesvolume FROM df1 ORDER BY salesvolume DESC LIMIT 10')
res

#7

df2 = read.csv('Data/AL.csv')
df2 = df2[c('FIELD19', 'FIELD20', 'FIELD22', 'FIELD45', 'FIELD64', 'FIELD65')]

colnames(df2) = c('householdwealth', 'income', 'home_value', 'state', 'countycode', 'censustract')
df2 = df2[df2$home_value != 0, ]
df2 = df2 %>% group_by(censustract) %>%
  summarise(wealth = mean(householdwealth),
            income = mean(income),
            homevalue = mean(home_value))
df2

#8

RMySQL::dbWriteTable(conn, name = 'df2', df2, overwrite = TRUE, row.names = FALSE)

#10
