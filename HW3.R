library(tidyverse)
library(tidycensus)
library(RMySQL)
library(stringr)
library(car)

#1

writeLines(readLines("/Users/anuraagramesh/.gitconfig", 10))

writeLines(readLines(".gitignore", 3))

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

agg = df %>% group_by(countycode, censustract) %>%
  summarise(employeesize = sum(employeesizelocation),
            salesvolume = sum(salesvolumelocation))
df1 = data.frame(agg)

df1$censustract = as.character(df1$censustract)

#4

host_name = 'localhost'
port_no = 3306
db_name = 'Hw3db'
u_id = 'root'

pwd = Sys.getenv('sqlpwd')

conn = RMySQL::dbConnect(RMySQL::MySQL(), host = host_name, port = port_no, user = u_id,
                         password = pwd, dbname = db_name)

RMySQL::dbWriteTable(conn, name = 'df1', df1, overwrite = TRUE, row.names = FALSE)

#5

res = dbGetQuery(conn, 'SELECT countycode, censustract, salesvolume FROM df1 ORDER BY salesvolume DESC LIMIT 10')
res

#7

df2 = read.csv('Data/AL.csv')
df2 = df2[c('FIELD19', 'FIELD20', 'FIELD22', 'FIELD45', 'FIELD64', 'FIELD65')]

colnames(df2) = c('householdwealth', 'income', 'home_value', 'state', 'countycode', 'censustract')
df2 = df2[df2$home_value != 0, ]
df2 = df2 %>% group_by(countycode, censustract) %>%
  summarise(wealth = mean(householdwealth),
            income = mean(income),
            homevalue = mean(home_value))
df2$censustract = as.character(df2$censustract)

#8

RMySQL::dbWriteTable(conn, name = 'df2', df2, overwrite = TRUE, row.names = FALSE)

#10

Sys.getenv("CENSUS_API_KEY")
census <- get_decennial(geography = 'tract', variables = c('H006001', 'H006002', 'H006003', 'H006004', 'H006005', 'H006006'), 
                        year = 2010, state = '01')
census = census %>% spread(variable, value)

census$whitepercent = census$H006002/census$H006001
census$blackpercent = census$H006003/census$H006001
census
census$countycode = as.numeric(substr(census$GEOID, 3, 5))
census$countycode = as.character(census$countycode)

census$tract = substr(census$GEOID, 6, 11)
census$tract = as.numeric(census$tract)
census$tract = as.character(census$tract)
census

RMySQL::dbWriteTable(conn, name = 'census', census, overwrite = TRUE, row.names = FALSE)

#11

combine = dbGetQuery(conn, 'SELECT d2.censustract, d2.wealth, d2.income, d2.homevalue, d1.employeesize, d1.salesvolume, c.whitepercent, c.blackpercent
                        FROM df2 d2 JOIN df1 d1 ON d2.countycode = d1.countycode AND d2.censustract = d1.censustract
                        JOIN census c ON c.countycode = d1.countycode AND c.tract = d1.censustract')
combine

#13

#Correlation matrix
cor(combine[c('wealth', 'income', 'homevalue', 'employeesize', 'salesvolume', 'blackpercent', 'whitepercent')], use = 'complete.obs')

#Model only having the percentage of white people in a tract
model_1 = lm(homevalue ~ whitepercent, data = combine)
summary(model_1)

#Model only having the percentage of black people in a tract
model_2 = lm(homevalue ~ blackpercent, data = combine)
summary(model_2)

#First model
model_3 = lm(homevalue ~ wealth + employeesize + salesvolume + blackpercent, data = combine)
summary(model_3)

#Variance Inflation Factors
vif(model_3)

#Standardizing the data
combine_standardized <- 
  combine %>% 
  mutate(wealth_s = scale(wealth),
         income_s = scale(income), 
         employeesize_s = scale(employeesize),
         salesvolume_s = scale(salesvolume),
         whitepercent_s = scale(whitepercent),
         blackpercent_s = scale(blackpercent),
         homevalue_s = scale(homevalue))

model_4 = lm(homevalue ~ wealth_s + employeesize_s + salesvolume_s + blackpercent_s, data = combine_standardized)
summary(model_4)

