# Test visualisation in a handsontable
# install.packages('rhandsontable')

library(rhandsontable)

df_test = data.frame(x = 1:5, y = rep(TRUE, 5))

rhandsontable(df_test, 
              rowHeaders = c('1/1/2021', '2/1/2021', '3/1/2021', '4/1/2021', '5/1/2021'),
              width = 900, height = 300)
