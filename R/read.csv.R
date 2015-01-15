#Function to read https url-default conditions are those of read.csv

read.https <-function(url,header = TRUE, sep = ",", quote = "\"", dec = ".", 
fill = TRUE, comment.char = "", ...){
  csv <-getURL(url)
  read.table(textConnection(csv),header = header, sep = sep, quote = quote, 
             dec = dec, fill = fill, comment.char = comment.char, ...)
}