#Function to read https url

read.https <-function(url){
  csv <-getURL(url)
  read.csv(textConnection(csv))
}