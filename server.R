#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #open
  #Read file
  
  highway <- read.csv(file.choose(),header = T)
  str(highway)
  
  #Build Corpus
  
  library(tm)
  library(stopwords)
  corpus <- iconv(highway$text,to = 'UTF-8', sub = "byte")
  corpus <- Corpus(VectorSource(corpus))
  
  #Cleaning text
  corpus <- tm_map(corpus, tolower)
  #inspect(corpus)
  
  #Remove comar
  corpus <- tm_map(corpus, removePunctuation)
  #inspect(corpus)
  
  #remove number
  corpus <- tm_map(corpus, removeNumbers)
  #inspect(corpus)
  
  
  #Exception word
  exceptions <- c('dari','dan','di','ke','masa','maklumat')
  
  my_stopwords <- setdiff(stopwords("ms", source = "stopwords-iso"), exceptions)
  
  #stopword
  cleanset <- tm_map(corpus,removeWords,my_stopwords)
  cleanset <- tm_map(corpus, removeWords, stopwords('english'))
  #inspect(cleanset)
  
  #Remove url
  removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
  cleanset <- tm_map(cleanset, content_transformer(removeURL))
  #inspect(corpus)
  
  #remove word
  cleanset <- tm_map(cleanset, removeWords, c('am','pm','amp'))
  
  #remove whitespace
  cleanset <- tm_map(cleanset,stripWhitespace)
  #inspect(cleanset)
  
  #insert filter tweet to data
  highwayData <- cleanset
  
  #check wheather word are in the data
  #highway list from rawang to taiping (north)
  rawang <- regexpr("rawang",highwayData[1:100])
  bukitBeruntung <- regexpr("bukit beruntung",highwayData[1:100])
  bukitTegar <- regexpr("bukit tegar",highwayData[1:100])
  lembahBeringin <- regexpr("lembah beringin",highwayData[1:100])
  tanjungMalim <- regexpr("tanjung malim",highwayData[1:100])
  behrang <- regexpr("behrang",highwayData[1:100])
  slimRiver <- regexpr("slim river",highwayData[1:100])
  sungkai <- regexpr("sungkai",highwayData[1:100])
  bidor <- regexpr("bidor",highwayData[1:100])
  tapah <- regexpr("tapah",highwayData[1:100])
  gopeng <- regexpr("gopeng",highwayData[1:100])
  simpangPulai <- regexpr("simpang pulai",highwayData[1:100])
  jelapang <- regexpr("jelapang",highwayData[1:100])
  kualaKangsar <- regexpr("kuala langsar",highwayData[1:100])
  changkatJering <- regexpr("changkat jering",highwayData[1:100])
  taiping <- regexpr("taiping",highwayData[1:100])
  
  
  #checking data
  if(rawang > 1){
    rawangTrafic <- print("Traffic Busy")
  }else{
    rawangTrafic <- print("Traffic Clear")
  }
  
  if(bukitBeruntung > 1){
    bukitBeruntungTrafic <- print("Traffic Busy")
  }else{
    bukitBeruntungTrafic <- print("Traffic Clear")
  }
  
  if(bukitTegar > 1){
    bukitTegarTrafic <- print("Traffic Busy")
  }else{
    bukitTegarTrafic <- print("Traffic Clear")
  }
  
  if(lembahBeringin > 1){
    lembahBeringinTrafic <- print("Traffic Busy")
  }else{
    lembahBeringinTrafic <- print("Traffic Clear")
  }
  
  if(tanjungMalim > 1){
    tanjungMalimTrafic <- print("Traffic Busy")
  }else{
    tanjungMalimTrafic <- print("Traffic Clear")
  }
  
  if(behrang > 1){
    behrangTrafic <- print("Traffic Busy")
  }else{
    behrangTrafic <- print("Traffic Clear")
  }
  
  if(slimRiver > 1){
    slimRiverTrafic <- print("Traffic Busy")
  }else{
    slimRiverTrafic <- print("Traffic Clear")
  }
  
  if(sungkai > 1){
    sungkaiTrafic <- print("Traffic Busy")
  }else{
    sungkaiTrafic <- print("Traffic Clear")
  }
  
  if(bidor > 1){
    bidorTrafic <- print("Traffic Busy")
  }else{
    bidorTrafic <- print("Traffic Clear")
  }
  
  if(tapah > 1){
    tapahTrafic <- print("Traffic Busy")
  }else{
    tapahTrafic <- print("Traffic Clear")
  }
  
  if(gopeng > 1){
    gopengTrafic <- print("Traffic Busy")
  }else{
    gopengTrafic <- print("Traffic Clear")
  }
  
  if(simpangPulai > 1){
    simpangPulaiTrafic <- print("Traffic Busy")
  }else{
    simpangPulaiTrafic <- print("Traffic Clear")
  }
  
  if(jelapang > 1){
    jelapangTrafic <- print("Traffic Busy")
  }else{
    jelapangTrafic <- print("Traffic Clear")
  }
  
  if(kualaKangsar > 1){
    kualaKangsarTrafic <- print("Traffic Busy")
  }else{
    kualaKangsarTrafic <- print("Traffic Clear")
  }
  
  if(changkatJering > 1){
    changkatJeringTrafic <- print("Traffic Busy")
  }else{
    changkatJeringTrafic <- print("Traffic Clear")
  }
  
  if(taiping > 1){
    taipingTrafic <- print("Traffic Busy")
  }else{
    taipingTrafic <- print("Traffic Clear")
  }
  
  
  #create dataframe
  HighwayName <- c("Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)",
                   "Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)",
                   "Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)",
                   "Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)","Lebuhraya Utara Selatan Arah Utara(PLUS)")
  
  KM <- c("Rawang ke Bukit Berungtung","Bukit Beruntung ke Bukit Tegar","Bukit Tegar ke Lembah Beringin",
          "Lembah Beringin ke Tanjung Malim","Tanjung Malim ke Behrang","Behrang ke Slim River",
          "Slim River ke Sungkai","Sungkai ke Bidor","Bidor ke Tapah","Tapah ke Gopeng","Gopeng ke Simpang Pulai",
          "Simpang Pulai ke Jelapang","Jelapang ke Kuala Kangsar","Kuala Kangsar ke Changkkat Jering","Changkat Jering ke Taiping",
          "Taiping ke Bukit Merah")
  
  TrafficStatus <- c(rawangTrafic,bukitBeruntungTrafic,bukitTegarTrafic,lembahBeringinTrafic,
                     tanjungMalimTrafic,behrangTrafic,slimRiverTrafic,sungkaiTrafic,
                     bidorTrafic,tapahTrafic,gopengTrafic,simpangPulaiTrafic,
                     jelapangTrafic,kualaKangsarTrafic,changkatJeringTrafic,taipingTrafic)
  
  #insert data frame
  plusTrafics = data.frame(HighwayName,KM,TrafficStatus)
  
  #print data frame
  print(plusTrafic)
  
  #close
   
  output$highwayTrafficStatus <- renderTable({
    highwayFilter <- subset(plusTrafics,plusTrafics$HighwayName == input$inTrafic)
  })
  
})
