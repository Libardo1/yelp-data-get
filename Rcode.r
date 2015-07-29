require(httr)
require(httpuv)
require(jsonlite)

CONSUMER_KEY = ''
CONSUMER_SECRET = ''
TOKEN = ''
TOKEN_SECRET = ''


#search business id in one area
#x[1] and y[1] is the latitude, longitude of southwest corner
#x[2] and y[2] are that of northeast corner
#npart is used to divide the field in several parts, so that we can search business id in each part
getBusiness_id = function(x, y, npart = 10){
  limit = 20
  
  lat = seq(x[1], x[2], length.out = npart)
  long = seq(y[1],y[2], length.out = npart)
  
  bus_id = replicate((npart-1)^2, list())
  m = 1
  
  for (i in 1:(npart - 1)){
    
    for (j in 1:(npart - 1)){
      sw1 = lat[i]
      sw2 = long[j]
      ne1 = lat[i+1]
      ne2 = long[j+1]
      bound = paste(sw1,',',sw2,'|',ne1,',',ne2,sep='')
      yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&term=restaurant&bounds=",bound)
      locationdata=GET(yelpurl, sig)
      locationdataContent = content(locationdata)
      ids = length(locationdataContent$businesses)
      if(ids > 0){
        bus_id[[m]] = replicate(ids, list())
        for(k in 1:ids){
          bus_id[[m]][k] = locationdataContent$businesses[[k]]$id
        }
      }
      m = m + 1
      Sys.sleep(0.2)
    }
  }
  ids = unlist(bus_id)
  return(unique(ids))
}





#using getBusiness_id() to get business id
#davis downtown 1-5 street
x = c(38.540628, 38.548550)
y = c(-121.746982, -121.734494)


#south sac

x = c(38.563529, 38.599359)
y = c(-121.547404, -121.512385)


#sac

x =c(38.375166, 38.596071)
y = c(-121.508265,-121.416941)

#north sac

x=c(38.598754, 38.646499)
y = c(-121.539164, -121.412134)

x = c(38.573662, 38.791817)
y = c(-121.414881, -121.193781)


# vacaville
x =c(38.316332, 38.417007)
y = c(-122.018185, -121.933556)


#fairfield
x = c(38.229010, 38.286701)
y = c(-122.083882, -121.988095)


#sfo1
x = c(37.716688, 37.816021)
y = c(-122.509945, -122.388409)

#sfo2
x = c(37.630852, 37.716688)
y = c(-122.509945, -122.388408)

myapp = oauth_app("YELP", key=CONSUMER_KEY, secret=CONSUMER_SECRET)
sig=sign_oauth1.0(myapp, token=TOKEN,token_secret=TOKEN_SECRET)

ids = getBusiness_id(x,y,10)

#save business id to txt
write(ids, file = "business_id.txt",
      ncolumns = 1,
      append = FALSE, sep = " ")

#save business id to rda file
save(ids, file = "business_id.rda")




#using R to get business data(by business id)
bus_id = ids
content = replicate(length(bus_id), list())

for(i in 1:length(bus_id)){
  yelpurl <- paste0("http://api.yelp.com/v2/business/",bus_id[i])
  locationdata=GET(yelpurl, sig)
  content[[i]]= content(locationdata)
}
save(content,file = "business_data.rda")





#the following code is used to read json data in txt
library("rjson")
x = readLines("data.txt")
data = replicate(length(x), list())
for(i in 1:length(x)){
  data[[i]] = fromJSON(x[i])
}



