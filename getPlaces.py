from googleplaces import GooglePlaces, types, lang
import csv
import sys
import time

#Get all the zipcodes for the requests later (done!)
#zipcode_filenames = os.listdir()

f = open('CAzipcode.csv','rb')
reader = csv.reader(f)
headers = reader.next()
column = {}
for h in headers:
	column[h] = []

for row in reader:
	for h,v in zip(headers, row):
		column[h].append(v)

zipcode_ls = column['zip']
city_ls = column['city']
state = 'CA'


#With a list of cities in a state, we use Google Places API to scrape all the dealership information with the following format
# Name 	Formatted_address  GeoLocation  Website
YOUR_API_KEY = 'AIzaSyBcNm3SKtlCLq0jld7EWs3DgRHpvSjKHgU'

google_places = GooglePlaces(YOUR_API_KEY)

company_list = ['Nissan','Toyota','Honda','Ford','Chevrolet','Smart']
for company in company_list:
	directory_str = 'DealersByState/'+company+state+'dealers.csv'

f = open('DealersByState/CA/NissanCAdealers.csv','wt')
writer = csv.writer(f)
writer.writerow(('Name','Adress','Zipcode','City','GeoLatitude','GeoLongtitude','Website'))

for zipid,city in zip(zipcode_ls[510:],city_ls[510:]):
	print zipid,city
	time.sleep(1)
	query_result = google_places.nearby_search(location= city+', MA', keyword=' Nissan new car dealership '+zipid,radius=10000)
	for place in query_result.places:
		print place.name
		place.get_details()
		writer.writerow( (place.name,place.formatted_address,zipid,city,place.geo_location['lat'],place.geo_location['lng'],place.website) )


f.close()

#for debug, if we find HTTP:500 error
# i.e use zipcode_ls.index('91730'), which returns 510, to find the index where the error occured
# then subset the remaining lists to continue our scraping
#like, zip(zipcode_ls[510:],city_ls[510:])



#print open(sys.argv[1],'rt').read()


# #for testing
# for place in query_result.places:
# 	print place.name
# 	print place.geo_location['lat'],place.geo_location['lng']

# for zipid,city in zip(zipcode_ls,city_ls):
# 	print city+', MA'+zipid