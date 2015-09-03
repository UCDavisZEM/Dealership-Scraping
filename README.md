# ${Web Scraping &Visualization for the vehicle sales market in U.S}

This project aims to apply web scraping to collect vehicle information from local dealerships websites in the U.S. RShiny Dashboard
integrates interactive visualization to show the vehicle sales market in U.S from the local dealerships perspective. This project is
started at May,2015, and supervised by Dr.Gustavo Collantes at the UC Davis Policy Institute. This project provide scientific evidence-based 
supports for the [Zero Emission Market Acceleration Partnerships Initiative](http://zeroemissionmap.ucdavis.edu/) led by UC Davis. 

## Strategies

1. We collect local dealership information from the manufacturers' websites to collect all nationwide dealerships information about dealer name, address, website
link and even inventory links(if they exists)
2. We use Google Place API to further integrate the information of geo-locations(Latitude,Longitude) and inventory links(if NA from the last step).
3. Through our experiments and test case, we summarise and break down web-scraping cases based on their common url patterns in inventory links.
4. After we tackled each case study of web scraping, we can integrate them to a scraper.
5. We design our visualization and Shiny dashboard and delpoy on the RShiny server.

## Credits

[Jiaping Zhang](https://github.com/jpzhangvincent)
[Jianshi Zhang](https://github.com/JaneJianshi)

## Further Work

- Real-time Data Collection
- Database Management

