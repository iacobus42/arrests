db = "/path/to/where/you/want/arrests.db"
keys = ["your google maps api key"]


######## EVERYTHING BELOW THIS LINE SHOULD NOT NEED TO BE CHANGED ##############

from bs4 import BeautifulSoup
import sqlite3 as lite
import re
import time
import urllib
import json

start = time.time()
# functions ################################################################
def genericClean(cell):
  cString = str(cell)
  cString = cString.replace("<br/>", " ")
  cString = re.sub("\<\/?td\>", "", cString)
  cString = re.sub("\\t", "", cString)
  cString = cString.replace(",", "")
  cString = cString.replace("\n", "").strip().lower()
  cString = re.sub('\s+', ' ', cString).strip()
  cString = cString.replace("'", "")
  return(cString)
  
def getLatLng(address, key):
  urlBase = "https://maps.googleapis.com/maps/api/geocode/json?"
  iowaCity = address.rfind("iowa city") != -1
  uh = address.rfind("university heights") != -1
  cv = address.rfind("coralville") != -1
  if iowaCity or uh or cv:
    inbounds = 1
  else:
    inbounds = 0
  if inbounds:
      address = urllib.quote_plus(address, ",")
      address = address.replace("'", "")
      qURL = urlBase + "address=" + address + key
      results = json.load(urllib.urlopen(qURL))
      attempt = 1
      try:
        lng = results['results'][0]['geometry']['location']['lng']
        lat = results['results'][0]['geometry']['location']['lat']
      except:
        lng = "NULL"
        lat = "NULL"
  else:
    lat = "NULL"
    lng = "NULL"
    attempt = 0
  return([attempt, lat, lng])

def cleanCharges(cell):
  chargesString = str(cell)
  chargesString = re.sub("\<\/?td\>", "", chargesString)
  chargesString = re.sub("\\t", "", chargesString)
  chargesString = chargesString.replace("\n", "")
  chargesString = chargesString.split(",")
  nCharges = len(chargesString)
  for i in range(nCharges):
    chargesString[i] = chargesString[i].strip().lower()
  return(chargesString)

def cleanDate(cell):
  dateString = genericClean(cell)
  dateString = dateString.split(" ")
  month = str(time.strptime(dateString[0].strip(), "%B").tm_mon)
  day = dateString[1].strip()
  year = dateString[2].strip()
  date = [month, day, year]
  return(date)
  
# table/variable defs ##########################################################
tattrs = {'id':'arrest'}
html = urllib.urlopen("http://data.press-citizen-media.com/arrests/searchresults.php")

# main #########################################################################
con = lite.connect(db)
cur = con.cursor()
cur.execute("DROP TABLE IF EXISTS CRIMINAL") 
cur.execute("DROP TABLE IF EXISTS CHARGES")

cur.execute("CREATE TABLE CRIMINAL \
  (aid int primary key, \
  lastName char, \
  firstName char, \
  arrestLocation char, \
  address char, \
  month int, \
  day int, \
  year int, \
  lat float, \
  lng float)")

cur.execute("CREATE TABLE CHARGES \
  (aid int, \
  charge char)")

soup = BeautifulSoup(html)
arrestsTable = soup.find("table", attrs = tattrs)
arrests = arrestsTable.find_all("tr")
nArrests = len(arrests)

aid = 1
keyN = 0
key = keys[keyN]
locateQueries = 0
innerTime = 0
for row in range(1, nArrests):
  if aid % 500 == 0:
    print aid
  arrest = arrests[row]
  # parse row
  cells = arrest.find_all("td")
  # name is stored as "lastName, firstName" in html, splitting into 
  # first and last
  name = genericClean(cells[0]).split()
  firstName = name[1].strip()
  lastName = name[0].strip()
  # address of person arrested, don't process but keep just in case
  address = genericClean(cells[1])
  # address of arrest location, clean for geocoding
  arrestLocation = genericClean(cells[2])
  outTime = time.time()
  geocodeArrest = getLatLng(arrestLocation, key)
  innerTime = time.time()
  locateQueries = locateQueries + geocodeArrest[0]
  # rate limiting to 5/s and 2,500 / day for Google
  if aid > 1 and (outTime - innerTime <= 0.25):
    time.sleep(0.25 - (outTime - innerTime))
  if locateQueries == 2495:
    locateQueries = 0
    time.sleep(24 * 60 * 60)
  # not all records have a date
  try:
    arrestDate = cleanDate(cells[3])
  except:
    arrestDate = ["NULL", "NULL", "NULL"]
  # multiple charges allowed so split on ",", needs own cleaning function
  charges = cleanCharges(cells[4])
  # insert arrest metadata into table
  command = "INSERT INTO CRIMINAL VALUES(" + str(aid) + ",'" + lastName 
  command = command + "', '" + firstName + "','" + arrestLocation + "','" 
  command = command + address + "'," + arrestDate[0] + "," + arrestDate[1] 
  command = command + "," + arrestDate[2] + "," + str(geocodeArrest[1]) + "," 
  command = command + str(geocodeArrest[2]) + ")"
  cur.execute(command)
  # insert charges into charges table, linked to criminal table by aid
  baseCommand = "INSERT INTO CHARGES VALUES(" + str(aid)
  for charge in charges:
    command = baseCommand + ",'" + charge + "')"
    cur.execute(command)
  aid = aid + 1
con.commit()
end = time.time()
delta = end - start

print "Imported " + str(aid - 1) + " rows in " + str(delta) + " seconds" 