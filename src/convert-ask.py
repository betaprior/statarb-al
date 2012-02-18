#!/usr/bin/python

# Python script to take CRSP output with the fields DATE, TICKER and RET and return
# a .csv of the form DATE,TICKER1,TICKER2,...,TICKERn with returns grouped by date.
# Missing or non-numeric values are represented as NA so that R can deal with them gracefully.
# Note: You'll want to include the TICKER in your CRSP down load and you'll want the
# "period returns" --- these are the ones that handle dividends correctly.
# source: http://www-stat.wharton.upenn.edu/~steele/Courses/956/Python/PythonResource.html
#
# From Michael Boldin (WRDS):
# The dictionary in a dictionary was clever trick. I did change  
#    if re.compile("-?\d+(\.\d+)?").match(values[3]):
# to be more general ,  using ret instead of value[3]
# Also there are cases in CRSP extracts where Tickers are missing (read in as
# NULL I think) if the date range is long enough for a few PERMNO cases, so I
# used
#         if ticker and re.compile("-?\d+(\.\d+)?").match(ret):
# another option is to use
#      isinstance(ret,(int,float))
# to test for a true number
#
# Finally, I am fairly sure that opening a file as 'rb' avoids any need to
# worry about \n breaks.  And it is probably not worth using for this case,
# but python has a nice CSV module for reading delimited text files. I
# thought it could both read in large chunks, or iterate through row by row,
# but looking at the documentation I am not sure about reading row blocks.  I
# have used the csv.writerows() method as a very fast writer of row blocks.

import re;
import getopt;
import sys;

usage = "Usage: convert.py -i filepath -o filepath\n";

inputFilePath = "";
outputFilePath = "";
try:
	opts, args = getopt.getopt(sys.argv[1:], "i:o:");
	for opt, arg in opts:
		if opt == "-i":
			inputFilePath = arg;
		elif opt == "-o":
			outputFilePath = arg;
except getopt.GetoptError:
	print usage;
	
if inputFilePath == "" or outputFilePath == "":
	print usage;

inputFile = None;
try:
	inputFile = open(inputFilePath, "r");
except IOError, (errno, strerror):
	print "I/O error(%s) attempting to open %s: %s" % (errno, inputFilePath, strerror);
	exit(0);

map = {};
fields = inputFile.readline().split(",");
for i in range(len(fields)):
	field = fields[i].replace("\n", "");
	key = "";
	if field == "DATE":
		key = "DATE";
	elif field == "TICKER":
		key = "TICKER";
	elif field == "ASK":
		key = "ASK";
	if key != "":
		map[key] = i;
		
if not "DATE" in map or not "TICKER" in map or not "ASK" in map:
	print inputFilePath, "is missing at least one of DATE, TICKER and ASK."
	exit(0);

database = {};
for line in inputFile:
	values = line.split(",");

	date = values[map["DATE"]].replace("\n", "");
	ticker = values[map["TICKER"]].replace("\n", "");
	ret = values[map["ASK"]].replace("\n", "");

	# if re.compile("-?\d+(\.\d+)?").match(values[3]): #as per comments above:
	if ticker and re.compile("-?\d+(\.\d+)?").match(ret): 
		if not date in database:
			database[date] = {};
		database[date][ticker] = ret;

inputFile.close();

dates = database.keys();
dates.sort();

tickers = [];	
for date in dates:
	tickers = tickers + database[date].keys();
tickers = list(set(tickers));
tickers.sort();

outputFile = None;
try:
	outputFile = open(outputFilePath, "w");
except IOError, (errno, strerror):
	print "I/O error(%s) attempting to open %s: %s" % (errno, outputFilePath, strerror);
	exit(0);

firstLine = "DATE";
for ticker in tickers:
	firstLine = firstLine + "," + ticker;
firstLine = firstLine + "\n";
outputFile.write(firstLine);

for date in dates:
	currentLine = date;
	for ticker in tickers:
		if not ticker in database[date]:
			currentLine = currentLine + "," + "NA";
		else:
			currentLine = currentLine + "," + database[date][ticker];
	currentLine = currentLine + "\n";
	outputFile.write(currentLine);

outputFile.close();
