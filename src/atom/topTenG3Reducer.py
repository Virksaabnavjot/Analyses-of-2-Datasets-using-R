#!/usr/bin/env python
import sys

# Reducer to return overall top N records
# Data source: https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION
# Data  header is achieved with ease using R - summary() function in RStudio
# Data header: "school" "sex" "age" "address" "famsize" "Pstatus" "Medu" "Fedu" "Mjob" "Fjob" "reason" "guardian" "traveltime" "studytime" "failures" "schoolsup" "famsup" "paid" "activities" "nursery" "higher" "internet" "romantic" "famrel" "freetime" "goout" "Dalc" "Walc" "health" "absences" "G1" "G2" "G3"

# Initialise a list to store the top N records as a collection of touples (g3, record)
myList = []
n = 10	# Number of top N records

for line in sys.stdin:
	# remove leading and trailing whitespace
	line = line.strip()
	# split data values into list
	data = line.split(";")

	# convert g3 (currently a string) to int
	try:
		g3 = int(data[32])
	except ValueError:
		# ignore/discard this line
		continue

	# add (g3, record) touple to list
	myList.append( (g3, line) )
	# sort list in reverse order
	myList.sort(reverse=True)

	# keep only first N records
	if len(myList) > n:
		myList = myList[:n]

# Print top N records
for (k,v) in myList:
	print(v)
