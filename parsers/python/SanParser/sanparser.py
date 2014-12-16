import os
import csv
import StringIO
import datetime
import locale

inputDir = 'input'
outputDir = 'output'
englishLocaleName = 'en_US.utf8'
spanishLocaleName = 'es_ES.utf8'

def parseAll():
    loc = locale.getlocale()
    #print datetime.datetime.strftime(datetime.datetime.now(), "%d-%b-%Y %H:%M:%S %Z")
    locale.setlocale(locale.LC_TIME, englishLocaleName)
    for filename in os.listdir(inputDir):
        joinedName = os.path.join(os.getcwd(), inputDir, filename)
        if not os.path.isdir(joinedName):
            continue
        print(filename)
        outputFile = createNewOutputFile(filename)
        parseDirectory(joinedName, outputFile)
        outputFile.close()
    print("Finished!")
    locale.setlocale(locale.LC_TIME, loc)               

def createNewOutputFile(filename):
    # This removes white spaces from the filename by mapping them
    # to None
    filename = filename.translate(None, ' ')
    joinedName = os.path.join(os.getcwd(), outputDir, filename + ".csv")
    return open(joinedName, 'w+')

def parseResourceName(resourceName):
    # String end-of-line and double quotes
    resourceName = resourceName.rstrip("\n").strip("\"")
    # This will create a 3-tuple
    return resourceName.partition("-")

def parseFile(filePath, outputFile):
    print("Parsing " + filePath)
    if "Events" in os.path.basename(filePath):
        parseEvents(filePath, outputFile.name[:-4] + "-events.csv")
        #print("Skipped events file")
        return
    else:
        with open(filePath, 'rU') as readFile:
            # Skip the first comment lines

            while not readFile.readline() == '\n':
                pass
            print("Skipped comments")
            for line in readFile:
                if line == "\n":
                    print("Reached the end of the file")
                    break
                else:
                    resources = parseResourceName(line)
                    isInterfaceFile = False
                    networkInterface = next(readFile) # Python 2.x 3.x compatible (Well, Python 2.6 onwards actually)
                    if not networkInterface == "\n":
                        isInterfaceFile = True
                        #Skip one line
                        next(readFile)
                    csvFragment = ""
                    for innerLine in readFile:
                        if innerLine == "\n":
                            break
                        csvFragment += innerLine
                    if csvFragment == "":
                        continue
                    readCsv = csv.reader(StringIO.StringIO(csvFragment))
                    firstRow = next(readCsv, None) # Skipped headers
                    # It turns out there are some interfaces without the extra \n
                    if "Bandwidth" in firstRow[-1]:
                        isInterfaceFile = True

                    csvWriter = csv.writer(outputFile, quoting=csv.QUOTE_MINIMAL)
                    for row in readCsv:
                        row.insert(0,resources[0]) # Machine name parsed before through partition
                        row.insert(1,resources[2]) # Resource name parsed before through partition
                        
                        #Date Replacement
                        date = row.pop(2)
                        row.insert(2, getNumericMonthDate(date))
                        
                        if (isInterfaceFile):
                            bOut = row.pop() # Delete Bandwidth out
                            row.append(firstRow[-2]) # Insert BW in title
                            csvWriter.writerow(row)
                            row.pop() # Delete BW in title
                            row.pop() # Delete Bandwidth In
                            row.append(bOut) # Add Bandwidth out
                            row.append(firstRow[-1]) # Insert BW Out title
                            csvWriter.writerow(row)
                        else:
                            row.append(firstRow[-1])
                            csvWriter.writerow(row)
        
def parseDirectory(directoryPath, outputFile):
    if not os.path.isdir(directoryPath):
        return
    else:
        for filename in sorted(os.listdir(directoryPath)):
            joinedPath = os.path.join(directoryPath, filename)
            if os.path.isdir(joinedPath):
                parseDirectory(joinedPath, outputFile)
            else:
                parseFile(joinedPath, outputFile)
        return                      

def parseEvents(filePath, eventsFileName):
    with open(filePath, 'rU') as readFile:
        with open(eventsFileName, 'a') as eventsFile:
            #csvReader = csv.reader(readFile)
            csvReader = csv.DictReader(readFile);
            #csvWriter = csv.writer(eventsFile, quoting=csv.QUOTE_ALL)
            fieldnames = ['Severity', 'Created On', 'Name', 'Event Type', 'Event', 'Created By', 'Cleared On', 'Cleared By', 'Model Type Name', 'Event Precedence']
            csvWriter = csv.DictWriter(eventsFile, delimiter=',', fieldnames=fieldnames, quoting=csv.QUOTE_ALL)
            #csvWriter.writeheader();
            # Skip header
            csvReader.next()
            for entry in csvReader:
                #Date Replacement
                entry['Created On'] = getNumericMonthDateSpanish(entry['Created On'])
                #print(entry['Created On'])
                csvWriter.writerow(entry)

def getNumericMonthDate(date):
    parsedDate = datetime.datetime.strptime(date, "%d-%b-%y %I:%M:%S %p")
    return parsedDate.strftime("%d-%m-%y %H:%M:%S")

def getNumericMonthDateSpanish(date):
    #For event dates which are in a different format. Nice!
    loc = locale.getlocale(locale.LC_TIME)
    locale.setlocale(locale.LC_TIME, spanishLocaleName)
    noTZdate = date[0:date.rfind(" ")]
    parsedDate = datetime.datetime.strptime(noTZdate, "%d-%b-%Y %H:%M:%S")
    locale.setlocale(locale.LC_TIME, loc)
    return parsedDate.strftime("%d-%m-%y %H:%M:%S")
# Main Script Here
parseAll()