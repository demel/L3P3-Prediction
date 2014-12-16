import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.csv.CSVRecord;


public class JSanParser {
    
    private BufferedWriter writer;
    private boolean headersAlreadyPrinted = false;
    private boolean printHeaders = false;
    
    private static String INPUT_DIR = "input";

    public static void main(String[] args){
	JSanParser myParser = new JSanParser();
	myParser.parseAll();
    }
    
    public JSanParser(){
    }
    
    public void parseAll(){
	File inputDir = new File(INPUT_DIR);
	if (!inputDir.isDirectory()){
	    return;
	}
	for (File oneDir : inputDir.listFiles()){
	    this.createNewOutputFile(oneDir.getName().replace(" ", "") + ".csv");
	    for (File oneSubDir : oneDir.listFiles()){
		this.parseDirectory(oneSubDir);
	    }
	}
    }
    
    private void createNewOutputFile(String filename){
	try {
	    if (this.writer != null){
		this.writer.close();
	    }
	    this.writer = new BufferedWriter(new FileWriter("output" + System.getProperty("file.separator") + filename));
	    this.headersAlreadyPrinted = false;
	} catch (IOException e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
	}
	
    }
    
    public void parseDirectory(File directory){
	if (!directory.isDirectory()){
	    return;
	}
	for (File oneFile : directory.listFiles()){
	    this.parseFile(oneFile);
	}
    }
    
    public void parseFile(File csvFile){
	if (csvFile.getName().contains("Events")){
	    // No need to change anything on the events file. It's a proper CSV file
	} else{
	    try {
		BufferedReader reader = new BufferedReader(new FileReader(csvFile));
		// Skip first few lines - Useless headers
		while (!reader.readLine().equals("")){
		}
		String str = "";
		while ((str = reader.readLine()) != null){
		    String resourceName = str;
		    String[] resources;
		    if (resourceName.equals("")){
			System.out.println("Reached the end of the file");
			// It's the end of the file
			break;
		    } else{
			resources = this.parseResourceName(resourceName);
			
		    }
		    
		    // Detect interfaces file
		    boolean isInterfaceFile = false;
		    String networkInterface = reader.readLine();
		    if (!networkInterface.equals("")){
			// It's a interfaces csv with two values
			isInterfaceFile = true;
			reader.readLine();
		    }
		    String line = "";
		    String csvFragment = "";
		    while (!(line = reader.readLine()).equals("")){
			csvFragment = csvFragment + line + "\n";
		    }
		    CSVParser parser = CSVParser.parse(csvFragment, CSVFormat.DEFAULT.withHeader());
		    
//		    BufferedWriter writer = new BufferedWriter(new FileWriter("output" + System.getProperty("file.separator") + resources[0] + "-" + resources[1] + ".csv"));
		    CSVPrinter printer = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader());

		    List<String> headerNames = new ArrayList<String>();
		    headerNames.add("Machine Name");
		    headerNames.add("Resource");
		    String formerVariableName = "";
		    String variableName = "";
		    for (String oneHeader : parser.getHeaderMap().keySet()){
			formerVariableName = variableName;
			variableName = oneHeader;
			if (oneHeader.contains("Bandwidth")){
			    isInterfaceFile = true;
			}
			headerNames.add(oneHeader);
		    }
		    headerNames.remove(variableName);
		    if (isInterfaceFile){
			headerNames.remove(formerVariableName);
		    }
		    
		    headerNames.add("Value");
		    headerNames.add("Variable");
		    if (printHeaders && !headersAlreadyPrinted){
			printer.printRecord(headerNames);
			this.headersAlreadyPrinted = true;
		    }

		    for(CSVRecord record : parser){
			List<String> data = new ArrayList<String>();
			data.add(resources[0]);
			data.add(resources[1]);
			String firstValue = "";
			String secondValue = "";
			for (String text : record){
			    firstValue = secondValue;
			    secondValue = text;
			    data.add(text);
			}
			data.add(variableName);
			if (isInterfaceFile){
			    data.remove(firstValue);
			    printer.printRecord(data);
			    data.remove(secondValue);
			    data.remove(variableName);
			    data.add(firstValue);
			    data.add(formerVariableName);
			}
			printer.printRecord(data);
		    }
		    printer.flush();
		}
		reader.close();
		
	    } catch (FileNotFoundException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	    } catch (IOException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	    }

	}


    }
    
    private String[] parseResourceName(String resourceLine){
	String[] parsedResource = new String[2];
	resourceLine = resourceLine.substring(1,resourceLine.length()-1); // Double quotes stripped
	parsedResource[0] = resourceLine.substring(0,resourceLine.indexOf("-"));
	parsedResource[1] = resourceLine.substring(resourceLine.indexOf("-")+1);
	
	return parsedResource;
    }
    
}
