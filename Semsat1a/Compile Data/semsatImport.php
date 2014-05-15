<?php
    header('Content-Disposition: attachment; filename="semsat1_out.csv"');
    header("Content-type: text/csv");

    //This is messy and an overall mediocre solution; however, if you throw this up onto a
    //PHP server, it'll take and parse a CSV file containing the raw data from Experiment 1.
    //I'd recommend first navigating to the directory where all of the raw data files are stored
    //and concatenating them into one csv file via:
    //
    //WINDOWS:
    //	> type *.txt > data.csv
    //  
    //MAC/LINUX:
    //	> cat *.txt > data.csv
    //
    //You can then upload the data via this page and have returned to you a second CSV that
    //cleans up all of the columns. (The raw files aren't particularly pretty: you could do
    //it manually in Excel; however, I won't recommend it.
    
    
    // automatically detects formatting of linebreaks
    ini_set('auto_detect_line_endings',TRUE);
    setlocale(LC_ALL, "en_US.UTF-8");
    
    // as long as there was actually a file uploaded...
    if (($handle = fopen($_FILES['file']['tmp_name'], "r")) !== FALSE) {

	$header = array(1,2,3,4,5,6,7,8,9,10,11,12);
	// this will contain all of the actual data uploaded
	$data = array();
	
	// until every row has been read in
	while ($row = fgetcsv($handle)) {
	    
	    // set up a temporary array
	    $arr = array();
	    
	    // for each column, set that header as the index key
	    foreach ($header as $i => $col) {
		
		// set that cell as the value
		$arr[$col] = $row[$i];
	    }
	    
	    // append the data to your array
	    $data[] = $arr;
	}
	
	// closes the file
	fclose($handle);
	
	// constructs an array for the cleaned-up data
	$cleanedData = array();
	$PID = 0;
	$ITEM = 1;
	
	foreach($data as &$event) {
	    if(is_numeric($event[1])) {
		 $PID = $event[1];
		 $ITEM = 1;
	    } elseif($event[1]=="filler related" || $event[1]=="filler unrelated") {
		$temp = array($PID,$event[1],$event[2],$ITEM,$event[4],
		    $event[5],"","","","",$event[7],$event[8],$event[9]);
		array_push($cleanedData, $temp);
        $ITEM++;
	    } else {
		$temp = array($PID,$event[1],$event[2],$ITEM,$event[4],
		    $event[5],$event[6],$event[7],$event[8],$event[9],
		    $event[10],$event[11],$event[12]);
		array_push($cleanedData, $temp);
        $ITEM++;
	    }
	}
	$out = fopen('php://output', 'w');
	foreach($cleanedData as $event) {
	    fputcsv($out, $event);
	}
	fclose($out);
    }
=======
<?php
    header('Content-Disposition: attachment; filename="semsat1_out.csv"');
    header("Content-type: text/csv");

    //This is messy and an overall mediocre solution; however, if you throw this up onto a
    //PHP server, it'll take and parse a CSV file containing the raw data from Experiment 1.
    //I'd recommend first navigating to the directory where all of the raw data files are stored
    //and concatenating them into one csv file via:
    //
    //WINDOWS:
    //	> type *.txt > data.csv
    //  
    //MAC/LINUX:
    //	> cat *.txt > data.csv
    //
    //You can then upload the data via this page and have returned to you a second CSV that
    //cleans up all of the columns. (The raw files aren't particularly pretty: you could do
    //it manually in Excel; however, I won't recommend it.
    
    
    // automatically detects formatting of linebreaks
    ini_set('auto_detect_line_endings',TRUE);
    setlocale(LC_ALL, "en_US.UTF-8");
    
    // as long as there was actually a file uploaded...
    if (($handle = fopen($_FILES['file']['tmp_name'], "r")) !== FALSE) {

	$header = array(1,2,3,4,5,6,7,8,9,10,11,12);
	// this will contain all of the actual data uploaded
	$data = array();
	
	// until every row has been read in
	while ($row = fgetcsv($handle)) {
	    
	    // set up a temporary array
	    $arr = array();
	    
	    // for each column, set that header as the index key
	    foreach ($header as $i => $col) {
		
		// set that cell as the value
		$arr[$col] = $row[$i];
	    }
	    
	    // append the data to your array
	    $data[] = $arr;
	}
	
	// closes the file
	fclose($handle);
	
	// constructs an array for the cleaned-up data
	$cleanedData = array();
	$PID = 0;
	$ITEM = 1;
	
	foreach($data as &$event) {
	    if(is_numeric($event[1])) {
		 $PID = $event[1];
		 $ITEM = 1;
	    } elseif($event[1]=="filler related" || $event[1]=="filler unrelated") {
		$temp = array($PID,$event[1],$event[2],$ITEM,$event[4],
		    $event[5],"","","","",$event[7],$event[8],$event[9]);
		array_push($cleanedData, $temp);
        $ITEM++;
	    } else {
		$temp = array($PID,$event[1],$event[2],$ITEM,$event[4],
		    $event[5],$event[6],$event[7],$event[8],$event[9],
		    $event[10],$event[11],$event[12]);
		array_push($cleanedData, $temp);
        $ITEM++;
	    }
	}
	$out = fopen('php://output', 'w');
	foreach($cleanedData as $event) {
	    fputcsv($out, $event);
	}
	fclose($out);
    }
?>