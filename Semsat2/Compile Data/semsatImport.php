<<<<<<< HEAD
<?php
    header('Content-Disposition: attachment; filename="semsat3_out.csv"');
    header("Content-type: text/csv");
    // automatically detects formatting of linebreaks
    ini_set('auto_detect_line_endings',TRUE);
    setlocale(LC_ALL, "en_US.UTF-8");
    
    // as long as there was actually a file uploaded...
    if (($handle = fopen($_FILES['file']['tmp_name'], "r")) !== FALSE) {
        
        $header = array(0,1,2,3,4,5,6,7,8,9,10,11,12);
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
        $PID = 0;
        $ITEM = 1;
        $STUDY = 0;
        $STIMESTAMP = array();
        $cleaned = array();
        
        foreach($data as &$event) {
            if(is_numeric($event[0])) {
                    $PID = $event[0];
                    $ITEM = 1;
                    $STIMESTAMP[$PID] = array();
            } elseif($event[0]=="STUDY") {
                    $STUDY = 1;
            } elseif($event[0]=="TEST1" | $event[0]=="TEST2" |
                     $event[0]=="TEST3" | $event[0]=="TEST4") {
                    $STUDY = 0;
            } elseif($STUDY==1) {
                    if($event[0] == "fillerREL" | $event[0] == "fillerUNREL") {
                            $STIMESTAMP[$PID][$event[2]] = $event[5];
                    } else {
                            $STIMESTAMP[$PID][$event[3]] = $event[7];
                    }
            } else {
                    if($event[0] == "fillerREL" | $event[0] == "fillerUNREL") {
                            $temp = array($PID, $event[0],$event[1],$ITEM,$event[2],
                                          $event[3],"",$event[4],$STIMESTAMP[$PID][$event[2]],
                                          $event[5],$event[7],$event[8],$event[9]);
                            array_push($cleaned,$temp);
                            $ITEM++;
                    } else {
                            $temp = array($PID, $event[0],$event[1],$ITEM,
                                          $event[3],$event[4],$event[5],$event[6],
                                          $STIMESTAMP[$PID][$event[3]],$event[7],$event[9],
                                          $event[10],$event[11]);
                            array_push($cleaned,$temp);
                            $ITEM++;
                    }
            }
        }
        
        $out = fopen('php://output', 'w');
        foreach($cleaned as $event) {
            fputcsv($out, $event);
        }
        fclose($out);
    }
=======
<?php
    header('Content-Disposition: attachment; filename="semsat3_out.csv"');
    header("Content-type: text/csv");
    // automatically detects formatting of linebreaks
    ini_set('auto_detect_line_endings',TRUE);
    setlocale(LC_ALL, "en_US.UTF-8");
    
    // as long as there was actually a file uploaded...
    if (($handle = fopen($_FILES['file']['tmp_name'], "r")) !== FALSE) {
        
        $header = array(0,1,2,3,4,5,6,7,8,9,10,11,12);
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
        $PID = 0;
        $ITEM = 1;
        $STUDY = 0;
        $STIMESTAMP = array();
        $cleaned = array();
        
        foreach($data as &$event) {
            if(is_numeric($event[0])) {
                    $PID = $event[0];
                    $ITEM = 1;
                    $STIMESTAMP[$PID] = array();
            } elseif($event[0]=="STUDY") {
                    $STUDY = 1;
            } elseif($event[0]=="TEST1" | $event[0]=="TEST2" |
                     $event[0]=="TEST3" | $event[0]=="TEST4") {
                    $STUDY = 0;
            } elseif($STUDY==1) {
                    if($event[0] == "fillerREL" | $event[0] == "fillerUNREL") {
                            $STIMESTAMP[$PID][$event[2]] = $event[5];
                    } else {
                            $STIMESTAMP[$PID][$event[3]] = $event[7];
                    }
            } else {
                    if($event[0] == "fillerREL" | $event[0] == "fillerUNREL") {
                            $temp = array($PID, $event[0],$event[1],$ITEM,$event[2],
                                          $event[3],"",$event[4],$STIMESTAMP[$PID][$event[2]],
                                          $event[5],$event[7],$event[8],$event[9]);
                            array_push($cleaned,$temp);
                            $ITEM++;
                    } else {
                            $temp = array($PID, $event[0],$event[1],$ITEM,
                                          $event[3],$event[4],$event[5],$event[6],
                                          $STIMESTAMP[$PID][$event[3]],$event[7],$event[9],
                                          $event[10],$event[11]);
                            array_push($cleaned,$temp);
                            $ITEM++;
                    }
            }
        }
        
        $out = fopen('php://output', 'w');
        foreach($cleaned as $event) {
            fputcsv($out, $event);
        }
        fclose($out);
    }
>>>>>>> 0b11987ac2d7bc3424907f4d5fc90a3ae48e33b5
?>