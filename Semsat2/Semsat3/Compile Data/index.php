<!DOCTYPE html>
<html>
	<head>
		<meta charset="UTF-8">
		<link href='http://fonts.googleapis.com/css?family=Open+Sans:300,400' rel='stylesheet' type='text/css'>
		<link rel="stylesheet" type="text/css" href="style.css">
	</head>
	
	<body>
		<div class="container">
			<div class="header">
				<div class="navbar">
					<div class="navtext">
					</div>
				</div>
			</div>

				<div class="bodycontent" padding="3">
<p>Have multiple files that you want to clean up at once? Move them to a folder on your desktop and on the keyboard, type <code>windows key + R</code>. Type <code>cmd</code> and hit <code>enter</code>. Type <code>cd Desktop/FolderWithData</code>. Now type <code>type *.txt > data.csv</code> and that'll combine all of the individual participant data files into a single CSV that you can upload here.</p>
					<form action="semsatImport.php" method="post" enctype="multipart/form-data">
						<h3>Import data</h3>
						<table width="60px">
							<tr>
								<td class="right">File<sup>1</sup>:</td>
								<td><input type="file" name="file" id="file" size="45" value=""></td>
							</tr>
							<tr>
								<td class="right"></td>
								<td><input style="float:left" type="submit" value="Upload"></td>
							</tr>
						</table>
						<p><sup>1</sup> Please be sure to upload the file as a CSV. Just open the file and click File -> Save As. For the flie type, choose 'CSV (Comma delimited).' When you hit Save,
						a dialog box will pop up asking you to confirm your choice. Select 'Yes.' Now you're ready to go!</p>
					</form>
				</div>
			<div class="footer">
			</div>
		</div>
	</body>
</html>