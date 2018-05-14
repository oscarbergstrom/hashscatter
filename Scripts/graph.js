		/* 
		Graph.js

		Graph.js is responsible for making calculations on the data that gets sent from the
		server, and then drawing the graph based on that data with the canvas object.

		Version 1.0: Basic functionality and structure created by Tobias Lindell.
		Version 1.1: Cleaned code and added HTTP functionality done by Kai Salmon.
		Version 1.2: Revision graph drawing code done by Tobias Lindell.
		Version 1.3: Enhancement of visual feedback on graph done by Eythor Einarsson.
		Version 1.4: Added input validation and error messages done by Petronela Petre.
 
		*/


		var useBounds = false;
		var equalizeBounds = false;
		var useLogs = true;
		var labelFont = 'bold 15px Hipsterish';
		var labelColor = '#0d0dcb';
		var labelColorX = '#890020';
		var dotColor = '#000000';
		var inputColor = '#888888';
		var tempData;	
		var g_inputA;
		var g_inputB;
		
		
		/*
		Method returns frequency of given tag within given list.

		This method is used whenever we want to use the requency of a given element
		of the lists used in the graph.

		*/
		function findTag(element, list){
			for(k=0;k<list.length;k++){
				if((list[k].tag.localeCompare(element))==0){
					return list[k].freq;
					break;
				}
			}
			return 0;
			
		}
		
		/*
		This method returns a list of all the tags that dont exist in the second list,
		but not in the first.


		This methos was originally used to give all the points that would be printed on
		the y-axis. However this is not longer being used as we decided that data on the
		axes was not relevant to the end users.
		*/
		function findLeftOvers(ListB, ListA){
			var leftOvers = new Array();
			
			for(var i=0;i<ListB.length;i++){
				if(findTag(ListB[i].tag, ListA)==0){
					leftOvers.push(ListB[i]);
				}
			}
			if(leftOvers!=null){
				return leftOvers;
			}else return [];
		}
		

		/*
		This method returns a list.

		The method combines the two input lists into one. This is the format which is
		used by other functions to draw the graph itself.
		*/
		function combineInputs(ListA, ListB){
			var combinedInput = new Array();
			
			for(var i=0;i<ListA.length;i++){
				
				//if(ListA[i].freq != 0 && (findTag(ListA[i].tag, ListB) != 0)){
					combinedInput.push({tag:ListA[i].tag, freqx:ListA[i].freq, freqy: (findTag(ListA[i].tag, ListB))})
				//}
			}
			
			combinedInput.join(findLeftOvers(ListB, ListA));
		
		}
		
	
		/*
		This method returns nothing.

		The method draws the base lines of the graph which will be the same regardless
		of input information.
		*/
		function drawBase(inputXAxis,inputYAxis,inputListA, inputListB){
			var ctx = document.getElementById('chart-box').getContext('2d');
			
			
			ctx.fillStyle = inputColor;
			ctx.beginPath();
			var largerAxis = Math.min(ctx.canvas.clientHeight, ctx.canvas.clientWidth);
			console.log(largerAxis);
			ctx.moveTo(50, 25);
			ctx.lineTo(50, (largerAxis - 30));
			ctx.lineTo((largerAxis - 30), (largerAxis - 30));
			
			ctx.moveTo(51, 25);
			ctx.lineTo(51, (largerAxis - 31));
			ctx.lineTo((largerAxis - 31), (largerAxis - 31));
						
			ctx.stroke();
			
			ctx.font = labelFont;
			ctx.fillText(inputXAxis, ((largerAxis-30)/2), largerAxis - 13);
			
			ctx.save();
			ctx.rotate(3.14/2);
			ctx.fillText(inputYAxis, ((largerAxis-30)/2), -25);
			
			ctx.restore();
			
			
			
			var xMinBound = 0;
			var xMaxBound = 10;
			var yMinBound = 0;
			var yMaxBound = 10;
		
		}
		
		/*
		This method returns nothing.

		We use this method to set the global maxBounds variables to fit the maximum
		values in the current input lists.
		*/
		function getBounds(inputListA,inputListB){
		
			/*xMaxBound = Math.ceil((getMaxFreq(inputListA)) / 10.0) * 10;
			yMaxBound = Math.ceil((getMaxFreq(inputListB)) / 10.0) * 10;
			xMinBound = parseInt(getMinFreq(inputListA)/10, 10)*10;
			yMinBound = parseInt(getMinFreq(inputListB)/10, 10)*10;
			*/
			
			xMaxBound = getMaxFreq(inputListA);
			yMaxBound = getMaxFreq(inputListB);
			xMinBound = 0;
			yMinBound = 0;
			
			if(equalizeBounds){
				yMaxBound = Math.max(xMaxBound,yMaxBound);
				xMaxBound = Math.max(xMaxBound,yMaxBound);
			}
		}
		
		/*
		This method returns nothing.

		Currently unused, this method adds little lines on the base graph to indicate
		the division of the bounds. It is toggled on or off with a global boolean.
		*/
		function drawBounds(){
		
			ctx.fillStyle = '#a8a8a8';
			for(var i=100;i<=500;i+=40){
				ctx.moveTo(i, 422);
				ctx.lineTo(i, 429);
				
				if(((i-100)/40)>0){
					ctx.fillText(((xMaxBound/10)*((i-100)/40)), i-4, 440);
				}else{
					ctx.fillText(0, i-4, 440);
				}
				
			}
		
			for(var i=100;i<=500;i+=40){
				ctx.moveTo(i+1, 422);
				ctx.lineTo(i+1, 429);
				
				if(((i-100)/40)>0){
					ctx.fillText(((xMaxBound/10)*((i-100)/40)), i-4, 440);
				}else{
					ctx.fillText(0, i-4, 440);
				}
				
			}
		
		
			for(var i=425;i>=25;i-=40){
				ctx.moveTo(97, i);
				ctx.lineTo(104, i);
				
				if(((425-i)/40)>0){
					ctx.fillText(((yMaxBound/10)*((425-i)/40)), 83, i+4);
				}else{
					ctx.fillText(0, 83, i+4);
				}
			}
			
			for(var i=425;i>=25;i-=40){
				ctx.moveTo(97, i+1);
				ctx.lineTo(104, i+1);
				
				if(((425-i)/40)>0){
					ctx.fillText(((yMaxBound/10)*((425-i)/40)), 83, i+4);
				}else{
					ctx.fillText(0, 83, i+4);
				}
			}
			ctx.stroke();
		
		}
		
		
		/*
		Returns an integer, which is the highest frequencry among all the pairs in
		a given input list.

		Used primarily when calculating the max bounds of the graph.
		*/
		function getMaxFreq(input){
			var max = 0;
			for(var i=0;i<input.length;i++){
				max = Math.max(input[i].freq,max);
			}
			return max;
		}
		/*
		Returns an integer, which is the lowest frequency amonng all the pairs in
		a given input list.

		Not currently used.
		*/
		function getMinFreq(M){
			
			var min = 1000;
			for(var i=0;i<M.length;i++){
			
				if(M[i].freq<min){
					min = M[i].freq;
				}
			}
		
			return min;
		}
			
			
	/*
	Returns nothing.

	This method uses the two input lists and draws all the points on the graph. It
	also changes the labels' color and position based on where it is drawn.
	*/
	function drawPoints(inputListA,inputListB){
		var overlapCounter = 0;
		for(var i=0;i<inputListA.length;i++){
			overlapCounter = 0;
			var name = inputListA[i].tag;
			var x = inputListA[i].freq;
			var y = findTag(inputListA[i].tag, inputListB);
			var smallerAxis = Math.min(ctx.canvas.clientHeight, ctx.canvas.clientWidth);
			var xPos = 50+((x/xMaxBound) * (smallerAxis - 80));
			var yPos = (smallerAxis - 30)-((y/yMaxBound) * (smallerAxis - 55));
			var xPosText = xPos;
			
			if(xPos > (ctx.canvas.clientWidth-50)){
				xPosText = xPos-30;
			}
			ctx = document.getElementById("chart-box").getContext("2d");
			ctx.fillStyle = labelColor;
			
			for(var j=0;j<i;j++){
				
				var isOverlapingx = (inputListA[i].freq == (inputListA[j].freq));
				var isInWayOfTextMax = (x <= (inputListA[j].freq+2));
				var isInWayOfTextMin = (x >= (inputListA[j].freq-1));
				var isOnSamey = (y == findTag(inputListA[j].tag, inputListB));
				var isOverlapingy = (findTag(inputListA[i].tag, inputListB) == (findTag(inputListA[j], inputListB)));
				
				if(isOverlapingx && isOverlapingy){
					overlapCounter++;
				}else if((isInWayOfTextMax && isInWayOfTextMin) && isOnSamey){
					overlapCounter++;
				}			
				//console.log(inputListA[i].tag + ": " + isInWayOfTextMax + ", " + isInWayOfTextMin + ", " + isOnSamey);
				//ctx.fillText(name, Math.floor(xPos+10), Math.floor(yPos));
					
		}
		
		
	
		switch(overlapCounter){
			case 0:
				if(x > y){
					ctx.fillStyle = labelColorX;
				}
				else{
					ctx.fillStyle = labelColor;
				}

				ctx.fillText(name, Math.floor(xPosText+10), Math.floor(yPos));
				ctx.fillStyle = dotColor;
				ctx.fillRect(Math.floor(xPos)-4, Math.floor(yPos)-4, 8, 8);
			break;
	
			case 1:

				if(x > y){
					ctx.fillStyle = labelColorX;
				}
				else{
					ctx.fillStyle = labelColor;
				}
				ctx.fillText(name, Math.floor(xPosText+10)-3, Math.floor(yPos)+13);
				ctx.fillStyle = dotColor;
				ctx.fillRect(Math.floor(xPos)-4, Math.floor(yPos)-4, 8, 8);

			break;

			case 2:
				if(x > y){
					ctx.fillStyle = labelColorX;
				}
				else{
					ctx.fillStyle = labelColor;
				}
				ctx.fillText(name, Math.floor(xPosText+10)-5, Math.floor(yPos)+26);
				ctx.fillStyle = dotColor;
				ctx.fillRect(Math.floor(xPos)-4, Math.floor(yPos)-4, 8, 8);
			break;							
		}

	}
		
		var leftOvers = findLeftOvers(inputListB, inputListA);
		
		for(var j=0;j<leftOvers.length;j++){
			
			var name = leftOvers[j].tag;
			var x = 0;
			var y = leftOvers[j].freq;
		
			var xPos = 100+((x/xMaxBound) * 400);
			var yPos = 425-((y/yMaxBound) * 400);
			
			ctx.fillStyle = '#000000';
			ctx.fillRect(Math.floor(xPos)-4, Math.floor(yPos)-4, 8, 8);
			ctx.fillStyle = labelColor;
			//ctx.fillText(leftOvers[j].tag, Math.floor(xPos+10), Math.floor(yPos));
		}
	}
	
	/*
	Returns new version of input list.

	This method removes any tag pairs with a frequency of 1 or less. To make the data more
	readable, we ran all our points through a logarithmic scale. Therefore, any frequencies
	of 1 or less would end up on the axes.
	*/
	function refinelist(input, compare){
		var output = new Array();
		var temp = JSON.parse(JSON.stringify(input));
		for(var i=0;i<temp.length;i++){
			if(findTag(temp[i].tag, compare) > 1 && temp[i].tag.charAt(0) !== '[' && temp[i].freq > 1){
				if(useLogs){
					temp[i].freq = Math.log(temp[i].freq);
				}
				output.push(temp[i]);
			}
			
		}
		//console.log(output);
		return output;
	}
	
	/*
	Returns nothing.

	This method calls all the methods which draw the graph and its contents in
	the correct order. It also uses the httpclient to receive the JSON
	data packet from the server.
	*/
	function drawgraph(){
		//start loading animation
		var inputXAxis = document.getElementById("x-input").value;
		var inputYAxis = document.getElementById("y-input").value;
		console.log("xinput" + inputXAxis);
		console.log("yinput" + inputYAxis);
		if(inputXAxis != inputYAxis){
			var loader = document.getElementsByClassName("windows8");
			loader[0].style.visibility = "visible";
			
			
			
			var canvas = document.getElementById("chart-box");
			
			canvas.style.display = "none";
			ctx = canvas.getContext("2d");
			ctx.clearRect(0, 0, canvas.width, canvas.height);
			
		
			ctx.fillStyle = '#000000';
			ctx.font = "20px Hipsterish";
			//ctx.fillText("Loading...",200,250);
			

			
			
			var url = "/evaluateHashtag.php?x="+inputXAxis+"&y="+inputYAxis;
			var aClient = new HttpClient();
			var request = aClient.get(url, function(r) {
				canvas.width = ctx.canvas.clientWidth;
				canvas.height = ctx.canvas.clientHeight;
				ctx.clearRect(0, 0, canvas.width, canvas.height);
				
				try  {
				r = JSON.parse(r);
				}
				catch(e) {
					errorCanvas("No data available.");
					return;
				}
				
				var ninputListA = r.X;
				var ninputListB = r.Y;
				var inputListA = refinelist(ninputListA, ninputListB);
				var inputListB = refinelist(ninputListB, ninputListA);
				

				combineInputs(inputListA, inputListB);
				getBounds(inputListA, inputListB);
				
				drawBase(inputXAxis,inputYAxis,inputListA, inputListB);
				drawPoints(inputListA,inputListB);
				
				//store data in global vars
				//tempData=r;
				
				g_inputA = inputListA;
				g_inputB = inputListB;
				
				//show canvas / hide loader
				loader[0].style.visibility = "hidden";
				canvas.style.display = "block";
				
				//redraw graph.. doesn't display properly.. need to separate drawing from data load
				redrawGraph();
				//store cookie
				//storeDataCookie(r);

			});
		}
			
	}
	
	/*
	Returns nothing.

	This method creates an HTTP connection which allows for our client to send get requests
	to the server, and receive the JSON data packet. The javascript uses our PHP file to open
	port 8000 specifically.
	*/
	var HttpClient = function() {
		this.get = function(aUrl, aCallback) {
			var anHttpRequest = new XMLHttpRequest();
			anHttpRequest.onreadystatechange = function() { 
				if (anHttpRequest.readyState == 4 && anHttpRequest.status == 200)
					aCallback(anHttpRequest.responseText);
				else if (!anHttpRequest.status == 200) {
					errorCanvas("Server offline. Come back later.");
				}
			}
	
			anHttpRequest.open( "GET", aUrl, true );            
			anHttpRequest.send( null );
		}
	}

	/*
	Returns nothing.

	This method prints an error message to screen when the server is unresponsive
	or if the request takes too long.
	*/
	function errorCanvas(string) {
		var loader = document.getElementsByClassName("windows8");
		loader[0].style.visibility = "hidden";
		var canvas = document.getElementById("chart-box");
		canvas.style.display = "block";
		ctx = canvas.getContext("2d");
		ctx.clearRect(0, 0, canvas.width, canvas.height);
		ctx.fillStyle = '#000000';
		ctx.font = "20px Hipsterish";
		ctx.textAlign = 'center';
		ctx.fillText(string,canvas.width/2,canvas.height/5);
		
	}
	
	/*
	Returns cookie data.

	Currently unused, this method was supposed to cache users' search data so that
	certain searches could be made more efficient if repeated. The information we needed
	to store took up too much space to use cookies in the end.
	*/
	function storeDataCookie(data){
		
		var cookie = ["graphdata", '=', JSON.stringify(data), '; domain=.', window.location.host.toString(), '; path=/;'].join('');
		document.cookie = cookie;
	}

	// Retrieve graph data from cookie (not used)
	function getDataCookie() {
		var result = document.cookie.match(new RegExp("graphdata" + '=([^;]+)'));
		result && (result = JSON.parse(result[1]));
		return result;
	}

	// Redraw graph with cookie data
	function redrawGraph() {
		//return from cookie
		//var r = getDataCookie();
		//var r = tempData;
		//if (r == "") 
		//	return 0;
		
		var canvas = document.getElementById("chart-box");
		ctx = canvas.getContext("2d");
		ctx.clearRect(0, 0, canvas.width, canvas.height);
		
		ctx.fillStyle = '#000000';
		ctx.font = "20px Hipsterish";

		canvas.width = ctx.canvas.clientWidth;
		canvas.height = ctx.canvas.clientHeight;
		ctx.clearRect(0, 0, canvas.width, canvas.height);
		
		
		var inputXAxis = document.getElementById("x-input").value;
		var inputYAxis = document.getElementById("y-input").value;
		//var ninputListA = r.X;
		//var ninputListB = r.Y;
		//var inputListA = refinelist(ninputListA, ninputListB);
		//var inputListB = refinelist(ninputListB, ninputListA);
		var inputListA = g_inputA;
		var inputListB = g_inputB;
		
		combineInputs(inputListA, inputListB);
		getBounds(inputListA, inputListB);
		drawBase(inputXAxis,inputYAxis,inputListA, inputListB);
		drawPoints(inputListA,inputListB);
		drawDottedLine(inputListA, inputListB);
	}

	/*
	Returns nothing.

	This method draws a dotted line on the graph which shows the baseline where
	tags are used at an equal frequency. This is useful to show how tags are used
	more frequently together with one input than the other, even when the bounds
	are uneven.
	*/
	function drawDottedLine(inputListA, inputListB){

		var canvas = document.getElementById("chart-box");
		
		var maxFreqX = getMaxFreq(inputListA);
		var maxFreqY = getMaxFreq(inputListB);
		console.log(inputListA);
		console.log(inputListB);
		var cornerOfXandY = Math.atan(maxFreqY/maxFreqX);
		var cornerOfXandYforLine = 90 - cornerOfXandY;
		var unknownCornerX = 90 + cornerOfXandY;
		var getSinOfCornerX = Math.sin(unknownCornerX);
		var lengthOfUnknownSideX = maxFreqY / getSinOfCornerX;
		var finalLengthOfUnknownSideX = Math.pow(maxFreqY, 2) + Math.pow(getSinOfCornerX, 2);
		finalLengthOfUnknownSideX = Math.sqrt(finalLengthOfUnknownSideX);
		var ratioOfUnknownSideX = finalLengthOfUnknownSideX/maxFreqX;
		var FinalLengthX = ratioOfUnknownSideX * (ctx.canvas.clientWidth * 0.84);

		var unknownCornerY = 180 - unknownCornerX;
		var getSinOfCornerY = Math.sin(unknownCornerY);
		var lengthOfUnknownSideY = maxFreqX / getSinOfCornerY;
		var finalLengthOfUnknownSideY = Math.pow(maxFreqX, 2) + Math.pow(getSinOfCornerY, 2);
		finalLengthOfUnknownSideY = Math.sqrt(finalLengthOfUnknownSideY);
		var ratioOfUnknownSideY = finalLengthOfUnknownSideY/maxFreqY;
		var FinalLengthY = ratioOfUnknownSideY * (canvas.height * 0.89);
		
		var smallerAxis = Math.min(ctx.canvas.clientHeight, ctx.canvas.clientWidth);
		ctx.setLineDash([2,5]);
		ctx.moveTo(50, 470-(500-smallerAxis));

		if(maxFreqX > maxFreqY){
			ctx.lineTo((FinalLengthX + 50) , (30-(500-smallerAxis)));
			//ctx.lineTo((FinalLengthX + (canvas.height * 0.1)) , (0.06 * canvas.height));
			ctx.stroke();
		}

		else{
			ctx.lineTo(470, ((470 - FinalLengthY)-(500-smallerAxis)));
			//ctx.lineTo((0.94 * canvas.height), ((0.94 * canvas.height) - (500-smallerAxis)));
			ctx.stroke();
		}	
	}
	// Window resize listener to redraw graph on reszie
	window.addEventListener("resize", redrawGraph);
	
	/*
	Returns nothing.

	This method validates the input from the user. If the textbox is empty, for example
	the button will not cause the application to attempt to draw a graph.
	*/
	function validateTags(){
		var tagX = document.getElementById("x-input").value;
		var tagY = document.getElementById("y-input").value;
		var button = document.getElementsByClassName("createbutton");
		
		document.getElementById("notice").innerHTML = '';
		button[0].removeAttribute('disabled');
		
		var regex = new RegExp("^[a-zA-Z0-9]*$");
		if (!regex.test(tagX) || !regex.test(tagY) || tagX.length < 1 || tagY.length < 1) {
			document.getElementById("notice").innerHTML = "Invalid input. Please check tags.";
			button[0].setAttribute('disabled', 'disabled');
		}
	}
	
