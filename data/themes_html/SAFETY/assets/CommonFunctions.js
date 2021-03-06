// BEGIN: Node Stream Source page - switching of currently selected coded stream range
	function setSelectedTimespan(_timespanValue, _timespanCodingCoverageMap) {
		// Get all anchor tags in the audio/video page
		var mediaReferences = document.getElementsByTagName("a");
		
		for (hyperlinkCounter = 0; hyperlinkCounter < mediaReferences.length; hyperlinkCounter++) {
			if (mediaReferences[hyperlinkCounter].className == "green-box current") {
				// Change colour of hyperlink background to green to indicate that it is not the currently selected timespan
				mediaReferences[hyperlinkCounter].className = "green-box";
			}
			else if (mediaReferences[hyperlinkCounter].innerHTML == _timespanValue) {
				// Change colour of hyperlink background to grey to indicate that it is the currently selected timespan
				mediaReferences[hyperlinkCounter].className = "green-box current";
			}
		}
		
		// Update the "Media reference currently displayed" subsection of the "Media References" section
		var divTags = document.getElementsByTagName("div");
		
		for (divCounter = 0; divCounter < divTags.length; divCounter++) {
			if (divTags[divCounter].id == "divTimespan") {
				divTags[divCounter].innerHTML = _timespanValue;
			}
			else if (divTags[divCounter].id == "divCoverage") {
				divTags[divCounter].innerHTML = _timespanCodingCoverageMap.get(_timespanValue);
			}
		}
	}
// END: Node Stream Source page - switching of currently selected coded stream range

// BEGIN: Javascript implementation of the Java Hashtable object
	function Hashtable(){
	    this.clear = hashtable_clear;
	    this.containsKey = hashtable_containsKey;
	    this.containsValue = hashtable_containsValue;
	    this.get = hashtable_get;
	    this.isEmpty = hashtable_isEmpty;
	    this.keys = hashtable_keys;
	    this.put = hashtable_put;
	    this.remove = hashtable_remove;
	    this.size = hashtable_size;
	    this.toString = hashtable_toString;
	    this.values = hashtable_values;
	    this.hashtable = new Array();
	}
	
	/*=======Private methods for internal use only========*/
	
	function hashtable_clear(){
	    this.hashtable = new Array();
	}
	
	function hashtable_containsKey(key){
	    var exists = false;
	    for (var i in this.hashtable) {
	        if (i == key && this.hashtable[i] != null) {
	            exists = true;
	            break;
	        }
	    }
	    return exists;
	}
	
	function hashtable_containsValue(value){
	    var contains = false;
	    if (value != null) {
	        for (var i in this.hashtable) {
	            if (this.hashtable[i] == value) {
	                contains = true;
	                break;
	            }
	        }
	    }
	    return contains;
	}
	
	function hashtable_get(key){
	    return this.hashtable[key];
	}
	
	function hashtable_isEmpty(){
	    return (parseInt(this.size()) == 0) ? true : false;
	}
	
	function hashtable_keys(){
	    var keys = new Array();
	    for (var i in this.hashtable) {
	        if (this.hashtable[i] != null) 
	            keys.push(i);
	    }
	    return keys;
	}
	
	function hashtable_put(key, value){
	    if (key == null || value == null) {
	        throw "NullPointerException {" + key + "},{" + value + "}";
	    }else{
	        this.hashtable[key] = value;
	    }
	}
	
	function hashtable_remove(key){
	    var rtn = this.hashtable[key];
	    this.hashtable[key] = null;
	    return rtn;
	}

	function hashtable_size(){
	    var size = 0;
	    for (var i in this.hashtable) {
	        if (this.hashtable[i] != null) 
	            size ++;
	    }
	    return size;
	}
	
	function hashtable_toString(){
	    var result = "";
	    for (var i in this.hashtable)
	    {      
	        if (this.hashtable[i] != null) 
	            result += "{" + i + "},{" + this.hashtable[i] + "}\n";   
	    }
	    return result;
	}
	
	function hashtable_values(){
	    var values = new Array();
	    for (var i in this.hashtable) {
	        if (this.hashtable[i] != null) 
	            values.push(this.hashtable[i]);
	    }
	    return values;
	}
// END: Javascript implementation of the Java Hashtable object

// BEGIN: Javascript browser detection code
	var _browserDetect = {
		init: function () {
			this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
			this.version = this.searchVersion(navigator.userAgent)
				|| this.searchVersion(navigator.appVersion)
				|| "an unknown version";
			this.OS = this.searchString(this.dataOS) || "an unknown OS";
		},
		searchString: function (data) {
			for (var i=0;i<data.length;i++) {
				var dataString = data[i].string;
				var dataProp = data[i].prop;
				this.versionSearchString = data[i].versionSearch || data[i].identity;
				if (dataString) {
					if (dataString.indexOf(data[i].subString) != -1)
						return data[i].identity;
				}
				else if (dataProp)
					return data[i].identity;
			}
		},
		searchVersion: function (dataString) {
			var index = dataString.indexOf(this.versionSearchString);
			if (index == -1) return;
				return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
		},
		dataBrowser: [
			{	string: navigator.userAgent,
				subString: "OmniWeb",
				versionSearch: "OmniWeb/",
				identity: "OmniWeb"
			},
			{
				string: navigator.vendor,
				subString: "Apple",
				identity: "Safari"
			},
			{
				prop: window.opera,
				identity: "Opera"
			},
			{
				string: navigator.vendor,
				subString: "iCab",
				identity: "iCab"
			},
			{
				string: navigator.vendor,
				subString: "KDE",
				identity: "Konqueror"
			},
			{
				string: navigator.userAgent,
				subString: "Firefox",
				identity: "Firefox"
			},
			{
				string: navigator.vendor,
				subString: "Camino",
				identity: "Camino"
			},
			{	// for newer Netscapes (6+)
				string: navigator.userAgent,
				subString: "Netscape",
				identity: "Netscape"
			},
			{
				string: navigator.userAgent,
				subString: "MSIE",
				identity: "Explorer",
				versionSearch: "MSIE"
			},
			{
				string: navigator.userAgent,
				subString: "Gecko",
				identity: "Mozilla",
				versionSearch: "rv"
			},
			{	// for older Netscapes (4-)
				string: navigator.userAgent,
				subString: "Mozilla",
				identity: "Netscape",
				versionSearch: "Mozilla"
			}
		],
		dataOS : [
			{
				string: navigator.platform,
				subString: "Win",
				identity: "Windows"
			},
			{
				string: navigator.platform,
				subString: "Mac",
				identity: "Mac"
			},
			{
				string: navigator.platform,
				subString: "Linux",
				identity: "Linux"
			}
		]
	};
	_browserDetect.init();
// END: Javascript browser detection code

// BEGIN: JavaScript implementation of String.format function
	function _StringFormatStatic() {
		for(var i=1;i<arguments.length;i++) {
			var exp = new RegExp('\\{' + (i-1) + '\\}','gm');
			arguments[0] = arguments[0].replace(exp,arguments[i]);
		}
		return arguments[0];
	}

	if(!String.format) {
		String.format = _StringFormatStatic;
	}
// END: JavaScript implementation of String.format function

// BEGIN: Popup for displaying lengthy text content (Datasets)
	var ExpandPopupObject = null;

	// Show the popup
	function ExpandPopup(aX, aY, aData) {
		ExpandPopupHide();

		var container = document.createElement('div');
		container.id = 'ExpandPopupContainer';
		
		// Ensure that popup will display in its entirety in the window
		var viewportSize = getViewportSize();
		var viewportWidth = viewportSize[0];
		var viewportHeight = viewportSize[1];
		
		// popupWidth and popupHeight obtained from NVivoHtmlExport.css, #ExpandPopupContainer (lines 610 and 611)
		var popupWidth = 250;
		var popupHeight = 125;
		
		if (aX + popupWidth > viewportWidth) {
			// Reposition left edge of popup so that entire width of popup appears in browser window
			// and a corner of the popup is anchored to its originating hyperlink
			aX -= popupWidth;
		}
		
		if (aY + popupHeight > viewportHeight) {
			// Reposition top edge of popup so that entire height of popup appears in browser window
			// and a corner of the popup is anchored to its originating hyperlink
			aY -= popupHeight;
		}
		
		container.style.left = aX + 'px';
		container.style.top = aY + 'px';
		container.innerHTML = '<input type="button" class="ExpandPopupButton" value="X" onclick="ExpandPopupHide();" />';

		// the content div (within the pop-up container)
		var content = document.createElement('div');
		content.className = 'ExpandPopupContents';
		content.innerHTML = aData;
		container.appendChild(content);	// add it to the container
	
		ExpandPopupObject = container;
		document.body.appendChild(container);
	}

	// Remove the popup container from the DOM
	function ExpandPopupHide() {
		if(ExpandPopupObject != null) {
			document.body.removeChild(ExpandPopupObject);
			ExpandPopupObject = null;
		}
	}
	
	// Viewport is the inner area a HTML page appears in a web browser.
	// Code obtained from http://andylangton.co.uk/articles/javascript/get-viewport-size-javascript/
	function getViewportSize() {
		var viewportWidth = 0;
		var viewportHeight = 0;
		
		if (typeof window.innerWidth != 'undefined') {
			// Applicable to standards compliant browsers (mozilla/netscape/opera/IE7+)
			viewportWidth = window.innerWidth;
			viewportHeight = window.innerHeight;
		}
		else if ((typeof document.documentElement != 'undefined')
			&& (typeof document.documentElement.clientWidth != 'undefined')
			&& (document.documentElement.clientWidth != 0)) {
				
			// Applicble to IE6 with a valid doctype as the first line in the document
			viewportWidth = document.documentElement.clientWidth;
			viewportHeight = document.documentElement.clientHeight;
		}
		else {
			// Older versions of IE
			viewportWidth = document.getElementByTagName('body')[0].clientWidth;
			viewportHeight = document.getElementByTagName('body')[0].clientHeight;
		}
		
		return [viewportWidth, viewportHeight];
	}
// END: Popup for displaying lengthy text content (Datasets)

// BEGIN: Function for stripping out HTML tags from a string
	/* This script and many more are available free online at
	The JavaScript Source!! http://javascript.internet.com
	Created by: Robert Nyman | http://robertnyman.com/ */
	function removeHTMLTags(strInputCode){
		// This line is optional, it replaces escaped brackets with real ones, 
		// i.e. < is replaced with < and > is replaced with >
		strInputCode = strInputCode.replace(/&(lt|gt);/g, function (strMatch, p1){
			return (p1 == "lt")? "<" : ">";
		});
		
		return strInputCode.replace(/<\/?[^>]+(>|$)/g, "");
	}
// END: Function for stripping out HTML tags from a string