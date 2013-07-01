
console.log("reading page");

//search webpage:
var count = function() {
	var text = "Hello, max!";
	return text;
	
}

$( document ).ready(function() {
 
		// Your code here.
	if(	$( "[name=bittoll-id]" ).length ) {
		console.log("found a bittoll-id tag!");
		//is able to successfully find a meta tag named bittoll!
		
		
		//now i get the value of the content tag thing that follows it
		var $stuff = $( "[name=bittoll-id]" ).attr("content");
		console.log("content of that tag:")
		console.log($stuff);
		//this also works!
		
		//now, I am going to try to check if they have a "returnid" field.
		//if so, will try to inject a value into it
		
		
		console.log( typeof(("found bittoll tag! id: " + $stuff) ));
		
		
		//try to pass a message back to the background page
		passMessageToBGPage( ("found bittoll tag! id: " + $stuff) );
	}	
	else {
		console.log("no bittoll id");
	}
});

function passMessageToBGPage( bittollTagContent ) {   
	chrome.runtime.sendMessage({greeting: bittollTagContent});
}




