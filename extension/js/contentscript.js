
console.log("reading page");

//search webpage:
var timeout_length = 1000;
//setTimeout("javascript function",milliseconds);
 

$( document ).ready(function() {
 
		// Your code here.
	if(	$( "[name=bittoll-payment-id]" ).length ) {
		console.log("found a bittoll-id tag!");
		//is able to successfully find a meta tag named bittoll!
		
		
		//now i get the value of the content tag thing that follows it
		var $stuff = $( "[name=bittoll-payment-id]" ).attr("content");
		console.log("content of that tag:");
		console.log($stuff);
		//this also works!
		
		
		//tell website it is processing
		console.log("trying to change");
		$( "[name=bittoll-payment-id]" ).attr("content", ($stuff + ";processing") );
		
		
		
		//try to pass a message back to the background page
		var return_val = MessageBGPage( ("payment request! id: " + $stuff) );
			//automaticalls waits for return confirmation/failure
		console.log("sent");
		
	}	
	else {
		console.log("no bittoll id");
	}
});

function MessageBGPage( bittollTagContent ) {   
	chrome.runtime.sendMessage({greeting: bittollTagContent}, function(response){
		console.log("waiting1");
		waitForMessage();
		console.log("waiting2");
	});
}


//wait for response
chrome.runtime.onMessage.addListener(
  function(request, sender, sendResponse) {
	console.log("response: " + request.greeting);
	
	//insert reponse back into the page
	if(	$( "[name=bittoll-payment-id]" ).length ) {
	  var $stuff = $( "[name=bittoll-payment-id]" ).attr("content");
	  $stuff = $stuff.slice( 0, $stuff.indexOf(";") );
	  $( "[name=bittoll-payment-id]" ).attr("content", ($stuff + request.greeting) );
	  console.log("response injected");
	}
  });
	  
	  
//next up: make message passing two-way, code for insertion into page




