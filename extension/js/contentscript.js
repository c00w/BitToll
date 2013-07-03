
console.log("reading page");

//search webpage:
var timeout_length = 1000;
//setTimeout("javascript function",milliseconds);
 

$( document ).ready(function() {
	checkDOMChange();
});

var checkForTags = function() {

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
		MessageBGPage( ("payment request! id: " + $stuff), $stuff);
			//automaticalls waits for return confirmation/failure
		
		return true;
	}	
	else {
		console.log("no bittoll id");
		return false;
	}


}


var MessageBGPage = function( bittollTagContent, prevStuff) {
	chrome.runtime.sendMessage({type: "payment_request", value: bittollTagContent}, function(response){
	  if(response.type == "payment_reply"){
		console.log("value returned: " + response.value);
		$( "[name=bittoll-payment-id]" ).attr("content", (prevStuff + response.value));
		return response.value;
	  }
	});
}



function checkDOMChange()
{
	if (checkForTags() == false){

		// check for any new element being inserted here,
		// or a particular node being modified

		// call the function again after 100 milliseconds
		setTimeout( checkDOMChange, 2000 );
	}
}

