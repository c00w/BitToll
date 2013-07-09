
console.log("reading page");

$(document).ready(function() {

    var current = $("[name=bittoll-payment-id]");
    console.log(current)

    var obs = new MutationObserver(processMutation);
    var config = {};
    //Look at all subchildren
    config.subtree = true;
    // Only monitor for children changes
    config.childList = true;
    obs.observe(document, config);

    checkForTags(current);
})

function processMutation (record) {
    console.log(record)
    for (var i = 0; i < record.length; i ++){
        var current = record[i].addedNodes
        checkForTags(current)
    }
}

var checkForTags = function(current) {

    for (var i = 0; i < current.length; i++) {
        var element = $(current[i]);

        if (element.attr("name") !== "bittoll-payment-id") {
            continue;
        }
		console.log("found a bittoll-id tag!");
		//is able to successfully find a meta tag named bittoll!
		//now i get the value of the content tag thing that follows it
		var stuff = element.attr("content");
		console.log("content of that tag:");
		console.log(stuff);
		//this also works!

		//tell website it is processing
		console.log("trying to change");
		element.attr("content", (stuff + ";processing") );

		//try to pass a message back to the background page
		MessageBGPage( stuff, stuff);
	    //automaticalls waits for return confirmation/failure
	}
}


var MessageBGPage = function( bittollTagContent, prevStuff) {
    chrome.runtime.sendMessage({type: "payment_request", value: bittollTagContent}, function(response){
        if(response.type == "payment_reply"){
            console.log("value returned: " + response.value);
            if (response.value) {
                $( "[name=bittoll-payment-id]" ).attr("content", (prevStuff + ";accepted"));
            }
            else {
                $( "[name=bittoll-payment-id]" ).attr("content", (prevStuff + ";rejected"));
            }
            return response.value;
        }
	});
}
