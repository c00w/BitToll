var loginInfo = null;
var paymentCallbacks = {};

function passMessageToContentScript( payment_return){
}

//listens for messages from the content script
//(sent when bittoll field encountered)
var handle_payment = function (request, sender, sendResponse) {

	var new_payment_id = request.value

    //If we are not logged in, deal with that first
    if (loginInfo !== null) {
        var paymenturl = chrome.extension.getURL("/html/option.html#/payment/" + new_payment_id)
    } else {
        var paymenturl = chrome.extension.getURL("/html/option.html#/login?next=/payment/" + new_payment_id)
    }

    console.log("Opening: " + paymenturl)
    chrome.tabs.create({"url":paymenturl})

    paymentCallbacks[new_payment_id] = function(response) {

    var new_payment_result = response.value
	//send confirmation back to content script
    sendResponse({type: "payment_reply", value: new_payment_result});
    console.log(new_payment_result);
    }
}

function handle_payment_response(request) {
    var paymentid = request.id
    var response = request.value
    var callback = paymentcallacks[paymentid];
    if (callback === undefined) {
        console.log("no callback for" + paymentid)
        return
    }
    callback(response)
}

chrome.runtime.onMessage.addListener(function(request,
                                              sender,
                                              sendResponse) {
	console.log(sender.tab ?
                "from a content script:" + sender.tab.url :
                "from the extension");
	console.log(request);

	//remove message prefix.
	//could add future functionality by allowing different prefixes
	if (request.type == "payment_request" ){
		handle_payment(request, sender, sendResponse);
        //Keep the response open since we want to use it beyond the scope of this event handler
        return true;
	}
	else if(request.type == "login_save") {
		loginInfo = request.value;
	}
    else if (request.type == "login_request") {
        sendResponse({"value":loginInfo})
    }
    else if (request.type == "payment_response") {
        handle_payment_response(request);
    } else {
        console.log("unknown message")
        console.log(request)
	}
});

/*
chrome.runtime.onMessage.addListener(
  function(request, sender, sendResponse) {
    console.log(sender.tab ?
                "from a content script:" + sender.tab.url :
                "from the extension");
    if (request.greeting == "hello")
      sendResponse({farewell: "goodbye"});
  });

  		chrome.tabs.getSelected(null, function(tab) {

		  chrome.tabs.sendMessage(tab.id, {greeting: new_payment_result}, function(response) {
			console.log(response.farewell);
		  });
		});

*/
