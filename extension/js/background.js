var loginInfo = null;

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
    window.open(paymenturl)

	//send confirmation back to content script
    sendResponse({type: "payment_reply", value: new_payment_result});
    console.log(new_payment_result);
}

chrome.runtime.onMessage.addListener(function(request,
                                              sender,
                                              sendResponse) {
	console.log(sender.tab ?
                "from a content script:" + sender.tab.url :
                "from the extension");
	console.log("blah");
	console.log(request.value);

	//remove message prefix.
	//could add future functionality by allowing different prefixes
	if (request.type == "payment_request" ){
		handle_payment(request, sender, sendResponse);

	}
	else if(request.type == "login_save") {
		loginInfo = request.value;

	}
	else{

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
