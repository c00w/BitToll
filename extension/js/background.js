var loginInfo = null;

var opt = {
    type: "basic",
    title: "bob",
    message: "bob"
}

function passMessageToContentScript( payment_return){
}
 
  //listens for messages from the content script 
	//(sent when bittoll field encountered)
var handle_payment = function (request, sender, sendResponse) {
	var new_payment_id = request.payment_request
		
	//ask user if it is okay
		//can this be done better?
		var foo = confirm("The page at " + sender.tab.url + 
			"is requesting a bittoll payment of $.blah" +
			"\n\n" + "accept this charge?");
		
		var new_payment_result;
		if(foo){
			//run bittoll transaction(new_payment_id)
			//new_payment_result = bittoll__something_or_other(blah);
			new_payment_result = ";confirmed;lkjl";
			
		}
		else{
			new_payment_result = ";denied;";
		
		}
		
		sendResponse({type: "payment_reply", value: new_payment_result});
		console.log(new_payment_result);
	//send confirmation back to content script


}
chrome.runtime.onMessage.addListener(
  function(request, sender, sendResponse) {
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
