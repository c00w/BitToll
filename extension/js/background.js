
function show(tab) {
    chrome.pageAction.show(tab.id);
    console.log(tab.id);
    chrome.notifications.create("b", opt, null);
};

function showid(tabid) {
    chrome.pageAction.show(tabid);
    console.log(tabid)
    chrome.notifications.create("b", opt, null);
};

var opt = {
    type: "basic",
    title: "bob",
    message: "bob"
}

chrome.tabs.onCreated.addListener(show);


function passMessageToContentScript( payment_return){
}
 
  //listens for messages from the content script 
	//(sent when bittoll field encountered)
chrome.runtime.onMessage.addListener(
  function(request, sender, sendResponse) {
	console.log(sender.tab ?
                "from a content script:" + sender.tab.url :
                "from the extension");
	console.log("blah");
	console.log(request.greeting);
	
	//remove message prefix. 
	//could add future functionality by allowing different prefixes
	var new_payment_id = request.greeting.replace("payment request! id: ", "");
	
	//find requested amount(new_paymentid)
		//waiting on bittoll stuff
		
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
		
		console.log(new_payment_result);
		chrome.tabs.getSelected(null, function(tab) {

		  chrome.tabs.sendMessage(tab.id, {greeting: new_payment_result}, function(response) {
			console.log(response.farewell);
		  });
		});
	//send confirmation back to content script
				
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
*/