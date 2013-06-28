
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
chrome.tabs.onSelectionChanged.addListener(showid);
