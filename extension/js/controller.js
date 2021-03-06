'use strict';

/* Controllers */

function PaymentCtrl($scope, $routeParams, $http) {
    $scope.paymentid = $routeParams.paymentid
    $scope.msg = "Loading Payment Details"
    $scope.can_pay = false
    var login = undefined
    $scope.accept = function () {
        console.log("Accepted")
        $scope.msg = "Making Payment"
        $http.post(
            "https://us.bittoll.com/pay",
            makeDict(login,
                {
                    "payment":$scope.paymentid
                }
            )).
        success(function (data, status, headers, config) {
            if (data.error_code !== "0") {
                console.log(data)
                $scope.msg = "Error making payment"
                return
            }
            $scope.msg ="Payment Accepted"
            chrome.runtime.sendMessage({"type":"payment_response", "value":true, "id":$scope.paymentid})
        }).
        error(function (data, status, headers, config) {
            console.log(data)
            $scope.msg = "Error making payment"
        })
    }

    $scope.reject = function () {
        console.log("Rejected")
        chrome.runtime.sendMessage({"type":"payment_response", "value":false, "id":$scope.paymentid})
    }

    chrome.runtime.sendMessage({"type":"login_request"}, function(response) {
        login = response.value
        $http.post(
            "https://us.bittoll.com/requestinfo",
            makeDict(login,
                {
                    "paymentid": $scope.paymentid
                }
            )
        ).
        success(function (data, status, headers, config) {
            console.log(data)
            $scope.msg = "Would you like to pay " + data.amount
            if (data.balance === "1") {
                $scope.can_pay = true
            }
        }).
        error(function (data, status, headers, config) {
            $scope.msg = "failure loading payment"
        })
    })
}

function DashBoardCtrl($scope, $http) {
    $scope.msg = "dashboard"
}

function LoginCtrl($scope, $http, $location, $routeParams) {
    $scope.msg = ""
    if ($routeParams.next) {
        $scope.next = $routeParams.next
    } else {
        $scope.next = "/dashboard"
    }
    $scope.tryLogin = function (username, password) {
        $scope.msg = "Working"
        $http.post(
            "https://us.bittoll.com/alias",
            {
                "aliasName": username,
                "aliasPassword": password,
            }
        ).
        success(function(data, status, haders, config) {
            if (data["error_code"] !== "0") {
                console.log("error")
                console.log(data)
                $scope.msg = "Error Logging In"
                return
            }
            delete(data["error_code"])
            $scope.msg = "Success"
            $scope.login = data
			chrome.runtime.sendMessage({type: "login_save", value: data});

            $location.search("next", undefined)
            $location.path($scope.next)
        }).
        error(function(data, status, headers, config) {
            console.log("error")
            console.log(data)
            console.log(status)
            $scope.msg = "Error communicating with server"
        })
    }
    $scope.register = function (username, password) {
        $scope.msg = "Working"

        $http.post("https://us.bittoll.com/register","{}").
        success(function(data, status, headers, config) {
            if (data["error_code"] !== "0") {
                console.log("error")
                console.log(data)
                $scope.msg = "Error Registering"
                return
            }
            var login = data
            delete(login["error_code"])
            $scope.msg = "Working..."

            $http.post(
                "https://us.bittoll.com/setalias",
                makeDict(login,
                    {
                        "aliasName":username,
                        "aliasPassword":password,
                    }
                )
            ).
            success(function(data, status, headers, config) {
                if (data["error_code"] !== "0") {
                    $scope.login = undefined
                    console.log("succ err")
                    $scope.msg = "Error registering"
                    return
                }
                $scope.login = login
                $scope.msg = "Success"
                $location.search("next", undefined)
                $location.path($scope.next)
            }).
            error(function(data, status, headers, config) {
                $scope.login = undefined
                $scope.msg = "Error Communicating with server"
                console.log(data)
                console.log("succ err")
            })
        }).
        error(function(data, status, headers, config) {
            console.log("error")
            console.log(data)
            $scope.msg = "Error Communicating with server"
        })
    }
}

