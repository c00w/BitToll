'use strict';

/* Controllers */

function PaymentCtrl($scope, $routeParams, $http) {
    $scope.paymentid = $routeParams.paymentid
    var login = {}
    $scope.accept = function () {
        console.log("Accepted")
    }
    $scope.reject = function () {
        console.log("Rejected")
    }

    $http.post(
        "https://us.bittoll.com/requestinfo",
        makeDict(login,
            {
                "paymentid": $scope.paymentid
            }
        )
    ).
    success(function (data, status, headers, config) {
        $scope.amount = data["amount"]
    })
}

function DashBoardCtrl($scope, $http) {
    $scope.msg = "dashboard"
}

function LoginCtrl($scope, $http, $location, $routeParams) {
    $scope.msg = ""
    if $routeParams.next {
        $scope.next = "/payment/" + $routeParams.next
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

