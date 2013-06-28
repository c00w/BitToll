'use strict';

/* Controllers */



function LoginCtrl($scope, $http) {
    $scope.hi = "hello world"
    $scope.tryLogin = function (username, password) {
        console.log(username)
        console.log(password)
    }
    $scope.register = function (username, password) {
        $http.post("https://us.bittoll.com/register","{}").
        success(function(data, status, headers, config) {
            var login = data
            delete(login["error_code"])
            console.log("succ")
            console.log(login)

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
                console.log(data)
                $scope.login = login
                console.log("succ al")
            }).
            error(function(data, status, headers, config) {
                $scope.login = undefined
                console.log("succ err")
            })
        }).
        error(function(data, status, headers, config) {
            console.log("error")
            console.log(data)
        })
        console.log("register")
        console.log(username)
        console.log(password)
    }
}

