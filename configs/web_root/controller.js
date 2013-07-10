
function CCPaymentCtrl($scope, $http) {
    balanced.init('${REPLACE_THIS_WITH_YOUR_MARKETPLACE_URI}');

    //Process Info to get URI, send to server
    $scope.pay = function (number, month, year, code) {
        console.log(number)
        console.log(month)
        console.log(year)
        console.log(code)
        var creditCardData = {
            card_number: number,
            expiration_month: month,
            expiration_year: year,
            security_code: code
        }
        balanced.card.create(creditCardData, function(response) {
            console.log(response.status)
            console.log(response.data)
            console.log(response.error)
        })

    }
}
