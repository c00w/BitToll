'use strict';

angular.module('bittoll', []).
  config(['$routeProvider', function($routeProvider) {
  $routeProvider.
      when('/login', {templateUrl: '../html/partials/login.html',   controller: LoginCtrl}).
      when('/dashboard', {templateUrl: '../html/partials/dashboard.html',   controller: DashBoardCtrl}).
      when('/payment/:paymentid', {templateUrl: '../html/partials/payment.html',   controller: PaymentCtrl}).
      otherwise({redirectTo: '/login'});
}]);
