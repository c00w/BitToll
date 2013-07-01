'use strict';

angular.module('bittoll', []).
  config(['$routeProvider', function($routeProvider) {
  $routeProvider.
      when('/login', {templateUrl: 'partials/login.html',   controller: LoginCtrl}).
      otherwise({redirectTo: '/login'});
}]);
