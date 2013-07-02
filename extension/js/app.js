'use strict';

angular.module('bittoll', []).
  config(['$routeProvider', function($routeProvider) {
  $routeProvider.
      when('/login', {templateUrl: 'html/partials/login.html',   controller: LoginCtrl}).
      otherwise({redirectTo: '/login'});
}]);
