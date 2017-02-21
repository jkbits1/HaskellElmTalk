/**
 * Created by Jon on 27/11/15.
 */
"use strict";
var angular2_1 = require('angular2/angular2');
var http_1 = require('angular2/http');
var http_2 = require('angular2/http');
var app_1 = require('./app');
angular2_1.bootstrap(app_1.App, [http_1.HTTP_BINDINGS], [http_2.JSONP_PROVIDERS])
    .catch(function (err) { return console.error(err); });
//# sourceMappingURL=main.js.map