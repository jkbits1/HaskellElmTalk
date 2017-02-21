/**
 * Created by Jon on 27/11/15.
 */

import {bootstrap} from 'angular2/angular2';
import {HTTP_BINDINGS} from 'angular2/http';
import {JSONP_PROVIDERS} from 'angular2/http';
import {App} from './app';

bootstrap(App, [HTTP_BINDINGS], [JSONP_PROVIDERS])
    .catch(err => console.error(err));
