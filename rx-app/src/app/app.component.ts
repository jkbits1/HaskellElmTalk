import { Component } from '@angular/core';

import { Subject, BehaviorSubject } from 'rxjs';

// this import form is more efficient for bandwidth
import {Observable} from 'rxjs/Observable';


@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})

export class AppComponent {
  title = 'Video Info Client';
}

