import { Component, OnInit } from '@angular/core';

import {Observable, Subject, BehaviorSubject} from 'rxjs';
import {Http} from '@angular/http';

var url = "http://localhost:8000/vidInfoWrapped/";
var urlCb = "?callback=JSON_CALLBACK";

@Component({
  selector: 'app-rx-item',
  templateUrl: './rx-item.component.html',
  styleUrls: ['./rx-item.component.css']
})

export class RxItemComponent implements OnInit {
  fileName: String = "";
  titleDetailsParsed: Array<any> = [];
  results: any = [];

  searchesSubject = new Subject<string>();
  searchesSubjectNot = new BehaviorSubject<string>("");

  fileNameSearchObservable: any = null;
  blockedLetterObservable: any = null;

  constructor(http:Http) { 
    this.blockedLetterObservable = this.searchesSubjectNot.asObservable();

    this.fileNameSearchObservable = this.searchesSubject.asObservable()
    .debounceTime(200)
    .distinctUntilChanged()
    // pre-process fileName to prevent errors
    .map(fileName => {
      if (fileName === undefined || fileName.length === 0) {
        return "red-info.txt";
      }
      else {
        return fileName;
      }
    });

    Observable.combineLatest 
      (this.fileNameSearchObservable,
        this.blockedLetterObservable,
     (file: String, blocked) => { 
        return {file, blocked} 
      }   
    )
    .filter(({file, blocked}) => { 
      return (blocked.length === 0) || !(file.includes(blocked)); 
    })
    .map(({file, blocked}) => file)
    .switchMap(fileName => {
      var url2 = url + fileName + urlCb;

      return http.get(url2) 
        .map(res => {
          return res.json();
        });
    })
    // handle error checking in a specific Observable, 
    // this is MUCH more elegant than the usual js way
    .filter((data) => { 
      if (data.fileName !== undefined && data.fileName.length !== 0)
        return true;
      else
        return false;
    })
    .map((data) => {
      return { 
        fileName: data.fileName,
        titleDetailsParsed: JSON.parse(data.titleDetails)
      };
    })
    .subscribe((data) => {
        console.log('Returned data: ' + data);
        this.fileName = data.fileName;

        this.results =
          data.titleDetailsParsed
          .map((val, idx) => {
            return {
              titleNumber: val.titleNumber,
              length: val.length
            }
          });                                    
      },
      error => {
        console.error('Error connecting to server.');
      },
      () => {
        console.log('Completed!');
    });    
  }

  ngOnInit() {
  }

  keyup($event){
    this.results = [];
    this.searchesSubject.next($event.currentTarget.value);
  }

  keyupNot($event){
    this.results = [];
    this.searchesSubjectNot.next($event.currentTarget.value);
  }
}
