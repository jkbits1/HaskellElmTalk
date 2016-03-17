/**
 * Created by Jon on 27/11/15.
 */

import {Component, FORM_DIRECTIVES, CORE_DIRECTIVES, Observable, EventEmitter} from 'angular2/angular2';
import { WheelCalcs } from './wheelCalcs';

enum WheelNums { First, Second, Third, Answers };

@Component({
    selector: 'my-app',
    template: `
<br>
<div class="container">
  <div class="row">
    <h2>RxJs Calculations</h2>
  </div>
  <br>
  <div class="row">
    <input #wheel1 type="text" (keyup)="keyup1($event)" value="1,2,3">
    <input #wheel2 type="text" (keyup)="keyup2($event)" value="4,5,6">
    <input #wheel3 type="text" (keyup)="keyup3($event)" value="7,8,9">
    <input #wheel4 type="text" (keyup)="keyup4($event)" value="12,15,18">
    <button (click)="testclick($event)">test</button>
  </div>

  <br>
  <div class="row">
    <div class="col-sm-2 itemLabel">
      Wheel 1
    </div>
    <div class="col-sm-2">
      {{wheels[0].toString()}}
    </div>
  </div>
  <br>
  <div class="row">
    <div class="col-sm-2 itemLabel">
      Wheel 2
    </div>
    <div class="col-sm-2">
      {{wheels[1].toString()}}
    </div>
    <div class="col-sm-2 itemLabel">
      Loop 2
    </div>
    <div class="col-sm-2">
      {{secLoop}}
    </div>
  </div>
  <br>
  <div class="row">
    <div class="col-sm-2 itemLabel">
      Wheel 3
    </div>
    <div class="col-sm-2">
      {{wheels[2].toString()}}
    </div>
    <div class="col-sm-2 itemLabel">
      Loop 3
    </div>
    <div class="col-sm-2">
      {{thrLoop}}
    </div>
  </div>
  <br>
  <div class="row">
    <div class="col-sm-2 itemLabel">
      Wheel 4
    </div>
    <div class="col-sm-2">
      {{wheels[3].toString()}}
    </div>
    <div class="col-sm-2 itemLabel">
      Loop ans
    </div>
    <div class="col-sm-2">
      {{ansLoop}}
    </div>
  </div>

  <br>
  <div class="row">
    <div class="col-sm-2 itemLabel">
      Perms 2
    </div>
    <div class="col-sm-8">
      {{perms2}}
    </div>
  </div>

  <br>
  <div class="row">
    <div class="col-sm-2 itemLabel">
      Perms 3
    </div>
    <div class="col-sm-6">
      {{perms3}}
    </div>
  </div>
  <br>
  <div class="row">
    <div class="col-sm-2 itemLabel">
      Answer
    </div>
    <div class="col-sm-6">
      {{answer}}
    </div>
  </div>
<br>

</div>
`,
directives: [CORE_DIRECTIVES, FORM_DIRECTIVES]
})
export class App {
  wheel1input = new EventEmitter();
  wheel2input = new EventEmitter();
  wheel3input = new EventEmitter();
  wheel4input = new EventEmitter();

  id: number = 0;
  results1 = [];
  results2 = [];
  results3 = [];
  results4 = [];

  wheels: Array<WheelCalcs.WheelPos> = [[],[], [], []];

  secLoop: WheelCalcs.WheelLoop = [];
  thrLoop: WheelCalcs.WheelLoop = [];
  ansLoop: WheelCalcs.WheelLoop = [];

  perms2: Array<WheelCalcs.LoopsPermutation> = [];
  perms3: Array<WheelCalcs.LoopsPermutation> = [];

  calcs: WheelCalcs.Calcs1 = undefined;

  answer: Any = undefined;

  constructor() {
    this.id = 0;

    this.results1 = [1];
    this.results2 = [2];
    this.results3 = [3];
    this.results4 = [4];

    this.handleWheelInputs(this.wheel1input._subject, this.results1, WheelNums.First);
    this.handleWheelInputs(this.wheel2input._subject, this.results2, WheelNums.Second);
    this.handleWheelInputs(this.wheel3input._subject, this.results3, WheelNums.Third);
    this.handleWheelInputs(this.wheel4input._subject, this.results4, WheelNums.Answers);

    this.calcs = new WheelCalcs.Calcs1();

    this.wheels[WheelNums.First]    = [1, 2, 3];
    this.wheels[WheelNums.Second]   = [4, 5, 6];
    this.wheels[WheelNums.Third]    = [7, 8, 9];
    this.wheels[WheelNums.Answers]  = [12, 15, 18];
  }

  handleWheelInputs (subject, results, wheelPos) {
    subject
      .debounceTime(50)
      .distinctUntilChanged()
      .subscribe(
        this.updateModel(this, results, wheelPos),
        error => {
          console.error('Error');
        },
        () => {
          console.log('Completed!');
        }
      );
  }

  updateModel (self, results, wheelPos) {

    function processInput (term) {
      console.log('term: ' + term);
      results.push({
        id: this.id++,
        val: term
      });

      var sNums = term.split(",");

      var nums = sNums.map(val => +(val) );

      self.wheels[wheelPos] = nums;
    }

    function manageModel (term) {
      processInput(term);

      self.updateCalculations();
    }

    return manageModel;
  }

  updateCalculations () {
    this.secLoop = this.calcs.createWheelLoop(this.wheels[WheelNums.Second]);
    this.thrLoop = this.calcs.createWheelLoop(this.wheels[WheelNums.Third]);
    this.ansLoop = this.calcs.createWheelLoop(this.wheels[WheelNums.Answers]);

    this.perms2 = this.calcs.twoWheelPerms(this.wheels[WheelNums.First], this.secLoop);
    this.perms3 = this.calcs.threeLoopPerms(this.wheels[WheelNums.First],
      this.secLoop, this.thrLoop);

    var c = this.calcs.columnsFromPermutation(this.perms3[0]);

    var a:Array<[WheelCalcs.LoopsPermAnswers, WheelCalcs.LoopsPermutation]> =
      this.calcs.answersPlusPerm(this.wheels[WheelNums.First], this.secLoop, this.thrLoop);

    var f = this.calcs.findSpecificAnswer(this.wheels[WheelNums.First], this.secLoop, this.thrLoop, this.ansLoop);

    this.answer = f;
  }

  passOnEvent (input: EventEmitter, $event: any) {
    input.next($event.currentTarget.value);
  }


  keyup1 ($event) {
    this.passOnEvent(this.wheel1input, $event);
  }

  keyup2 ($event) {
    this.passOnEvent(this.wheel2input, $event);
  }

  keyup3 ($event) {
    this.passOnEvent(this.wheel3input, $event);
  }

  keyup4 ($event) {
    this.passOnEvent(this.wheel4input, $event);
  }

  testclick ($event) {
  }
}
