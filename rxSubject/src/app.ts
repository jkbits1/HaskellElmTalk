/**
 * Created by Jon on 27/11/15.
 */

import {Component, FORM_DIRECTIVES, CORE_DIRECTIVES, Observable, EventEmitter} from 'angular2/angular2';
import { WheelCalcs } from './wheelCalcs';

import { Subject, BehaviorSubject }    from 'rxjs5';

enum WheelNums { First, Second, Third, Answers };
enum LoopNums { First, Second, Third, Answers };

@Component({
    selector: 'my-app',
    template: `
<br>
<div class="container">
  <div class="row">
    <h2>RxJs Calculations</h2>
  </div>
  <div class="row">
    <h5>Change the values to see the algorithm automatically recalcuate</h5>
  </div>
  <br>
  <div class="row">
    <input #wheel1 type="text" (keyup)="passOnEvent(wheel1subject, $event)" value="1,2,3">
    <input #wheel2 type="text" (keyup)="passOnEvent(wheel2subject, $event)" value="4,5,6">
    <input #wheel3 type="text" (keyup)="passOnEvent(wheel3subject, $event)" value="7,8,9">
    <input #wheel4 type="text" (keyup)="passOnEvent(wheel4subject, $event)" value="12,15,18">
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
      {{loops[1].toString()}}
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
      {{loops[2].toString()}}
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
      {{loops[3].toString()}}
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
  private wheel1subject = new BehaviorSubject<string>("1,2,3");
  private wheel2subject = new BehaviorSubject<string>("4,5,6");
  private wheel3subject = new BehaviorSubject<string>("7,8,9");
  private wheel4subject = new BehaviorSubject<string>("12, 15, 18");

  private wheel1observable : Observable; 
  private wheel2observable : Observable; 
  private wheel3observable : Observable; 
  private wheel4observable : Observable;

  id: number = 0;
  results1 = [];
  results2 = [];
  results3 = [];
  results4 = [];

  wheels: Array<WheelCalcs.WheelPos> = [[], [], [], []];

  loops: Array<WheelCalcs.WheelLoop> = [[], [], [], []];

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

    this.wheel1observable = this.wheel1subject.asObservable();
    this.wheel2observable = this.wheel2subject.asObservable();
    this.wheel3observable = this.wheel3subject.asObservable();
    this.wheel4observable = this.wheel4subject.asObservable();  

    this.calcs = new WheelCalcs.Calcs1();

    this.handleWheelInputs(this.wheel1observable, this.results1, WheelNums.First);

    var wheelNums2 = this.handleWheelInputs(this.wheel2observable, this.results2, WheelNums.Second);
    var wheelNums3 = this.handleWheelInputs(this.wheel3observable, this.results3, WheelNums.Third);
    var wheelNumsAns = this.handleWheelInputs(this.wheel4observable, this.results4, WheelNums.Answers);

    this.numsInputSubscribe(wheelNums2.wheelNums, LoopNums.Second);
    this.numsInputSubscribe(wheelNums3.wheelNums, LoopNums.Third);
    this.numsInputSubscribe(wheelNumsAns.wheelNums, LoopNums.Answers);

    this.wheels[WheelNums.First]    = [1, 2, 3];
    this.wheels[WheelNums.Second]   = [4, 5, 6];
    this.wheels[WheelNums.Third]    = [7, 8, 9];
    this.wheels[WheelNums.Answers]  = [12, 15, 18];
  }

  numsInputSubscribe (numsInput, loopNum) {
    numsInput.subscribe(
      wheelNums => this.loops[loopNum] = this.calcs.createWheelLoop(wheelNums) 
    );
  }

  handleWheelInputs (input, results, wheelPos) {
    var distinctInput, wheelNums;
    
    distinctInput =
      input
        .debounceTime(50)
        .distinctUntilChanged();

    wheelNums =
      distinctInput.map(this.numsFromInput);

    wheelNums
    .subscribe(
      nums => this.wheels[wheelPos] = nums,
      error,
      completed
    );

    distinctInput
    .subscribe(
      this.updateModel(this, results, wheelPos),
      error,
      completed
    );

    function error () {
      console.error('Error');
    }

    function completed () {
      console.log('Completed!');
    }

    return {
      wheelNums : wheelNums
    };
  }

  numsFromInput (inputData) {
    var sNums = inputData.split(",");

    var nums = sNums.map(val => +(val) );

    return nums;
  }

  updateModel (self, results, wheelPos) {

    function processInput (term) {
      console.log('term: ' + term);
      results.push({
        id: this.id++,
        val: term
      });
    }

    function manageModel (term) {
      processInput(term);

      // self.updateCalculations();
    }

    return manageModel;
  }

  updateCalculations () {
    // this.secLoop = this.calcs.createWheelLoop(this.wheels[WheelNums.Second]);
    // this.thrLoop = this.calcs.createWheelLoop(this.wheels[WheelNums.Third]);
    // this.ansLoop = this.calcs.createWheelLoop(this.wheels[WheelNums.Answers]);

    this.perms2 = this.calcs.twoWheelPerms(this.wheels[WheelNums.First], this.secLoop);
    this.perms3 = this.calcs.threeLoopPerms(this.wheels[WheelNums.First],
      this.secLoop, this.thrLoop);

    var c = this.calcs.columnsFromPermutation(this.perms3[0]);

    var a:Array<[WheelCalcs.LoopsPermAnswers, WheelCalcs.LoopsPermutation]> =
      this.calcs.answersPlusPerm(this.wheels[WheelNums.First], this.secLoop, this.thrLoop);

    var f = this.calcs.findSpecificAnswer(this.wheels[WheelNums.First], this.secLoop, this.thrLoop, this.ansLoop);

    this.answer = f;
  }

  passOnEvent (input: Subject, $event: any) {
    input.next($event.currentTarget.value);
  }
}
