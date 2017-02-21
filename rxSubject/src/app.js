/**
 * Created by Jon on 27/11/15.
 */
"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var angular2_1 = require('angular2/angular2');
var wheelCalcs_1 = require('./wheelCalcs');
var rxjs5_1 = require('rxjs5');
var WheelNums;
(function (WheelNums) {
    WheelNums[WheelNums["First"] = 0] = "First";
    WheelNums[WheelNums["Second"] = 1] = "Second";
    WheelNums[WheelNums["Third"] = 2] = "Third";
    WheelNums[WheelNums["Answers"] = 3] = "Answers";
})(WheelNums || (WheelNums = {}));
;
var LoopNums;
(function (LoopNums) {
    LoopNums[LoopNums["First"] = 0] = "First";
    LoopNums[LoopNums["Second"] = 1] = "Second";
    LoopNums[LoopNums["Third"] = 2] = "Third";
    LoopNums[LoopNums["Answers"] = 3] = "Answers";
})(LoopNums || (LoopNums = {}));
;
var App = (function () {
    function App() {
        this.wheel1subject = new rxjs5_1.BehaviorSubject("1,2,3");
        this.wheel2subject = new rxjs5_1.BehaviorSubject("4,5,6");
        this.wheel3subject = new rxjs5_1.BehaviorSubject("7,8,9");
        this.wheel4subject = new rxjs5_1.BehaviorSubject("12, 15, 18");
        this.id = 0;
        this.results1 = [];
        this.results2 = [];
        this.results3 = [];
        this.results4 = [];
        this.wheels = [[], [], [], []];
        this.loops = [[], [], [], []];
        this.secLoop = [];
        this.thrLoop = [];
        this.ansLoop = [];
        this.perms2 = [];
        this.perms3 = [];
        this.calcs = undefined;
        this.answer = undefined;
        this.id = 0;
        this.results1 = [1];
        this.results2 = [2];
        this.results3 = [3];
        this.results4 = [4];
        this.wheel1observable = this.wheel1subject.asObservable();
        this.wheel2observable = this.wheel2subject.asObservable();
        this.wheel3observable = this.wheel3subject.asObservable();
        this.wheel4observable = this.wheel4subject.asObservable();
        this.calcs = new wheelCalcs_1.WheelCalcs.Calcs1();
        this.handleWheelInputs(this.wheel1observable, this.results1, WheelNums.First);
        var wheelNums2 = this.handleWheelInputs(this.wheel2observable, this.results2, WheelNums.Second);
        var wheelNums3 = this.handleWheelInputs(this.wheel3observable, this.results3, WheelNums.Third);
        var wheelNumsAns = this.handleWheelInputs(this.wheel4observable, this.results4, WheelNums.Answers);
        this.numsInputSubscribe(wheelNums2.wheelNums, LoopNums.Second);
        this.numsInputSubscribe(wheelNums3.wheelNums, LoopNums.Third);
        this.numsInputSubscribe(wheelNumsAns.wheelNums, LoopNums.Answers);
        this.wheels[WheelNums.First] = [1, 2, 3];
        this.wheels[WheelNums.Second] = [4, 5, 6];
        this.wheels[WheelNums.Third] = [7, 8, 9];
        this.wheels[WheelNums.Answers] = [12, 15, 18];
    }
    App.prototype.numsInputSubscribe = function (numsInput, loopNum) {
        var _this = this;
        numsInput.subscribe(function (wheelNums) { return _this.loops[loopNum] = _this.calcs.createWheelLoop(wheelNums); });
    };
    App.prototype.handleWheelInputs = function (input, results, wheelPos) {
        var _this = this;
        var distinctInput, wheelNums;
        distinctInput =
            input
                .debounceTime(50)
                .distinctUntilChanged();
        wheelNums =
            distinctInput.map(this.numsFromInput);
        wheelNums
            .subscribe(function (nums) { return _this.wheels[wheelPos] = nums; }, error, completed);
        distinctInput
            .subscribe(this.updateModel(this, results, wheelPos), error, completed);
        function error() {
            console.error('Error');
        }
        function completed() {
            console.log('Completed!');
        }
        return {
            wheelNums: wheelNums
        };
    };
    App.prototype.numsFromInput = function (inputData) {
        var sNums = inputData.split(",");
        var nums = sNums.map(function (val) { return +(val); });
        return nums;
    };
    App.prototype.updateModel = function (self, results, wheelPos) {
        function processInput(term) {
            console.log('term: ' + term);
            results.push({
                id: this.id++,
                val: term
            });
        }
        function manageModel(term) {
            processInput(term);
            // self.updateCalculations();
        }
        return manageModel;
    };
    App.prototype.updateCalculations = function () {
        // this.secLoop = this.calcs.createWheelLoop(this.wheels[WheelNums.Second]);
        // this.thrLoop = this.calcs.createWheelLoop(this.wheels[WheelNums.Third]);
        // this.ansLoop = this.calcs.createWheelLoop(this.wheels[WheelNums.Answers]);
        this.perms2 = this.calcs.twoWheelPerms(this.wheels[WheelNums.First], this.secLoop);
        this.perms3 = this.calcs.threeLoopPerms(this.wheels[WheelNums.First], this.secLoop, this.thrLoop);
        var c = this.calcs.columnsFromPermutation(this.perms3[0]);
        var a = this.calcs.answersPlusPerm(this.wheels[WheelNums.First], this.secLoop, this.thrLoop);
        var f = this.calcs.findSpecificAnswer(this.wheels[WheelNums.First], this.secLoop, this.thrLoop, this.ansLoop);
        this.answer = f;
    };
    App.prototype.passOnEvent = function (input, $event) {
        input.next($event.currentTarget.value);
    };
    App = __decorate([
        angular2_1.Component({
            selector: 'my-app',
            template: "\n<br>\n<div class=\"container\">\n  <div class=\"row\">\n    <h2>RxJs Calculations</h2>\n  </div>\n  <div class=\"row\">\n    <h5>Change the values to see the algorithm automatically recalcuate</h5>\n  </div>\n  <br>\n  <div class=\"row\">\n    <input #wheel1 type=\"text\" (keyup)=\"passOnEvent(wheel1subject, $event)\" value=\"1,2,3\">\n    <input #wheel2 type=\"text\" (keyup)=\"passOnEvent(wheel2subject, $event)\" value=\"4,5,6\">\n    <input #wheel3 type=\"text\" (keyup)=\"passOnEvent(wheel3subject, $event)\" value=\"7,8,9\">\n    <input #wheel4 type=\"text\" (keyup)=\"passOnEvent(wheel4subject, $event)\" value=\"12,15,18\">\n  </div>\n\n  <br>\n  <div class=\"row\">\n    <div class=\"col-sm-2 itemLabel\">\n      Wheel 1\n    </div>\n    <div class=\"col-sm-2\">\n      {{wheels[0].toString()}}\n    </div>\n  </div>\n  <br>\n  <div class=\"row\">\n    <div class=\"col-sm-2 itemLabel\">\n      Wheel 2\n    </div>\n    <div class=\"col-sm-2\">\n      {{wheels[1].toString()}}\n    </div>\n    <div class=\"col-sm-2 itemLabel\">\n      Loop 2\n    </div>\n    <div class=\"col-sm-2\">\n      {{loops[1].toString()}}\n    </div>\n  </div>\n  <br>\n  <div class=\"row\">\n    <div class=\"col-sm-2 itemLabel\">\n      Wheel 3\n    </div>\n    <div class=\"col-sm-2\">\n      {{wheels[2].toString()}}\n    </div>\n    <div class=\"col-sm-2 itemLabel\">\n      Loop 3\n    </div>\n    <div class=\"col-sm-2\">\n      {{loops[2].toString()}}\n    </div>\n  </div>\n  <br>\n  <div class=\"row\">\n    <div class=\"col-sm-2 itemLabel\">\n      Wheel 4\n    </div>\n    <div class=\"col-sm-2\">\n      {{wheels[3].toString()}}\n    </div>\n    <div class=\"col-sm-2 itemLabel\">\n      Loop ans\n    </div>\n    <div class=\"col-sm-2\">\n      {{loops[3].toString()}}\n    </div>\n  </div>\n\n  <br>\n  <div class=\"row\">\n    <div class=\"col-sm-2 itemLabel\">\n      Perms 2\n    </div>\n    <div class=\"col-sm-8\">\n      {{perms2}}\n    </div>\n  </div>\n\n  <br>\n  <div class=\"row\">\n    <div class=\"col-sm-2 itemLabel\">\n      Perms 3\n    </div>\n    <div class=\"col-sm-6\">\n      {{perms3}}\n    </div>\n  </div>\n  <br>\n  <div class=\"row\">\n    <div class=\"col-sm-2 itemLabel\">\n      Answer\n    </div>\n    <div class=\"col-sm-6\">\n      {{answer}}\n    </div>\n  </div>\n<br>\n\n</div>\n",
            directives: [angular2_1.CORE_DIRECTIVES, angular2_1.FORM_DIRECTIVES]
        })
    ], App);
    return App;
}());
exports.App = App;
//# sourceMappingURL=app.js.map