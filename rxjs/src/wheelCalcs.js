/**
 * Created by Jon on 28/02/16.
 */
"use strict";
var WheelCalcs;
(function (WheelCalcs) {
    var Calcs1 = (function () {
        function Calcs1() {
        }
        Calcs1.prototype.turnWheel = function (wheel, turns) {
            var dropPart = R.drop(turns, wheel);
            var takePart = R.take(turns, wheel);
            return dropPart.concat(takePart);
        };
        Calcs1.prototype.getWheelLoop = function (positions, pos, count) {
            if (count === 0) {
                return [pos].concat(positions);
            }
            return this.getWheelLoop([this.turnWheel(pos, count)].concat(positions), pos, count - 1);
        };
        Calcs1.prototype.createWheelLoop = function (initialPos) {
            return this.getWheelLoop([], initialPos, initialPos.length - 1);
        };
        Calcs1.prototype.twoWheelPerms = function (first, secLoop) {
            return secLoop.map(function (secPos) { return [first].concat([secPos]); });
        };
        Calcs1.prototype.appendTwoWheelPerms = function (twoWheelPermsLocal, thrPos) {
            return twoWheelPermsLocal.map(function (twoLoopsPerm) { return twoLoopsPerm.concat([thrPos]); });
        };
        Calcs1.prototype.threeLoopPerms = function (first, secLoop, thrLoop) {
            var self = this;
            var twoWheelPermsLocal = self.twoWheelPerms(first, secLoop);
            // AS CURRIED FUNCTION
            // var addPosToTwoWheelPerms = (_.curry(appendTwoWheelPerms))(twoWheelPermsLocal);
            // AS CLOSURE
            function addPosToTwoWheelPerms2(thrPos) {
                return self.appendTwoWheelPerms(twoWheelPermsLocal, thrPos);
            }
            return _.flatten(thrLoop.map(addPosToTwoWheelPerms2));
        };
        Calcs1.prototype.sumColumn = function (_a) {
            var a = _a[0], b = _a[1], c = _a[2];
            // convert strings to numbers, then add
            return +(a) + +(b) + +(c);
        };
        Calcs1.prototype.columnsFromPermutation = function (perm) {
            var firstPos = R.head(perm);
            var secPos = R.head(R.drop(1, perm));
            var thrPos = R.head(R.drop(2, perm));
            var getSpecificPos = R.compose(R.head, R.drop);
            return _.zip(firstPos, secPos, thrPos);
        };
        Calcs1.prototype.answersPlusPerm = function (first, secLoop, thrLoop) {
            var self = this;
            function sumPlusPerm(perm) {
                var cols = self.columnsFromPermutation(perm);
                return [[cols.map(self.sumColumn), perm]];
            }
            var perms3 = this.threeLoopPerms(first, secLoop, thrLoop);
            var ansPlus = perms3.map(sumPlusPerm);
            return _.flatten(ansPlus);
        };
        Calcs1.prototype.findSpecificAnswer = function (first, secLoop, thrLoop, answersLoop) {
            var candidates = this.answersPlusPerm(first, secLoop, thrLoop);
            function chkForAnswer(_a) {
                var ans = _a[0], lists = _a[1];
                // this code has no side effects, such as changing a var in a closure
                var results = answersLoop.filter(function (val) { return _.isEqual(ans, val); });
                return results.length > 0;
            }
            return candidates.filter(chkForAnswer);
        };
        return Calcs1;
    })();
    WheelCalcs.Calcs1 = Calcs1;
})(WheelCalcs = exports.WheelCalcs || (exports.WheelCalcs = {}));
//# sourceMappingURL=wheelCalcs.js.map