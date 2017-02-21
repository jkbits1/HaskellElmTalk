/**
 * Created by Jon on 28/02/16.
 */

"use strict";

export module WheelCalcs {

  declare module _ {
    interface RIf {
      drop<T>(n:number, array:T[]): T[];
      take<T>(n:number, array:T[]): T[];
      head<T>(array:List<T>): T;
      //compose<V0, V1, T1>(fn0: (x0: V0, x1: V1) => T1): (x0: V0, x1: V1) => T1;
      //compose<V0, T1, T2>(fn1: (x: T1) => T2, fn0: (x0: V0) => T1): (x0: V0) => T2;
    }

    interface List<T> {
      [index: number]: T;
      length: number;
    }

    interface lodashIf {
      curry<T1, T2, R>(func: (t1: T1, t2: T2) => R):
        CurriedFunction2<T1, T2, R>;
      flatten<T>(array: List<T|T[]>): T[];
      head<T>(array: List<T>): T;
      zip<T>(...arrays: List<T>[]): T[][];
      isEqual(value: any, other: any): boolean;
    }

    interface CurriedFunction1<T1, R> {
      (): CurriedFunction1<T1, R>;
      (t1: T1): R;
    }

    interface CurriedFunction2<T1, T2, R> {
      (): CurriedFunction2<T1, T2, R>;
      (t1: T1): CurriedFunction1<T2, R>;
      (t1: T1, t2: T2): R;
    }
  }

  declare var R: _.RIf;
  declare var _: _.lodashIf;

  interface Column extends Array<number> { 0: number, 1: number, 2: number}

  export type WheelPos         = Array<number>;
  export type WheelLoop        = Array<WheelPos>;
  export type LoopsPermutation = Array<WheelPos>;
  export type LoopsPermColumn  = Column;
  export type LoopsPermAnswers = Array<number>;

  export class Calcs1 {

    constructor() {}

    turnWheel(wheel: WheelPos, turns: number): WheelPos {
      var dropPart = R.drop(turns, wheel);
      var takePart = R.take(turns, wheel);

      return dropPart.concat(takePart);
    }

    getWheelLoop (positions: Array<WheelPos>, pos:WheelPos, count:number): WheelLoop {
      if (count === 0) {
        return [pos].concat(positions);
      }

      return this.getWheelLoop([this.turnWheel(pos, count)].concat(positions), pos, count - 1);
    }

    createWheelLoop (initialPos: WheelPos): WheelLoop {
      return this.getWheelLoop([], initialPos, initialPos.length - 1);
    }

    twoWheelPerms (first: WheelPos, secLoop:WheelLoop): Array<LoopsPermutation> {
      return secLoop.map(secPos => [first].concat([secPos]));
    }

    appendTwoWheelPerms (twoWheelPermsLocal: Array<LoopsPermutation>, thrPos: WheelPos) :Array<LoopsPermutation> {
      return twoWheelPermsLocal.map(twoLoopsPerm => twoLoopsPerm.concat([thrPos]));
    }

    threeLoopPerms (first: WheelPos, secLoop: WheelLoop, thrLoop: WheelLoop): Array<LoopsPermutation> {
      var self = this;

      var twoWheelPermsLocal = self.twoWheelPerms(first, secLoop);

      // AS CURRIED FUNCTION
      // var addPosToTwoWheelPerms = (_.curry(appendTwoWheelPerms))(twoWheelPermsLocal);

      // AS CLOSURE
      function addPosToTwoWheelPerms2 (thrPos: WheelPos): Array<LoopsPermutation> {
        return self.appendTwoWheelPerms(twoWheelPermsLocal, thrPos);
      }

      return _.flatten(thrLoop.map(addPosToTwoWheelPerms2));
    }

    sumColumn ([a, b, c]: LoopsPermColumn): number {
      // convert strings to numbers, then add
      return +(a) + +(b) + +(c);
    }

    columnsFromPermutation (perm: LoopsPermutation): Array<LoopsPermColumn> {
      var firstPos = R.head(perm);
      var secPos = R.head(R.drop(1, perm));
      var thrPos = R.head(R.drop(2, perm));

      var getSpecificPos = R.compose(R.head, R.drop);

      return _.zip(firstPos, secPos, thrPos);
    }

    answersPlusPerm (first: WheelPos, secLoop: WheelLoop,
                                thrLoop: WheelLoop):
    Array<[LoopsPermAnswers, LoopsPermutation]> {
      var self = this;

      function sumPlusPerm (perm: LoopsPermutation): Array<[LoopsPermAnswers, LoopsPermutation]> {
        var cols:Array<LoopsPermColumn> = self.columnsFromPermutation(perm);

        return [[cols.map(self.sumColumn), perm]];
      }

      var perms3 = this.threeLoopPerms(first, secLoop, thrLoop);

      var ansPlus = perms3.map(sumPlusPerm)

      return _.flatten(ansPlus);
    }

    findSpecificAnswer (first: WheelPos, secLoop: WheelLoop,
                                 thrLoop: WheelLoop, answersLoop: WheelLoop):
  Array<[LoopsPermAnswers, LoopsPermutation]> {
    var candidates:Array<[LoopsPermAnswers, LoopsPermutation]> =
      this.answersPlusPerm(first, secLoop, thrLoop);

    function chkForAnswer ([ans, lists]:[LoopsPermAnswers, LoopsPermutation]): boolean {
      // this code has no side effects, such as changing a var in a closure
      var results:Array<WheelPos> = answersLoop.filter( val => _.isEqual(ans, val) );

      return results.length > 0;
    }

    return candidates.filter(chkForAnswer);
  }
  }
}
