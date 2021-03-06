// Generated by psc-bundle 0.9.1
var PS = {};
(function(exports) {
    "use strict";

  // module Control.Monad.Eff.Console

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
    "use strict";

  exports.showNumberImpl = function (n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Show"];     
  var Show = function (show) {
      this.show = show;
  };                                                 
  var showNumber = new Show($foreign.showNumberImpl);
  var show = function (dict) {
      return dict.show;
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["showNumber"] = showNumber;
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  exports["log"] = $foreign.log;
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var Control_Category = PS["Control.Category"];
  var apply = function (f) {
      return function (x) {
          return f(x);
      };
  };
  exports["apply"] = apply;
})(PS["Data.Function"] = PS["Data.Function"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];
  var Data_Semiring = PS["Data.Semiring"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Function = PS["Data.Function"];
  var Data_Show = PS["Data.Show"];        
  var fac = (function () {
      var go = function (__copy_acc) {
          return function (__copy_v) {
              var acc = __copy_acc;
              var v = __copy_v;
              tco: while (true) {
                  if (v === 1.0) {
                      return acc;
                  };
                  var __tco_acc = acc * v;
                  var __tco_v = v - 1.0;
                  acc = __tco_acc;
                  v = __tco_v;
                  continue tco;
              };
          };
      };
      return go(1.0);
  })();
  var main = Data_Function.apply(Control_Monad_Eff_Console.log)(Data_Function.apply(Data_Show.show(Data_Show.showNumber))(fac(3.0)));
  exports["fac"] = fac;
  exports["main"] = main;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();
