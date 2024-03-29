// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Monad$CoreCategory = require("@enetsee/core-category/src/Monad.bs.js");

function Make(F) {
  var effect = function (x) {
    return /* Eff */Block.__(1, [x]);
  };
  var run = function (t, pure, eff) {
    if (t.tag) {
      return Curry._1(eff, Curry._2(F.map, t[0], (function (param) {
                        return run(param, pure, eff);
                      })));
    } else {
      return Curry._1(pure, t[0]);
    }
  };
  var liftF = function (e) {
    return /* Eff */Block.__(1, [Curry._2(F.map, e, (function (x) {
                      return /* Pure */Block.__(0, [x]);
                    }))]);
  };
  var map_ = function (t, f) {
    if (t.tag) {
      return /* Eff */Block.__(1, [Curry._2(F.map, t[0], (function (param) {
                        return map_(param, f);
                      }))]);
    } else {
      return /* Pure */Block.__(0, [Curry._1(f, t[0])]);
    }
  };
  var map = /* `Custom */[
    -198771759,
    map_
  ];
  var pure = function (x) {
    return /* Pure */Block.__(0, [x]);
  };
  var bind = function (t, f) {
    if (t.tag) {
      return /* Eff */Block.__(1, [Curry._2(F.map, t[0], (function (param) {
                        return bind(param, f);
                      }))]);
    } else {
      return Curry._1(f, t[0]);
    }
  };
  var apply_ = function (t, f) {
    if (f.tag) {
      return /* Eff */Block.__(1, [Curry._2(F.map, f[0], (function (f) {
                        return apply_(t, f);
                      }))]);
    } else {
      var g = f[0];
      if (t.tag) {
        return /* Eff */Block.__(1, [Curry._2(F.map, t[0], (function (param) {
                          return map_(param, g);
                        }))]);
      } else {
        return /* Pure */Block.__(0, [Curry._1(g, t[0])]);
      }
    }
  };
  var apply = /* `Custom */[
    -198771759,
    apply_
  ];
  var include = Monad$CoreCategory.MakeCustom1({
        pure: pure,
        bind: bind,
        map: map,
        replace: /* Derived */-684824643,
        apply: apply,
        liftA2: /* Derived */-684824643,
        applyFirst: /* Derived */-684824643,
        applySecond: /* Derived */-684824643,
        select: /* Derived */-684824643,
        join: /* Derived */-684824643
      });
  return {
          map: include.map,
          replace: include.replace,
          $$void: include.$$void,
          FunctorInfix: include.FunctorInfix,
          $less$$great: include.$less$$great,
          $less$amp$great: include.$less$amp$great,
          $less$: include.$less$,
          $$great: include.$$great,
          apply: include.apply,
          liftA2: include.liftA2,
          applyFirst: include.applyFirst,
          applySecond: include.applySecond,
          ApplyInfix: include.ApplyInfix,
          $less$star$great: include.$less$star$great,
          $star$great: include.$star$great,
          $less$star: include.$less$star,
          $star$star: include.$star$star,
          liftA3: include.liftA3,
          liftA4: include.liftA4,
          liftA5: include.liftA5,
          merge: include.merge,
          pure: include.pure,
          when_: include.when_,
          unless: include.unless,
          select: include.select,
          SelectiveInfix: include.SelectiveInfix,
          $less$star$question: include.$less$star$question,
          $less$pipe$pipe$great: include.$less$pipe$pipe$great,
          $less$amp$amp$great: include.$less$amp$amp$great,
          orS: include.orS,
          andS: include.andS,
          whenS: include.whenS,
          branch: include.branch,
          ifS: include.ifS,
          fromOptionS: include.fromOptionS,
          anyS: include.anyS,
          allS: include.allS,
          whileS: include.whileS,
          bind: include.bind,
          MonadInfix: include.MonadInfix,
          $great$great$eq: include.$great$great$eq,
          $great$great$tilde: include.$great$great$tilde,
          $great$eq$great: include.$great$eq$great,
          join: include.join,
          forever: include.forever,
          sequenceM: include.sequenceM,
          mapM: include.mapM,
          mapM_: include.mapM_,
          liftF: liftF,
          effect: effect,
          run: run
        };
}

function MakeChurch(F) {
  var effect = function (x) {
    return {
            run: (function (kp, kf) {
                return Curry._1(kf, Curry._2(F.map, x, (function (param) {
                                  return Curry._2(param.run, kp, kf);
                                })));
              })
          };
  };
  var pure = function (x) {
    return {
            run: (function (kp, param) {
                return Curry._1(kp, x);
              })
          };
  };
  var run = function (param, pure, eff) {
    return Curry._2(param.run, pure, eff);
  };
  var liftF = function (e) {
    return effect(Curry._2(F.map, e, pure));
  };
  var bind = function (param, f) {
    var run = param.run;
    return {
            run: (function (kp, kf) {
                return Curry._2(run, (function (x) {
                              return Curry._2(Curry._1(f, x).run, kp, kf);
                            }), kf);
              })
          };
  };
  var map_ = function (param, f) {
    var run = param.run;
    return {
            run: (function (kp, kf) {
                return Curry._2(run, (function (x) {
                              return Curry._1(kp, Curry._1(f, x));
                            }), kf);
              })
          };
  };
  var map = /* `Custom */[
    -198771759,
    map_
  ];
  var apply_ = function (t, f) {
    return {
            run: (function (kp, kf) {
                return Curry._2(f.run, (function (f) {
                              return Curry._2(t.run, (function (x) {
                                            return Curry._1(kp, Curry._1(f, x));
                                          }), kf);
                            }), kf);
              })
          };
  };
  var apply = /* `Custom */[
    -198771759,
    apply_
  ];
  var join_ = function (mmx) {
    return {
            run: (function (kp, kf) {
                return Curry._2(mmx.run, (function (mx) {
                              return Curry._2(mx.run, kp, kf);
                            }), kf);
              })
          };
  };
  var join = /* `Custom */[
    -198771759,
    join_
  ];
  var include = Monad$CoreCategory.MakeCustom1({
        pure: pure,
        bind: bind,
        map: map,
        replace: /* Derived */-684824643,
        apply: apply,
        liftA2: /* Derived */-684824643,
        applyFirst: /* Derived */-684824643,
        applySecond: /* Derived */-684824643,
        select: /* Derived */-684824643,
        join: join
      });
  return {
          map: include.map,
          replace: include.replace,
          $$void: include.$$void,
          FunctorInfix: include.FunctorInfix,
          $less$$great: include.$less$$great,
          $less$amp$great: include.$less$amp$great,
          $less$: include.$less$,
          $$great: include.$$great,
          apply: include.apply,
          liftA2: include.liftA2,
          applyFirst: include.applyFirst,
          applySecond: include.applySecond,
          ApplyInfix: include.ApplyInfix,
          $less$star$great: include.$less$star$great,
          $star$great: include.$star$great,
          $less$star: include.$less$star,
          $star$star: include.$star$star,
          liftA3: include.liftA3,
          liftA4: include.liftA4,
          liftA5: include.liftA5,
          merge: include.merge,
          pure: include.pure,
          when_: include.when_,
          unless: include.unless,
          select: include.select,
          SelectiveInfix: include.SelectiveInfix,
          $less$star$question: include.$less$star$question,
          $less$pipe$pipe$great: include.$less$pipe$pipe$great,
          $less$amp$amp$great: include.$less$amp$amp$great,
          orS: include.orS,
          andS: include.andS,
          whenS: include.whenS,
          branch: include.branch,
          ifS: include.ifS,
          fromOptionS: include.fromOptionS,
          anyS: include.anyS,
          allS: include.allS,
          whileS: include.whileS,
          bind: include.bind,
          MonadInfix: include.MonadInfix,
          $great$great$eq: include.$great$great$eq,
          $great$great$tilde: include.$great$great$tilde,
          $great$eq$great: include.$great$eq$great,
          join: include.join,
          forever: include.forever,
          sequenceM: include.sequenceM,
          mapM: include.mapM,
          mapM_: include.mapM_,
          liftF: liftF,
          effect: effect,
          run: run
        };
}

exports.Make = Make;
exports.MakeChurch = MakeChurch;
/* Monad-CoreCategory Not a pure module */
