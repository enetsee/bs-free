// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Fn$CoreCategory = require("@enetsee/core-category/src/Fn.bs.js");
var Either$CoreCategory = require("@enetsee/core-category/src/Either.bs.js");
var Selective$CoreCategory = require("@enetsee/core-category/src/Selective.bs.js");

function Make(F) {
  var map_ = function (x, f) {
    if (x.tag) {
      return /* Select */Block.__(1, [
                map_(x[0], (function (param) {
                        return Curry._2(Either$CoreCategory.mapSecond, param, f);
                      })),
                Curry._2(F.map, x[1], (function (param) {
                        return Curry._2(Fn$CoreCategory.map, param, f);
                      }))
              ]);
    } else {
      return /* Pure */Block.__(0, [Curry._1(f, x[0])]);
    }
  };
  var map = /* `Custom */[
    -198771759,
    map_
  ];
  var select = function (x, f) {
    if (f.tag) {
      var f$1 = function (x) {
        return Curry._2(Either$CoreCategory.mapSecond, x, Either$CoreCategory.second);
      };
      var g = function (y, a) {
        return Curry._3(Either$CoreCategory.bimap, y, (function (b) {
                      return /* tuple */[
                              b,
                              a
                            ];
                    }), (function (param) {
                      return Curry._1(param, a);
                    }));
      };
      var h = function (z) {
        return Curry._1(Fn$CoreCategory.uncurry, z);
      };
      return /* Select */Block.__(1, [
                select(map_(x, f$1), map_(f[0], g)),
                Curry._2(F.map, f[1], h)
              ]);
    } else {
      var y = f[0];
      return map_(x, (function (param) {
                    return Either$CoreCategory.either(y, Fn$CoreCategory.id, param);
                  }));
    }
  };
  var pure = function (x) {
    return /* Pure */Block.__(0, [x]);
  };
  var apply = function (x, f) {
    return select(map_(f, Either$CoreCategory.first), map_(x, (function (prim, prim$1) {
                      return Curry._1(prim$1, prim);
                    })));
  };
  return Selective$CoreCategory.MakeCustom1({
              pure: pure,
              apply: apply,
              map: map,
              replace: /* Derived */-684824643,
              liftA2: /* Derived */-684824643,
              applyFirst: /* Derived */-684824643,
              applySecond: /* Derived */-684824643,
              select: select
            });
}

exports.Make = Make;
/* Fn-CoreCategory Not a pure module */