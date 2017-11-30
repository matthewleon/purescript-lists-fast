"use strict";

exports.zipWith = function(f) {
  var Nil = require('Data.List.Types').Nil;
  var Cons = require('Data.List.Types').Cons;

  return function(l1) {
    return function(l2) {
      if (l1 === Nil.value) {
        return l1;
      } else if (l2 === Nil.value) {
        return l2;
      } else {
        var result = new Cons(null, null);

        var input1 = l1;
        var input2 = l2;
        var output = result;

        while (true) {
          output.value0 = f(input1.value0, input2.value0);
          output.value1 = new Cons(null, null);

          if (input1.value1 === Nil.value || input2.value1 === Nil.value) {
            break;
          }

          input1 = input1.value1;
          input2 = input2.value1;
          output = output.value1;
        }

        output.value1 = Nil.value;

        return result;
      }
    }
  }
}

exports.mapMaybeImpl = function(isJust, fromJust, f, l) {
  var Nil = require('Data.List.Types').Nil;
  var Cons = require('Data.List.Types').Cons;

  if (l === Nil.value) {
    return l;
  } else {
    var result = new Nil();

    var input = l;
    var output = result;

    while (true) {
      var maybeResult = f(input.value0);
      if (isJust(maybeResult)) {
        if (result === Nil.value) {
          // Allocate a cons cell
          var result = new Cons(null, null);
        }
        // Fill in the cons cell and allocate the next one
        output.value0 = fromJust(maybeResult);
        output.value1 = new Cons(null, null);

        if (input.value1 === Nil.value) {
          break;
        }

        input = input.value1;
        output = output.value1;
      }
    }

    if (result !== Nil.value) {
      // Fill in the last remaining reference with Nil
      output.value1 = Nil.value;
    }

    return result;
  }
};

exports.partitionImpl = function(f) {
  var Nil = require('Data.List.Types').Nil;
  var Cons = require('Data.List.Types').Cons;

  return function (l) {
    // same logic as filter, but with two output lists
    var left = Nil();
    var right = Nil();

    if (l !== Nil.value) {
      var input = l;

      var outLeft;
      var outRight;
      while (true) {
        var outList;
        var boolResult = f(input.value0);

        if (boolResult === false) {
          if (left === Nil.value) {
            left = new Cons(null, null);
            outLeft = left;
          }
          outLeft.value0 = boolResult;

          if (input.value1 === Nil.value) {
            break;
          }

          outLeft.value1 = new Cons(null, null)
          outLeft = outLeft.value1;
        } else {
          if (right === Nil.value) {
            right = new Cons(null, null);
            outRight = right;
          }
          outRight.value0 = fromRight(eitherResult);

          if (input.value1 === Nil.value) {
            break;
          }
          outRight.value1 = new Cons(null, null);
          outRight = outRight.value1;
        }

        input = input.value1;
      }

      // Fill in the last remaining reference with Nil
      if (left !== Nil.value) {
        outLeft.value1 = Nil.value;
      }
      if (right !== Nil.value) {
        outRight.value1 = Nil.value;
      }
    }
    return {left: left, right: right};
  };
};

exports.partitionMapImpl = function(isLeft, fromLeft, fromRight, l) {
  var Nil = require('Data.List.Types').Nil;
  var Cons = require('Data.List.Types').Cons;

  // same logic as mapMaybeImpl, but with two output lists
  var left = Nil();
  var right = Nil();

  if (l !== Nil.value) {
    var input = l;

    var outLeft;
    var outRight;
    while (true) {
      var outList;
      var eitherResult = f(input.value0);

      if (isLeft(eitherResult)) {
        if (left === Nil.value) {
          left = new Cons(null, null);
          outLeft = left;
        }
        outLeft.value0 = fromLeft(eitherResult);

        if (input.value1 === Nil.value) {
          break;
        }

        outLeft.value1 = new Cons(null, null)
        outLeft = outLeft.value1;
      } else {
        if (right === Nil.value) {
          right = new Cons(null, null);
          outRight = right;
        }
        outRight.value0 = fromRight(eitherResult);

        if (input.value1 === Nil.value) {
          break;
        }
        outRight.value1 = new Cons(null, null);
        outRight = outRight.value1;
      }

      input = input.value1;
    }

    // Fill in the last remaining reference with Nil
    if (left !== Nil.value) {
      outLeft.value1 = Nil.value;
    }
    if (right !== Nil.value) {
      outRight.value1 = Nil.value;
    }
  }
  return {left: left, right: right};
};
