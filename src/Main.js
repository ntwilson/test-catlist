
exports.arrayFromFoldableImpl = function (foldl) {
  var foldingFn = function(state) { return function(element) { state.push(element); return state; } }
  return foldl(foldingFn);
}
