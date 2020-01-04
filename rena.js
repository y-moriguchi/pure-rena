/*
 * This source code is under the Unlicense
 */
function Rena() {
    var me;

    me = {
        range: function(beginChar, endChar) {
            var begin = beginChar.charCodeAt(0),
                end = endChar ? endChar.charCodeAt(0) : begin;

            return function(match, index, attr) {
                var ch;

                if(index >= match.length) {
                    return null;
                } else {
                    ch = match.charCodeAt(index);
                    if(ch >= begin && ch <= end) {
                        return {
                            match: match.charAt(index),
                            lastIndex: index + 1,
                            attr: attr
                        };
                    } else {
                        return null;
                    }
                }
            };
        },

        epsilon: function() {
            return function() {
                return {
                    match: "",
                    lastIndex: index,
                    attr: attr
                };
            };
        },

        concat: function (skipSpace) {
            return function(/* args */) {
                var args = Array.prototype.slice.call(arguments);

                return function(match, index, attr) {
                    var indexNew = index,
                        attrNew = attr,
                        result,
                        i;

                    for(i = 0; i < args.length; i++) {
                        result = args[i](match, indexNew, attrNew);
                        if(result) {
                            indexNew = result.lastIndex;
                            attrNew = result.attr;
                        } else {
                            return null;
                        }
                    }
                    return {
                        match: match.substring(index, indexNew),
                        lastIndex: indexNew,
                        attr: attrNew
                    };
                };
            };
        },

        choice: function(/* args */) {
            var args = Array.prototype.slice.call(arguments);

            return function(match, index, attr) {
                var result, i;

                for(i = 0; i < args.length; i++) {
                    result = args[i](match, index, attr);
                    if(result) {
                        return result;
                    }
                }
                return null;
            };
        },

        action: function(exp, action) {
            return function(match, index, attr) {
                var result = exp(match, index, attr);

                if(result) {
                    return {
                        match: result.match,
                        lastIndex: result.lastIndex,
                        attr: action(result.match, result.attr, attr)
                    };
                } else {
                    return null;
                }
            };
        },

        lookaheadNot: function(exp) {
            return function(match, index, attr) {
                var result = exp(match, index, attr);

                if(result) {
                    return null;
                } else {
                    return {
                        match: "",
                        lastIndex: index,
                        attr: attr
                    };
                }
            };
        },

        letrec: function(/* args */) {
            var l = Array.prototype.slice.call(arguments),
                i,
                res;

            res = (function(g) {
                return g(g);
            })(function(p) {
                var i,
                    li,
                    res = [];
                for(i = 0; i < l.length; i++) {
                    (function (li) {
                        res.push(function(str, index, captures) {
                            return (li.apply(null, p(p)))(str, index, captures);
                        });
                    })(l[i]);
                }
                return res;
            });
            return res[0];
        },

        zeroOrMore: function(exp) {
            return me.letrec(function(y) {
                return me.choice(me.concat(exp, y), "");
            });
        },

        oneOrMore: function(exp) {
            return me.concat(exp, me.zeroOrMore(exp));
        },

        opt: function(exp) {
            return me.choice(exp, "");
        },

        lookahead: function(exp) {
            return me.lookaheadNot(me.lookaheadNot(exp));
        }
    };
    return me;
}

