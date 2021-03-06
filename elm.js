(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.ai.S === region.ar.S)
	{
		return 'on line ' + region.ai.S;
	}
	return 'on lines ' + region.ai.S + ' through ' + region.ar.S;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**_UNUSED/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aZ,
		impl.a9,
		impl.a5,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		w: func(record.w),
		ak: record.ak,
		ah: record.ah
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.w;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.ak;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.ah) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aZ,
		impl.a9,
		impl.a5,
		function(sendToApp, initialModel) {
			var view = impl.bc;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aZ,
		impl.a9,
		impl.a5,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.U && impl.U(sendToApp)
			var view = impl.bc;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aR);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.a7) && (_VirtualDom_doc.title = title = doc.a7);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.a0;
	var onUrlRequest = impl.a1;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		U: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.aF === next.aF
							&& curr.au === next.au
							&& curr.aB.a === next.aB.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		aZ: function(flags)
		{
			return A3(impl.aZ, flags, _Browser_getUrl(), key);
		},
		bc: impl.bc,
		a9: impl.a9,
		a5: impl.a5
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { aX: 'hidden', aS: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { aX: 'mozHidden', aS: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { aX: 'msHidden', aS: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { aX: 'webkitHidden', aS: 'webkitvisibilitychange' }
		: { aX: 'hidden', aS: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		aJ: _Browser_getScene(),
		aO: {
			_: _Browser_window.pageXOffset,
			aa: _Browser_window.pageYOffset,
			L: _Browser_doc.documentElement.clientWidth,
			F: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		L: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		F: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			aJ: {
				L: node.scrollWidth,
				F: node.scrollHeight
			},
			aO: {
				_: node.scrollLeft,
				aa: node.scrollTop,
				L: node.clientWidth,
				F: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			aJ: _Browser_getScene(),
			aO: {
				_: x,
				aa: y,
				L: _Browser_doc.documentElement.clientWidth,
				F: _Browser_doc.documentElement.clientHeight
			},
			aU: {
				_: x + rect.left,
				aa: y + rect.top,
				L: rect.width,
				F: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var author$project$Main$Point = F2(
	function (x, y) {
		return {_: x, aa: y};
	});
var author$project$Main$graphWidth = 1000.0;
var author$project$Main$intervalSize = 0.5;
var elm$core$Basics$EQ = 1;
var elm$core$Basics$GT = 2;
var elm$core$Basics$LT = 0;
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Basics$truncate = _Basics_truncate;
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var author$project$Main$functionToPoints = function (graphFunc) {
	var mapList = A2(
		elm$core$List$map,
		function (n) {
			return n;
		},
		A2(elm$core$List$range, 0, (author$project$Main$graphWidth / author$project$Main$intervalSize) | 0));
	var mapFunction = function (n) {
		return A2(
			author$project$Main$Point,
			n * author$project$Main$intervalSize,
			graphFunc(n * author$project$Main$intervalSize));
	};
	return A2(elm$core$List$map, mapFunction, mapList);
};
var author$project$Main$graphHeight = 1000.0;
var elm$core$Maybe$Nothing = {$: 1};
var mdgriffith$elm_style_animation$Animation$Model$Spring = function (a) {
	return {$: 0, a: a};
};
var mdgriffith$elm_style_animation$Animation$initMotion = F2(
	function (position, unit) {
		return {
			G: mdgriffith$elm_style_animation$Animation$Model$Spring(
				{ap: 26, aK: 170}),
			a_: elm$core$Maybe$Nothing,
			a3: position,
			a6: position,
			a8: unit,
			bb: 0
		};
	});
var mdgriffith$elm_style_animation$Animation$Model$LineTo = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var mdgriffith$elm_style_animation$Animation$lineTo = F2(
	function (x_, y_) {
		return A2(
			mdgriffith$elm_style_animation$Animation$Model$LineTo,
			A2(mdgriffith$elm_style_animation$Animation$initMotion, x_, ''),
			A2(mdgriffith$elm_style_animation$Animation$initMotion, y_, ''));
	});
var mdgriffith$elm_style_animation$Animation$Model$MoveTo = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var mdgriffith$elm_style_animation$Animation$moveTo = F2(
	function (x_, y_) {
		return A2(
			mdgriffith$elm_style_animation$Animation$Model$MoveTo,
			A2(mdgriffith$elm_style_animation$Animation$initMotion, x_, ''),
			A2(mdgriffith$elm_style_animation$Animation$initMotion, y_, ''));
	});
var author$project$Main$pointsToPathCommand = function (points) {
	return A2(
		elm$core$List$cons,
		A2(mdgriffith$elm_style_animation$Animation$moveTo, 0, author$project$Main$graphHeight / 2),
		A2(
			elm$core$List$map,
			function (n) {
				return A2(mdgriffith$elm_style_animation$Animation$lineTo, n._, n.aa);
			},
			points));
};
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$sin = _Basics_sin;
var elm$core$Basics$False = 1;
var elm$core$Basics$True = 0;
var elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.a) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.c),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.c);
		} else {
			var treeLen = builder.a * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.e) : builder.e;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.a);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.c) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.c);
		}
	});
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{e: nodeList, a: (len / elm$core$Array$branchFactor) | 0, c: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 1) {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var mdgriffith$elm_style_animation$Animation$Model$Path = function (a) {
	return {$: 9, a: a};
};
var mdgriffith$elm_style_animation$Animation$path = function (commands) {
	return mdgriffith$elm_style_animation$Animation$Model$Path(commands);
};
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$time$Time$Posix = elm$core$Basics$identity;
var elm$time$Time$millisToPosix = elm$core$Basics$identity;
var mdgriffith$elm_style_animation$Animation$Model$Animation = elm$core$Basics$identity;
var mdgriffith$elm_style_animation$Animation$initialState = function (current) {
	return {
		ae: _List_Nil,
		ab: false,
		aj: _List_Nil,
		l: current,
		aN: {
			ao: elm$time$Time$millisToPosix(0),
			aT: elm$time$Time$millisToPosix(0)
		}
	};
};
var elm$core$Basics$pi = _Basics_pi;
var mdgriffith$elm_style_animation$Animation$Model$AtSpeed = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_style_animation$Animation$speed = function (speedValue) {
	return mdgriffith$elm_style_animation$Animation$Model$AtSpeed(speedValue);
};
var mdgriffith$elm_style_animation$Animation$Model$Easing = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_style_animation$Animation$defaultInterpolationByProperty = function (prop) {
	var linear = function (duration) {
		return mdgriffith$elm_style_animation$Animation$Model$Easing(
			{ac: duration, ad: elm$core$Basics$identity, aE: 1, ai: 0});
	};
	var defaultSpring = mdgriffith$elm_style_animation$Animation$Model$Spring(
		{ap: 26, aK: 170});
	switch (prop.$) {
		case 0:
			return defaultSpring;
		case 1:
			return linear(
				elm$time$Time$millisToPosix(400));
		case 2:
			return defaultSpring;
		case 3:
			return defaultSpring;
		case 4:
			return defaultSpring;
		case 5:
			var name = prop.a;
			return (name === 'rotate3d') ? mdgriffith$elm_style_animation$Animation$speed(
				{az: elm$core$Basics$pi}) : defaultSpring;
		case 6:
			return defaultSpring;
		case 7:
			return mdgriffith$elm_style_animation$Animation$speed(
				{az: elm$core$Basics$pi});
		case 8:
			return defaultSpring;
		default:
			return defaultSpring;
	}
};
var mdgriffith$elm_style_animation$Animation$Model$AngleProperty = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var mdgriffith$elm_style_animation$Animation$Model$ColorProperty = F5(
	function (a, b, c, d, e) {
		return {$: 1, a: a, b: b, c: c, d: d, e: e};
	});
var mdgriffith$elm_style_animation$Animation$Model$ExactProperty = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_style_animation$Animation$Model$Points = function (a) {
	return {$: 8, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$Property = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var mdgriffith$elm_style_animation$Animation$Model$Property2 = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var mdgriffith$elm_style_animation$Animation$Model$Property3 = F4(
	function (a, b, c, d) {
		return {$: 5, a: a, b: b, c: c, d: d};
	});
var mdgriffith$elm_style_animation$Animation$Model$Property4 = F5(
	function (a, b, c, d, e) {
		return {$: 6, a: a, b: b, c: c, d: d, e: e};
	});
var mdgriffith$elm_style_animation$Animation$Model$ShadowProperty = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var mdgriffith$elm_style_animation$Animation$Model$AntiClockwiseArc = function (a) {
	return {$: 17, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$ClockwiseArc = function (a) {
	return {$: 16, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$Close = {$: 18};
var mdgriffith$elm_style_animation$Animation$Model$Curve = function (a) {
	return {$: 8, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$CurveTo = function (a) {
	return {$: 9, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$Horizontal = function (a) {
	return {$: 4, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$HorizontalTo = function (a) {
	return {$: 5, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$Line = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var mdgriffith$elm_style_animation$Animation$Model$Move = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_style_animation$Animation$Model$Quadratic = function (a) {
	return {$: 10, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$QuadraticTo = function (a) {
	return {$: 11, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$Smooth = function (a) {
	return {$: 14, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$SmoothQuadratic = function (a) {
	return {$: 12, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$SmoothQuadraticTo = function (a) {
	return {$: 13, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$SmoothTo = function (a) {
	return {$: 15, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$Vertical = function (a) {
	return {$: 6, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$VerticalTo = function (a) {
	return {$: 7, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$mapPathMotion = F2(
	function (fn, cmd) {
		var mapCoords = function (coords) {
			return A2(
				elm$core$List$map,
				function (_n1) {
					var x = _n1.a;
					var y = _n1.b;
					return _Utils_Tuple2(
						fn(x),
						fn(y));
				},
				coords);
		};
		switch (cmd.$) {
			case 0:
				var m1 = cmd.a;
				var m2 = cmd.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$Move,
					fn(m1),
					fn(m2));
			case 1:
				var m1 = cmd.a;
				var m2 = cmd.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$MoveTo,
					fn(m1),
					fn(m2));
			case 2:
				var m1 = cmd.a;
				var m2 = cmd.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$Line,
					fn(m1),
					fn(m2));
			case 3:
				var m1 = cmd.a;
				var m2 = cmd.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$LineTo,
					fn(m1),
					fn(m2));
			case 4:
				var motion = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$Horizontal(
					fn(motion));
			case 5:
				var motion = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$HorizontalTo(
					fn(motion));
			case 6:
				var motion = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$Vertical(
					fn(motion));
			case 7:
				var motion = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$VerticalTo(
					fn(motion));
			case 8:
				var control1 = cmd.a.O;
				var control2 = cmd.a.P;
				var point = cmd.a.q;
				return mdgriffith$elm_style_animation$Animation$Model$Curve(
					{
						O: _Utils_Tuple2(
							fn(control1.a),
							fn(control1.b)),
						P: _Utils_Tuple2(
							fn(control2.a),
							fn(control2.b)),
						q: _Utils_Tuple2(
							fn(point.a),
							fn(point.b))
					});
			case 9:
				var control1 = cmd.a.O;
				var control2 = cmd.a.P;
				var point = cmd.a.q;
				return mdgriffith$elm_style_animation$Animation$Model$CurveTo(
					{
						O: _Utils_Tuple2(
							fn(control1.a),
							fn(control1.b)),
						P: _Utils_Tuple2(
							fn(control2.a),
							fn(control2.b)),
						q: _Utils_Tuple2(
							fn(point.a),
							fn(point.b))
					});
			case 10:
				var control = cmd.a.N;
				var point = cmd.a.q;
				return mdgriffith$elm_style_animation$Animation$Model$Quadratic(
					{
						N: _Utils_Tuple2(
							fn(control.a),
							fn(control.b)),
						q: _Utils_Tuple2(
							fn(point.a),
							fn(point.b))
					});
			case 11:
				var control = cmd.a.N;
				var point = cmd.a.q;
				return mdgriffith$elm_style_animation$Animation$Model$QuadraticTo(
					{
						N: _Utils_Tuple2(
							fn(control.a),
							fn(control.b)),
						q: _Utils_Tuple2(
							fn(point.a),
							fn(point.b))
					});
			case 12:
				var coords = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$SmoothQuadratic(
					mapCoords(coords));
			case 13:
				var coords = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$SmoothQuadraticTo(
					mapCoords(coords));
			case 14:
				var coords = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$Smooth(
					mapCoords(coords));
			case 15:
				var coords = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$SmoothTo(
					mapCoords(coords));
			case 16:
				var arc = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$ClockwiseArc(
					function () {
						var y = arc.aa;
						var x = arc._;
						var startAngle = arc.V;
						var radius = arc.T;
						var endAngle = arc.Q;
						return _Utils_update(
							arc,
							{
								Q: fn(endAngle),
								T: fn(radius),
								V: fn(startAngle),
								_: fn(x),
								aa: fn(y)
							});
					}());
			case 17:
				var arc = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$AntiClockwiseArc(
					function () {
						var y = arc.aa;
						var x = arc._;
						var startAngle = arc.V;
						var radius = arc.T;
						var endAngle = arc.Q;
						return _Utils_update(
							arc,
							{
								Q: fn(endAngle),
								T: fn(radius),
								V: fn(startAngle),
								_: fn(x),
								aa: fn(y)
							});
					}());
			default:
				return mdgriffith$elm_style_animation$Animation$Model$Close;
		}
	});
var mdgriffith$elm_style_animation$Animation$Model$mapToMotion = F2(
	function (fn, prop) {
		switch (prop.$) {
			case 0:
				var name = prop.a;
				var value = prop.b;
				return A2(mdgriffith$elm_style_animation$Animation$Model$ExactProperty, name, value);
			case 1:
				var name = prop.a;
				var m1 = prop.b;
				var m2 = prop.c;
				var m3 = prop.d;
				var m4 = prop.e;
				return A5(
					mdgriffith$elm_style_animation$Animation$Model$ColorProperty,
					name,
					fn(m1),
					fn(m2),
					fn(m3),
					fn(m4));
			case 2:
				var name = prop.a;
				var inset = prop.b;
				var shadow = prop.c;
				var size = shadow.r;
				var red = shadow.k;
				var offsetY = shadow.o;
				var offsetX = shadow.n;
				var green = shadow.j;
				var blur = shadow.m;
				var blue = shadow.h;
				var alpha = shadow.g;
				return A3(
					mdgriffith$elm_style_animation$Animation$Model$ShadowProperty,
					name,
					inset,
					{
						g: fn(alpha),
						h: fn(blue),
						m: fn(blur),
						j: fn(green),
						n: fn(offsetX),
						o: fn(offsetY),
						k: fn(red),
						r: fn(size)
					});
			case 3:
				var name = prop.a;
				var m1 = prop.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$Property,
					name,
					fn(m1));
			case 4:
				var name = prop.a;
				var m1 = prop.b;
				var m2 = prop.c;
				return A3(
					mdgriffith$elm_style_animation$Animation$Model$Property2,
					name,
					fn(m1),
					fn(m2));
			case 5:
				var name = prop.a;
				var m1 = prop.b;
				var m2 = prop.c;
				var m3 = prop.d;
				return A4(
					mdgriffith$elm_style_animation$Animation$Model$Property3,
					name,
					fn(m1),
					fn(m2),
					fn(m3));
			case 6:
				var name = prop.a;
				var m1 = prop.b;
				var m2 = prop.c;
				var m3 = prop.d;
				var m4 = prop.e;
				return A5(
					mdgriffith$elm_style_animation$Animation$Model$Property4,
					name,
					fn(m1),
					fn(m2),
					fn(m3),
					fn(m4));
			case 7:
				var name = prop.a;
				var m1 = prop.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$AngleProperty,
					name,
					fn(m1));
			case 8:
				var ms = prop.a;
				return mdgriffith$elm_style_animation$Animation$Model$Points(
					A2(
						elm$core$List$map,
						function (_n1) {
							var x = _n1.a;
							var y = _n1.b;
							return _Utils_Tuple2(
								fn(x),
								fn(y));
						},
						ms));
			default:
				var cmds = prop.a;
				return mdgriffith$elm_style_animation$Animation$Model$Path(
					A2(
						elm$core$List$map,
						mdgriffith$elm_style_animation$Animation$Model$mapPathMotion(fn),
						cmds));
		}
	});
var mdgriffith$elm_style_animation$Animation$setDefaultInterpolation = function (prop) {
	var interp = mdgriffith$elm_style_animation$Animation$defaultInterpolationByProperty(prop);
	return A2(
		mdgriffith$elm_style_animation$Animation$Model$mapToMotion,
		function (m) {
			return _Utils_update(
				m,
				{G: interp});
		},
		prop);
};
var elm$core$Basics$not = _Basics_not;
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$List$sortBy = _List_sortBy;
var elm$core$List$sort = function (xs) {
	return A2(elm$core$List$sortBy, elm$core$Basics$identity, xs);
};
var mdgriffith$elm_style_animation$Animation$Model$propertyName = function (prop) {
	switch (prop.$) {
		case 0:
			var name = prop.a;
			return name;
		case 1:
			var name = prop.a;
			return name;
		case 2:
			var name = prop.a;
			return name;
		case 3:
			var name = prop.a;
			return name;
		case 4:
			var name = prop.a;
			return name;
		case 5:
			var name = prop.a;
			return name;
		case 6:
			var name = prop.a;
			return name;
		case 7:
			var name = prop.a;
			return name;
		case 8:
			return 'points';
		default:
			return 'path';
	}
};
var mdgriffith$elm_style_animation$Animation$Render$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			if (!list.b) {
				return _List_Nil;
			} else {
				var x = list.a;
				var xs = list.b;
				if (predicate(x)) {
					var $temp$predicate = predicate,
						$temp$list = xs;
					predicate = $temp$predicate;
					list = $temp$list;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var mdgriffith$elm_style_animation$Animation$Render$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				if (!list.b) {
					return elm$core$List$reverse(memo);
				} else {
					var x = list.a;
					var xs = list.b;
					if (predicate(x)) {
						var $temp$memo = A2(elm$core$List$cons, x, memo),
							$temp$list = xs;
						memo = $temp$memo;
						list = $temp$list;
						continue takeWhileMemo;
					} else {
						return elm$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(_List_Nil);
};
var mdgriffith$elm_style_animation$Animation$Render$span = F2(
	function (p, xs) {
		return _Utils_Tuple2(
			A2(mdgriffith$elm_style_animation$Animation$Render$takeWhile, p, xs),
			A2(mdgriffith$elm_style_animation$Animation$Render$dropWhile, p, xs));
	});
var mdgriffith$elm_style_animation$Animation$Render$groupWhile = F2(
	function (eq, xs_) {
		if (!xs_.b) {
			return _List_Nil;
		} else {
			var x = xs_.a;
			var xs = xs_.b;
			var _n1 = A2(
				mdgriffith$elm_style_animation$Animation$Render$span,
				eq(x),
				xs);
			var ys = _n1.a;
			var zs = _n1.b;
			return A2(
				elm$core$List$cons,
				A2(elm$core$List$cons, x, ys),
				A2(mdgriffith$elm_style_animation$Animation$Render$groupWhile, eq, zs));
		}
	});
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var mdgriffith$elm_style_animation$Animation$Render$isTransformation = function (prop) {
	return A2(
		elm$core$List$member,
		mdgriffith$elm_style_animation$Animation$Model$propertyName(prop),
		_List_fromArray(
			['rotate', 'rotateX', 'rotateY', 'rotateZ', 'rotate3d', 'translate', 'translate3d', 'scale', 'scale3d']));
};
var mdgriffith$elm_style_animation$Animation$Render$warnForDoubleListedProperties = function (props) {
	var _n0 = A2(
		elm$core$List$map,
		function (propGroup) {
			var _n1 = elm$core$List$head(propGroup);
			if (_n1.$ === 1) {
				return '';
			} else {
				var name = _n1.a;
				return (elm$core$List$length(propGroup) > 1) ? '' : '';
			}
		},
		A2(
			mdgriffith$elm_style_animation$Animation$Render$groupWhile,
			elm$core$Basics$eq,
			elm$core$List$sort(
				A2(
					elm$core$List$map,
					mdgriffith$elm_style_animation$Animation$Model$propertyName,
					A2(
						elm$core$List$filter,
						function (prop) {
							return !mdgriffith$elm_style_animation$Animation$Render$isTransformation(prop);
						},
						props)))));
	return props;
};
var mdgriffith$elm_style_animation$Animation$style = function (props) {
	return mdgriffith$elm_style_animation$Animation$initialState(
		A2(
			elm$core$List$map,
			mdgriffith$elm_style_animation$Animation$setDefaultInterpolation,
			mdgriffith$elm_style_animation$Animation$Render$warnForDoubleListedProperties(props)));
};
var author$project$Main$init = function (_n0) {
	var model = {
		R: elm$core$Basics$sin,
		v: 1.0,
		y: 100.0,
		l: mdgriffith$elm_style_animation$Animation$style(
			_List_fromArray(
				[
					mdgriffith$elm_style_animation$Animation$path(
					author$project$Main$pointsToPathCommand(
						author$project$Main$functionToPoints(
							function (n) {
								return author$project$Main$graphHeight / 2;
							})))
				]))
	};
	return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
};
var author$project$Main$Animate = function (a) {
	return {$: 3, a: a};
};
var elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 0, a: a};
};
var elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {ag: oldTime, aI: request, aL: subs};
	});
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$browser$Browser$AnimationManager$init = elm$core$Task$succeed(
	A3(elm$browser$Browser$AnimationManager$State, _List_Nil, elm$core$Maybe$Nothing, 0));
var elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var elm$browser$Browser$Dom$NotFound = elm$core$Basics$identity;
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$Perform = elm$core$Basics$identity;
var elm$core$Task$init = elm$core$Task$succeed(0);
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return 0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0;
		return A2(elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			A2(elm$core$Task$map, toMessage, task));
	});
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = 0;
var elm$url$Url$Https = 1;
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {at: fragment, au: host, ay: path, aB: port_, aF: protocol, aG: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 1) {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		0,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		1,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$AnimationManager$now = _Browser_now(0);
var elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(0);
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Process$kill = _Scheduler_kill;
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _n0) {
		var request = _n0.aI;
		var oldTime = _n0.ag;
		var _n1 = _Utils_Tuple2(request, subs);
		if (_n1.a.$ === 1) {
			if (!_n1.b.b) {
				var _n2 = _n1.a;
				return elm$browser$Browser$AnimationManager$init;
			} else {
				var _n4 = _n1.a;
				return A2(
					elm$core$Task$andThen,
					function (pid) {
						return A2(
							elm$core$Task$andThen,
							function (time) {
								return elm$core$Task$succeed(
									A3(
										elm$browser$Browser$AnimationManager$State,
										subs,
										elm$core$Maybe$Just(pid),
										time));
							},
							elm$browser$Browser$AnimationManager$now);
					},
					elm$core$Process$spawn(
						A2(
							elm$core$Task$andThen,
							elm$core$Platform$sendToSelf(router),
							elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_n1.b.b) {
				var pid = _n1.a.a;
				return A2(
					elm$core$Task$andThen,
					function (_n3) {
						return elm$browser$Browser$AnimationManager$init;
					},
					elm$core$Process$kill(pid));
			} else {
				return elm$core$Task$succeed(
					A3(elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _n0) {
		var subs = _n0.aL;
		var oldTime = _n0.ag;
		var send = function (sub) {
			if (!sub.$) {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(
						elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			elm$core$Task$andThen,
			function (pid) {
				return A2(
					elm$core$Task$andThen,
					function (_n1) {
						return elm$core$Task$succeed(
							A3(
								elm$browser$Browser$AnimationManager$State,
								subs,
								elm$core$Maybe$Just(pid),
								newTime));
					},
					elm$core$Task$sequence(
						A2(elm$core$List$map, send, subs)));
			},
			elm$core$Process$spawn(
				A2(
					elm$core$Task$andThen,
					elm$core$Platform$sendToSelf(router),
					elm$browser$Browser$AnimationManager$rAF)));
	});
var elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 1, a: a};
};
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (!sub.$) {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Time(
				A2(elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Delta(
				A2(elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager(elm$browser$Browser$AnimationManager$init, elm$browser$Browser$AnimationManager$onEffects, elm$browser$Browser$AnimationManager$onSelfMsg, 0, elm$browser$Browser$AnimationManager$subMap);
var elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return elm$browser$Browser$AnimationManager$subscription(
		elm$browser$Browser$AnimationManager$Time(tagger));
};
var elm$browser$Browser$Events$onAnimationFrame = elm$browser$Browser$AnimationManager$onAnimationFrame;
var elm$core$Platform$Sub$map = _Platform_map;
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var mdgriffith$elm_style_animation$Animation$isRunning = function (_n0) {
	var model = _n0;
	return model.ab;
};
var mdgriffith$elm_style_animation$Animation$Model$Tick = elm$core$Basics$identity;
var mdgriffith$elm_style_animation$Animation$subscription = F2(
	function (msg, states) {
		return A2(elm$core$List$any, mdgriffith$elm_style_animation$Animation$isRunning, states) ? A2(
			elm$core$Platform$Sub$map,
			msg,
			elm$browser$Browser$Events$onAnimationFrame(elm$core$Basics$identity)) : elm$core$Platform$Sub$none;
	});
var author$project$Main$subscriptions = function (model) {
	return A2(
		mdgriffith$elm_style_animation$Animation$subscription,
		author$project$Main$Animate,
		_List_fromArray(
			[model.l]));
};
var elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var elm$core$Basics$degrees = function (angleInDegrees) {
	return (angleInDegrees * elm$core$Basics$pi) / 180;
};
var elm$core$Basics$negate = function (n) {
	return -n;
};
var author$project$Main$addScalarsToFunction = function (model) {
	return function (n) {
		return A3(
			elm$core$Basics$clamp,
			-10,
			author$project$Main$graphHeight + 10,
			(model.y * model.R(
				elm$core$Basics$degrees(model.v) * n)) + (author$project$Main$graphHeight / 2));
	};
};
var elm$core$Basics$cos = _Basics_cos;
var elm$core$Basics$tan = _Basics_tan;
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$core$String$toFloat = _String_toFloat;
var elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var elm$time$Time$posixToMillis = function (_n0) {
	var millis = _n0;
	return millis;
};
var mdgriffith$elm_style_animation$Animation$extractInitialWait = function (steps) {
	var _n0 = elm$core$List$head(steps);
	if (_n0.$ === 1) {
		return _Utils_Tuple2(
			elm$time$Time$millisToPosix(0),
			_List_Nil);
	} else {
		var step = _n0.a;
		if (step.$ === 4) {
			var till = step.a;
			var _n2 = mdgriffith$elm_style_animation$Animation$extractInitialWait(
				A2(elm$core$List$drop, 1, steps));
			var additionalTime = _n2.a;
			var remainingSteps = _n2.b;
			return _Utils_Tuple2(
				elm$time$Time$millisToPosix(
					elm$time$Time$posixToMillis(till) + elm$time$Time$posixToMillis(additionalTime)),
				remainingSteps);
		} else {
			return _Utils_Tuple2(
				elm$time$Time$millisToPosix(0),
				steps);
		}
	}
};
var mdgriffith$elm_style_animation$Animation$interrupt = F2(
	function (steps, _n0) {
		var model = _n0;
		return _Utils_update(
			model,
			{
				ae: A2(
					elm$core$List$cons,
					mdgriffith$elm_style_animation$Animation$extractInitialWait(steps),
					model.ae),
				ab: true
			});
	});
var mdgriffith$elm_style_animation$Animation$Model$To = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_style_animation$Animation$to = function (props) {
	return mdgriffith$elm_style_animation$Animation$Model$To(props);
};
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _n0) {
				var trues = _n0.a;
				var falses = _n0.b;
				return pred(x) ? _Utils_Tuple2(
					A2(elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2(elm$core$List$cons, x, falses));
			});
		return A3(
			elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var elm$core$Basics$round = _Basics_round;
var mdgriffith$elm_style_animation$Animation$Model$refreshTiming = F2(
	function (now, timing) {
		var dt = elm$time$Time$posixToMillis(now) - elm$time$Time$posixToMillis(timing.ao);
		return {
			ao: now,
			aT: ((dt > 34) || (!elm$time$Time$posixToMillis(timing.ao))) ? elm$time$Time$millisToPosix(
				elm$core$Basics$round(16.666)) : elm$time$Time$millisToPosix(dt)
		};
	});
var elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			elm$core$List$any,
			A2(elm$core$Basics$composeL, elm$core$Basics$not, isOkay),
			list);
	});
var mdgriffith$elm_style_animation$Animation$Model$Loop = function (a) {
	return {$: 7, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$Repeat = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var mdgriffith$elm_style_animation$Animation$Model$Step = {$: 0};
var mdgriffith$elm_style_animation$Animation$Model$Wait = function (a) {
	return {$: 4, a: a};
};
var mdgriffith$elm_style_animation$Animation$Model$isCmdDone = function (cmd) {
	var motionDone = function (motion) {
		return (!motion.bb) && _Utils_eq(motion.a3, motion.a6);
	};
	switch (cmd.$) {
		case 0:
			var m1 = cmd.a;
			var m2 = cmd.b;
			return motionDone(m1) && motionDone(m2);
		case 1:
			var m1 = cmd.a;
			var m2 = cmd.b;
			return motionDone(m1) && motionDone(m2);
		case 2:
			var m1 = cmd.a;
			var m2 = cmd.b;
			return motionDone(m1) && motionDone(m2);
		case 3:
			var m1 = cmd.a;
			var m2 = cmd.b;
			return motionDone(m1) && motionDone(m2);
		case 4:
			var motion = cmd.a;
			return motionDone(motion);
		case 5:
			var motion = cmd.a;
			return motionDone(motion);
		case 6:
			var motion = cmd.a;
			return motionDone(motion);
		case 7:
			var motion = cmd.a;
			return motionDone(motion);
		case 8:
			var control1 = cmd.a.O;
			var control2 = cmd.a.P;
			var point = cmd.a.q;
			return motionDone(control1.a) && (motionDone(control1.b) && (motionDone(control2.a) && (motionDone(control2.b) && (motionDone(point.a) && motionDone(point.b)))));
		case 9:
			var control1 = cmd.a.O;
			var control2 = cmd.a.P;
			var point = cmd.a.q;
			return motionDone(control1.a) && (motionDone(control1.b) && (motionDone(control2.a) && (motionDone(control2.b) && (motionDone(point.a) && motionDone(point.b)))));
		case 10:
			var control = cmd.a.N;
			var point = cmd.a.q;
			return motionDone(control.a) && (motionDone(control.b) && (motionDone(point.a) && motionDone(point.b)));
		case 11:
			var control = cmd.a.N;
			var point = cmd.a.q;
			return motionDone(control.a) && (motionDone(control.b) && (motionDone(point.a) && motionDone(point.b)));
		case 12:
			var coords = cmd.a;
			return A2(
				elm$core$List$all,
				function (_n1) {
					var x = _n1.a;
					var y = _n1.b;
					return motionDone(x) && motionDone(y);
				},
				coords);
		case 13:
			var coords = cmd.a;
			return A2(
				elm$core$List$all,
				function (_n2) {
					var x = _n2.a;
					var y = _n2.b;
					return motionDone(x) && motionDone(y);
				},
				coords);
		case 14:
			var coords = cmd.a;
			return A2(
				elm$core$List$all,
				function (_n3) {
					var x = _n3.a;
					var y = _n3.b;
					return motionDone(x) && motionDone(y);
				},
				coords);
		case 15:
			var coords = cmd.a;
			return A2(
				elm$core$List$all,
				function (_n4) {
					var x = _n4.a;
					var y = _n4.b;
					return motionDone(x) && motionDone(y);
				},
				coords);
		case 16:
			var arc = cmd.a;
			return motionDone(arc._) && (motionDone(arc.aa) && (motionDone(arc.T) && (motionDone(arc.V) && motionDone(arc.Q))));
		case 17:
			var arc = cmd.a;
			return motionDone(arc._) && (motionDone(arc.aa) && (motionDone(arc.T) && (motionDone(arc.V) && motionDone(arc.Q))));
		default:
			return true;
	}
};
var mdgriffith$elm_style_animation$Animation$Model$isDone = function (property) {
	var motionDone = function (motion) {
		var runningInterpolation = A2(elm$core$Maybe$withDefault, motion.G, motion.a_);
		switch (runningInterpolation.$) {
			case 0:
				return (!motion.bb) && _Utils_eq(motion.a3, motion.a6);
			case 1:
				var eased = runningInterpolation.a;
				return (eased.aE === 1) || ((!eased.aE) && _Utils_eq(motion.a3, motion.a6));
			default:
				var speed = runningInterpolation.a;
				return _Utils_eq(motion.a3, motion.a6);
		}
	};
	switch (property.$) {
		case 0:
			return true;
		case 1:
			var m1 = property.b;
			var m2 = property.c;
			var m3 = property.d;
			var m4 = property.e;
			return A2(
				elm$core$List$all,
				motionDone,
				_List_fromArray(
					[m1, m2, m3, m4]));
		case 2:
			var shadow = property.c;
			return A2(
				elm$core$List$all,
				motionDone,
				_List_fromArray(
					[shadow.n, shadow.o, shadow.r, shadow.m, shadow.k, shadow.j, shadow.h, shadow.g]));
		case 3:
			var m1 = property.b;
			return motionDone(m1);
		case 4:
			var m1 = property.b;
			var m2 = property.c;
			return motionDone(m1) && motionDone(m2);
		case 5:
			var m1 = property.b;
			var m2 = property.c;
			var m3 = property.d;
			return A2(
				elm$core$List$all,
				motionDone,
				_List_fromArray(
					[m1, m2, m3]));
		case 6:
			var m1 = property.b;
			var m2 = property.c;
			var m3 = property.d;
			var m4 = property.e;
			return A2(
				elm$core$List$all,
				motionDone,
				_List_fromArray(
					[m1, m2, m3, m4]));
		case 7:
			var m1 = property.b;
			return motionDone(m1);
		case 8:
			var ms = property.a;
			return A2(
				elm$core$List$all,
				function (_n1) {
					var x = _n1.a;
					var y = _n1.b;
					return motionDone(x) && motionDone(y);
				},
				ms);
		default:
			var cmds = property.a;
			return A2(elm$core$List$all, mdgriffith$elm_style_animation$Animation$Model$isCmdDone, cmds);
	}
};
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (!_n0.$) {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2(elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var elm$core$List$repeat = F2(
	function (n, value) {
		return A3(elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var mdgriffith$elm_style_animation$Animation$Model$matchPoints = F2(
	function (points1, points2) {
		var diff = elm$core$List$length(points1) - elm$core$List$length(points2);
		if (diff > 0) {
			var _n0 = elm$core$List$head(
				elm$core$List$reverse(points2));
			if (_n0.$ === 1) {
				return _Utils_Tuple2(points1, points2);
			} else {
				var last2 = _n0.a;
				return _Utils_Tuple2(
					points1,
					_Utils_ap(
						points2,
						A2(
							elm$core$List$repeat,
							elm$core$Basics$abs(diff),
							last2)));
			}
		} else {
			if (diff < 0) {
				var _n1 = elm$core$List$head(
					elm$core$List$reverse(points1));
				if (_n1.$ === 1) {
					return _Utils_Tuple2(points1, points2);
				} else {
					var last1 = _n1.a;
					return _Utils_Tuple2(
						_Utils_ap(
							points1,
							A2(
								elm$core$List$repeat,
								elm$core$Basics$abs(diff),
								last1)),
						points2);
				}
			} else {
				return _Utils_Tuple2(points1, points2);
			}
		}
	});
var mdgriffith$elm_style_animation$Animation$Model$setPathTarget = F2(
	function (cmd, targetCmd) {
		var setMotionTarget = F2(
			function (motion, targetMotion) {
				var _n27 = motion.G;
				if (_n27.$ === 1) {
					var ease = _n27.a;
					return _Utils_update(
						motion,
						{
							G: mdgriffith$elm_style_animation$Animation$Model$Easing(
								_Utils_update(
									ease,
									{ai: motion.a3})),
							a6: targetMotion.a3
						});
				} else {
					return _Utils_update(
						motion,
						{a6: targetMotion.a3});
				}
			});
		switch (cmd.$) {
			case 0:
				var m1 = cmd.a;
				var m2 = cmd.b;
				if (!targetCmd.$) {
					var t1 = targetCmd.a;
					var t2 = targetCmd.b;
					return A2(
						mdgriffith$elm_style_animation$Animation$Model$Move,
						A2(setMotionTarget, m1, t1),
						A2(setMotionTarget, m2, t2));
				} else {
					return cmd;
				}
			case 1:
				var m1 = cmd.a;
				var m2 = cmd.b;
				if (targetCmd.$ === 1) {
					var t1 = targetCmd.a;
					var t2 = targetCmd.b;
					return A2(
						mdgriffith$elm_style_animation$Animation$Model$MoveTo,
						A2(setMotionTarget, m1, t1),
						A2(setMotionTarget, m2, t2));
				} else {
					return cmd;
				}
			case 2:
				var m1 = cmd.a;
				var m2 = cmd.b;
				if (targetCmd.$ === 2) {
					var t1 = targetCmd.a;
					var t2 = targetCmd.b;
					return A2(
						mdgriffith$elm_style_animation$Animation$Model$Line,
						A2(setMotionTarget, m1, t1),
						A2(setMotionTarget, m2, t2));
				} else {
					return cmd;
				}
			case 3:
				var m1 = cmd.a;
				var m2 = cmd.b;
				if (targetCmd.$ === 3) {
					var t1 = targetCmd.a;
					var t2 = targetCmd.b;
					return A2(
						mdgriffith$elm_style_animation$Animation$Model$LineTo,
						A2(setMotionTarget, m1, t1),
						A2(setMotionTarget, m2, t2));
				} else {
					return cmd;
				}
			case 4:
				var m1 = cmd.a;
				if (targetCmd.$ === 4) {
					var t1 = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$Horizontal(
						A2(setMotionTarget, m1, t1));
				} else {
					return cmd;
				}
			case 5:
				var m1 = cmd.a;
				if (targetCmd.$ === 5) {
					var t1 = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$HorizontalTo(
						A2(setMotionTarget, m1, t1));
				} else {
					return cmd;
				}
			case 6:
				var m1 = cmd.a;
				if (targetCmd.$ === 6) {
					var t1 = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$Vertical(
						A2(setMotionTarget, m1, t1));
				} else {
					return cmd;
				}
			case 7:
				var m1 = cmd.a;
				if (targetCmd.$ === 7) {
					var t1 = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$VerticalTo(
						A2(setMotionTarget, m1, t1));
				} else {
					return cmd;
				}
			case 8:
				var points = cmd.a;
				if (targetCmd.$ === 8) {
					var targets = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$Curve(
						{
							O: _Utils_Tuple2(
								A2(setMotionTarget, points.O.a, targets.O.a),
								A2(setMotionTarget, points.O.b, targets.O.b)),
							P: _Utils_Tuple2(
								A2(setMotionTarget, points.P.a, targets.P.a),
								A2(setMotionTarget, points.P.b, targets.P.b)),
							q: _Utils_Tuple2(
								A2(setMotionTarget, points.q.a, targets.q.a),
								A2(setMotionTarget, points.q.b, targets.q.b))
						});
				} else {
					return cmd;
				}
			case 9:
				var points = cmd.a;
				if (targetCmd.$ === 9) {
					var targets = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$CurveTo(
						{
							O: _Utils_Tuple2(
								A2(setMotionTarget, points.O.a, targets.O.a),
								A2(setMotionTarget, points.O.b, targets.O.b)),
							P: _Utils_Tuple2(
								A2(setMotionTarget, points.P.a, targets.P.a),
								A2(setMotionTarget, points.P.b, targets.P.b)),
							q: _Utils_Tuple2(
								A2(setMotionTarget, points.q.a, targets.q.a),
								A2(setMotionTarget, points.q.b, targets.q.b))
						});
				} else {
					return cmd;
				}
			case 10:
				var points = cmd.a;
				if (targetCmd.$ === 10) {
					var targets = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$Quadratic(
						{
							N: _Utils_Tuple2(
								A2(setMotionTarget, points.N.a, targets.N.a),
								A2(setMotionTarget, points.N.b, targets.N.b)),
							q: _Utils_Tuple2(
								A2(setMotionTarget, points.q.a, targets.q.a),
								A2(setMotionTarget, points.q.b, targets.q.b))
						});
				} else {
					return cmd;
				}
			case 11:
				var points = cmd.a;
				if (targetCmd.$ === 11) {
					var targets = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$QuadraticTo(
						{
							N: _Utils_Tuple2(
								A2(setMotionTarget, points.N.a, targets.N.a),
								A2(setMotionTarget, points.N.b, targets.N.b)),
							q: _Utils_Tuple2(
								A2(setMotionTarget, points.q.a, targets.q.a),
								A2(setMotionTarget, points.q.b, targets.q.b))
						});
				} else {
					return cmd;
				}
			case 12:
				var coords = cmd.a;
				if (targetCmd.$ === 12) {
					var targetCoords = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$SmoothQuadratic(
						A3(
							elm$core$List$map2,
							F2(
								function (_n14, _n15) {
									var x1 = _n14.a;
									var y1 = _n14.b;
									var x2 = _n15.a;
									var y2 = _n15.b;
									return _Utils_Tuple2(
										A2(setMotionTarget, x1, x2),
										A2(setMotionTarget, y1, y2));
								}),
							coords,
							targetCoords));
				} else {
					return cmd;
				}
			case 13:
				var coords = cmd.a;
				if (targetCmd.$ === 13) {
					var targetCoords = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$SmoothQuadraticTo(
						A3(
							elm$core$List$map2,
							F2(
								function (_n17, _n18) {
									var x1 = _n17.a;
									var y1 = _n17.b;
									var x2 = _n18.a;
									var y2 = _n18.b;
									return _Utils_Tuple2(
										A2(setMotionTarget, x1, x2),
										A2(setMotionTarget, y1, y2));
								}),
							coords,
							targetCoords));
				} else {
					return cmd;
				}
			case 14:
				var coords = cmd.a;
				if (targetCmd.$ === 14) {
					var targetCoords = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$Smooth(
						A3(
							elm$core$List$map2,
							F2(
								function (_n20, _n21) {
									var x1 = _n20.a;
									var y1 = _n20.b;
									var x2 = _n21.a;
									var y2 = _n21.b;
									return _Utils_Tuple2(
										A2(setMotionTarget, x1, x2),
										A2(setMotionTarget, y1, y2));
								}),
							coords,
							targetCoords));
				} else {
					return cmd;
				}
			case 15:
				var coords = cmd.a;
				if (targetCmd.$ === 15) {
					var targetCoords = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$SmoothTo(
						A3(
							elm$core$List$map2,
							F2(
								function (_n23, _n24) {
									var x1 = _n23.a;
									var y1 = _n23.b;
									var x2 = _n24.a;
									var y2 = _n24.b;
									return _Utils_Tuple2(
										A2(setMotionTarget, x1, x2),
										A2(setMotionTarget, y1, y2));
								}),
							coords,
							targetCoords));
				} else {
					return cmd;
				}
			case 16:
				var arc = cmd.a;
				if (targetCmd.$ === 16) {
					var target = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$ClockwiseArc(
						function () {
							var y = arc.aa;
							var x = arc._;
							var startAngle = arc.V;
							var radius = arc.T;
							var endAngle = arc.Q;
							return _Utils_update(
								arc,
								{
									Q: A2(setMotionTarget, endAngle, target.Q),
									T: A2(setMotionTarget, radius, target.T),
									V: A2(setMotionTarget, startAngle, target.V),
									_: A2(setMotionTarget, x, target._),
									aa: A2(setMotionTarget, y, target.aa)
								});
						}());
				} else {
					return cmd;
				}
			case 17:
				var arc = cmd.a;
				if (targetCmd.$ === 17) {
					var target = targetCmd.a;
					return mdgriffith$elm_style_animation$Animation$Model$AntiClockwiseArc(
						function () {
							var y = arc.aa;
							var x = arc._;
							var startAngle = arc.V;
							var radius = arc.T;
							var endAngle = arc.Q;
							return _Utils_update(
								arc,
								{
									Q: A2(setMotionTarget, endAngle, target.Q),
									T: A2(setMotionTarget, radius, target.T),
									V: A2(setMotionTarget, startAngle, target.V),
									_: A2(setMotionTarget, x, target._),
									aa: A2(setMotionTarget, y, target.aa)
								});
						}());
				} else {
					return cmd;
				}
			default:
				return mdgriffith$elm_style_animation$Animation$Model$Close;
		}
	});
var mdgriffith$elm_style_animation$Animation$Model$setTarget = F3(
	function (overrideInterpolation, current, newTarget) {
		var setMotionTarget = F2(
			function (motion, targetMotion) {
				var newMotion = overrideInterpolation ? _Utils_update(
					motion,
					{
						a_: elm$core$Maybe$Just(targetMotion.G)
					}) : motion;
				var _n13 = newMotion.a_;
				if (_n13.$ === 1) {
					var _n14 = newMotion.G;
					if (_n14.$ === 1) {
						var ease = _n14.a;
						return _Utils_update(
							newMotion,
							{
								G: mdgriffith$elm_style_animation$Animation$Model$Easing(
									_Utils_update(
										ease,
										{aE: 0, ai: motion.a3})),
								a6: targetMotion.a3
							});
					} else {
						return _Utils_update(
							newMotion,
							{a6: targetMotion.a3});
					}
				} else {
					var override = _n13.a;
					if (override.$ === 1) {
						var ease = override.a;
						return _Utils_update(
							newMotion,
							{
								a_: elm$core$Maybe$Just(
									mdgriffith$elm_style_animation$Animation$Model$Easing(
										_Utils_update(
											ease,
											{aE: 0, ai: motion.a3}))),
								a6: targetMotion.a3
							});
					} else {
						return _Utils_update(
							newMotion,
							{a6: targetMotion.a3});
					}
				}
			});
		switch (current.$) {
			case 0:
				var name = current.a;
				var value = current.b;
				return A2(mdgriffith$elm_style_animation$Animation$Model$ExactProperty, name, value);
			case 1:
				var name = current.a;
				var m1 = current.b;
				var m2 = current.c;
				var m3 = current.d;
				var m4 = current.e;
				if (newTarget.$ === 1) {
					var t1 = newTarget.b;
					var t2 = newTarget.c;
					var t3 = newTarget.d;
					var t4 = newTarget.e;
					return A5(
						mdgriffith$elm_style_animation$Animation$Model$ColorProperty,
						name,
						A2(setMotionTarget, m1, t1),
						A2(setMotionTarget, m2, t2),
						A2(setMotionTarget, m3, t3),
						A2(setMotionTarget, m4, t4));
				} else {
					return current;
				}
			case 2:
				var name = current.a;
				var inset = current.b;
				var shadow = current.c;
				if (newTarget.$ === 2) {
					var targetShadow = newTarget.c;
					return A3(
						mdgriffith$elm_style_animation$Animation$Model$ShadowProperty,
						name,
						inset,
						{
							g: A2(setMotionTarget, shadow.g, targetShadow.g),
							h: A2(setMotionTarget, shadow.h, targetShadow.h),
							m: A2(setMotionTarget, shadow.m, targetShadow.m),
							j: A2(setMotionTarget, shadow.j, targetShadow.j),
							n: A2(setMotionTarget, shadow.n, targetShadow.n),
							o: A2(setMotionTarget, shadow.o, targetShadow.o),
							k: A2(setMotionTarget, shadow.k, targetShadow.k),
							r: A2(setMotionTarget, shadow.r, targetShadow.r)
						});
				} else {
					return current;
				}
			case 3:
				var name = current.a;
				var m1 = current.b;
				if (newTarget.$ === 3) {
					var t1 = newTarget.b;
					return A2(
						mdgriffith$elm_style_animation$Animation$Model$Property,
						name,
						A2(setMotionTarget, m1, t1));
				} else {
					return current;
				}
			case 4:
				var name = current.a;
				var m1 = current.b;
				var m2 = current.c;
				if (newTarget.$ === 4) {
					var t1 = newTarget.b;
					var t2 = newTarget.c;
					return A3(
						mdgriffith$elm_style_animation$Animation$Model$Property2,
						name,
						A2(setMotionTarget, m1, t1),
						A2(setMotionTarget, m2, t2));
				} else {
					return current;
				}
			case 5:
				var name = current.a;
				var m1 = current.b;
				var m2 = current.c;
				var m3 = current.d;
				if (newTarget.$ === 5) {
					var t1 = newTarget.b;
					var t2 = newTarget.c;
					var t3 = newTarget.d;
					return A4(
						mdgriffith$elm_style_animation$Animation$Model$Property3,
						name,
						A2(setMotionTarget, m1, t1),
						A2(setMotionTarget, m2, t2),
						A2(setMotionTarget, m3, t3));
				} else {
					return current;
				}
			case 6:
				var name = current.a;
				var m1 = current.b;
				var m2 = current.c;
				var m3 = current.d;
				var m4 = current.e;
				if (newTarget.$ === 6) {
					var t1 = newTarget.b;
					var t2 = newTarget.c;
					var t3 = newTarget.d;
					var t4 = newTarget.e;
					return A5(
						mdgriffith$elm_style_animation$Animation$Model$Property4,
						name,
						A2(setMotionTarget, m1, t1),
						A2(setMotionTarget, m2, t2),
						A2(setMotionTarget, m3, t3),
						A2(setMotionTarget, m4, t4));
				} else {
					return current;
				}
			case 7:
				var name = current.a;
				var m1 = current.b;
				if (newTarget.$ === 7) {
					var t1 = newTarget.b;
					return A2(
						mdgriffith$elm_style_animation$Animation$Model$AngleProperty,
						name,
						A2(setMotionTarget, m1, t1));
				} else {
					return current;
				}
			case 8:
				var currentPts = current.a;
				if (newTarget.$ === 8) {
					var targetPts = newTarget.a;
					var _n9 = A2(mdgriffith$elm_style_animation$Animation$Model$matchPoints, currentPts, targetPts);
					var m1s = _n9.a;
					var m2s = _n9.b;
					return mdgriffith$elm_style_animation$Animation$Model$Points(
						A3(
							elm$core$List$map2,
							F2(
								function (_n10, _n11) {
									var x1 = _n10.a;
									var y1 = _n10.b;
									var x2 = _n11.a;
									var y2 = _n11.b;
									return _Utils_Tuple2(
										A2(setMotionTarget, x1, x2),
										A2(setMotionTarget, y1, y2));
								}),
							m1s,
							m2s));
				} else {
					return current;
				}
			default:
				var cmds = current.a;
				if (newTarget.$ === 9) {
					var targets = newTarget.a;
					return mdgriffith$elm_style_animation$Animation$Model$Path(
						A3(elm$core$List$map2, mdgriffith$elm_style_animation$Animation$Model$setPathTarget, cmds, targets));
				} else {
					return current;
				}
		}
	});
var mdgriffith$elm_style_animation$Animation$Model$zipPropertiesGreedy = F2(
	function (initialProps, newTargetProps) {
		var propertyMatch = F2(
			function (prop1, prop2) {
				return _Utils_eq(
					mdgriffith$elm_style_animation$Animation$Model$propertyName(prop1),
					mdgriffith$elm_style_animation$Animation$Model$propertyName(prop2));
			});
		var _n0 = A3(
			elm$core$List$foldl,
			F2(
				function (_n1, _n2) {
					var stackA = _n2.a;
					var stackB = _n2.b;
					var result = _n2.c;
					var _n3 = elm$core$List$head(stackA);
					if (_n3.$ === 1) {
						return _Utils_Tuple3(stackA, stackB, result);
					} else {
						var a = _n3.a;
						var _n4 = A2(
							elm$core$List$partition,
							propertyMatch(a),
							stackB);
						var matchingBs = _n4.a;
						var nonMatchingBs = _n4.b;
						return _Utils_Tuple3(
							A2(elm$core$List$drop, 1, stackA),
							function () {
								if (!matchingBs.b) {
									return nonMatchingBs;
								} else {
									var b = matchingBs.a;
									var remainingBs = matchingBs.b;
									return _Utils_ap(remainingBs, nonMatchingBs);
								}
							}(),
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									a,
									elm$core$List$head(matchingBs)),
								result));
					}
				}),
			_Utils_Tuple3(initialProps, newTargetProps, _List_Nil),
			A2(
				elm$core$List$repeat,
				elm$core$List$length(initialProps),
				0));
		var warnings = _n0.b;
		var props = _n0.c;
		var _n6 = warnings;
		return elm$core$List$reverse(props);
	});
var mdgriffith$elm_style_animation$Animation$Model$startTowards = F3(
	function (overrideInterpolation, current, target) {
		return A2(
			elm$core$List$filterMap,
			function (propPair) {
				if (!propPair.b.$) {
					var cur = propPair.a;
					var to = propPair.b.a;
					return elm$core$Maybe$Just(
						A3(mdgriffith$elm_style_animation$Animation$Model$setTarget, overrideInterpolation, cur, to));
				} else {
					var prop = propPair.a;
					var _n1 = propPair.b;
					return elm$core$Maybe$Just(prop);
				}
			},
			A2(mdgriffith$elm_style_animation$Animation$Model$zipPropertiesGreedy, current, target));
	});
var elm$core$Basics$ge = _Utils_ge;
var mdgriffith$elm_style_animation$Animation$Model$tolerance = 1.0e-2;
var mdgriffith$elm_style_animation$Animation$Model$vTolerance = 0.1;
var mdgriffith$elm_style_animation$Animation$Model$stepInterpolation = F2(
	function (posix, motion) {
		var interpolationToUse = A2(elm$core$Maybe$withDefault, motion.G, motion.a_);
		var dtms = elm$time$Time$posixToMillis(posix);
		switch (interpolationToUse.$) {
			case 2:
				var perSecond = interpolationToUse.a.az;
				var _n1 = function () {
					if (_Utils_cmp(motion.a3, motion.a6) < 0) {
						var _new = motion.a3 + (perSecond * (dtms / 1000));
						return _Utils_Tuple2(
							_new,
							_Utils_cmp(_new, motion.a6) > -1);
					} else {
						var _new = motion.a3 - (perSecond * (dtms / 1000));
						return _Utils_Tuple2(
							_new,
							_Utils_cmp(_new, motion.a6) < 1);
					}
				}();
				var newPos = _n1.a;
				var finished = _n1.b;
				return finished ? _Utils_update(
					motion,
					{a3: motion.a6, bb: 0.0}) : _Utils_update(
					motion,
					{a3: newPos, bb: perSecond * 1000});
			case 0:
				var stiffness = interpolationToUse.a.aK;
				var damping = interpolationToUse.a.ap;
				var fspring = stiffness * (motion.a6 - motion.a3);
				var fdamper = ((-1) * damping) * motion.bb;
				var dt = dtms / 1000;
				var a = fspring + fdamper;
				var newVelocity = motion.bb + (a * dt);
				var newPos = motion.a3 + (newVelocity * dt);
				var dx = elm$core$Basics$abs(motion.a6 - newPos);
				return ((_Utils_cmp(dx, mdgriffith$elm_style_animation$Animation$Model$tolerance) < 0) && (_Utils_cmp(
					elm$core$Basics$abs(newVelocity),
					mdgriffith$elm_style_animation$Animation$Model$vTolerance) < 0)) ? _Utils_update(
					motion,
					{a3: motion.a6, bb: 0.0}) : _Utils_update(
					motion,
					{a3: newPos, bb: newVelocity});
			default:
				var progress = interpolationToUse.a.aE;
				var duration = interpolationToUse.a.ac;
				var ease = interpolationToUse.a.ad;
				var start = interpolationToUse.a.ai;
				var durationMs = elm$time$Time$posixToMillis(duration);
				var newProgress = (((dtms / durationMs) + progress) < 1) ? ((dtms / durationMs) + progress) : 1;
				var eased = ease(newProgress);
				var distance = motion.a6 - start;
				var newPos = ((((eased * distance) + start) * 10000) | 0) / 10000;
				var newVelocity = (newProgress === 1) ? 0 : ((newPos - motion.a3) / dtms);
				var _n2 = motion.a_;
				if (_n2.$ === 1) {
					return _Utils_update(
						motion,
						{
							G: mdgriffith$elm_style_animation$Animation$Model$Easing(
								{ac: duration, ad: ease, aE: newProgress, ai: start}),
							a3: newPos,
							bb: newVelocity
						});
				} else {
					var override = _n2.a;
					return _Utils_update(
						motion,
						{
							a_: elm$core$Maybe$Just(
								mdgriffith$elm_style_animation$Animation$Model$Easing(
									{ac: duration, ad: ease, aE: newProgress, ai: start})),
							a3: newPos,
							bb: newVelocity
						});
				}
		}
	});
var mdgriffith$elm_style_animation$Animation$Model$stepPath = F2(
	function (dt, cmd) {
		var stepCoords = function (coords) {
			return A2(
				elm$core$List$map,
				function (_n1) {
					var x = _n1.a;
					var y = _n1.b;
					return _Utils_Tuple2(
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, x),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, y));
				},
				coords);
		};
		switch (cmd.$) {
			case 0:
				var m1 = cmd.a;
				var m2 = cmd.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$Move,
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, m1),
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, m2));
			case 1:
				var m1 = cmd.a;
				var m2 = cmd.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$MoveTo,
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, m1),
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, m2));
			case 2:
				var m1 = cmd.a;
				var m2 = cmd.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$Line,
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, m1),
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, m2));
			case 3:
				var m1 = cmd.a;
				var m2 = cmd.b;
				return A2(
					mdgriffith$elm_style_animation$Animation$Model$LineTo,
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, m1),
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, m2));
			case 4:
				var motion = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$Horizontal(
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion));
			case 5:
				var motion = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$HorizontalTo(
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion));
			case 6:
				var motion = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$Vertical(
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion));
			case 7:
				var motion = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$VerticalTo(
					A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion));
			case 8:
				var control1 = cmd.a.O;
				var control2 = cmd.a.P;
				var point = cmd.a.q;
				return mdgriffith$elm_style_animation$Animation$Model$Curve(
					{
						O: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control1.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control1.b)),
						P: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control2.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control2.b)),
						q: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, point.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, point.b))
					});
			case 9:
				var control1 = cmd.a.O;
				var control2 = cmd.a.P;
				var point = cmd.a.q;
				return mdgriffith$elm_style_animation$Animation$Model$CurveTo(
					{
						O: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control1.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control1.b)),
						P: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control2.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control2.b)),
						q: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, point.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, point.b))
					});
			case 10:
				var control = cmd.a.N;
				var point = cmd.a.q;
				return mdgriffith$elm_style_animation$Animation$Model$Quadratic(
					{
						N: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control.b)),
						q: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, point.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, point.b))
					});
			case 11:
				var control = cmd.a.N;
				var point = cmd.a.q;
				return mdgriffith$elm_style_animation$Animation$Model$QuadraticTo(
					{
						N: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, control.b)),
						q: _Utils_Tuple2(
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, point.a),
							A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, point.b))
					});
			case 12:
				var coords = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$SmoothQuadratic(
					stepCoords(coords));
			case 13:
				var coords = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$SmoothQuadraticTo(
					stepCoords(coords));
			case 14:
				var coords = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$Smooth(
					stepCoords(coords));
			case 15:
				var coords = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$SmoothTo(
					stepCoords(coords));
			case 16:
				var arc = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$ClockwiseArc(
					_Utils_update(
						arc,
						{
							Q: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc.Q),
							T: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc.T),
							V: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc.V),
							_: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc._),
							aa: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc.aa)
						}));
			case 17:
				var arc = cmd.a;
				return mdgriffith$elm_style_animation$Animation$Model$AntiClockwiseArc(
					_Utils_update(
						arc,
						{
							Q: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc.Q),
							T: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc.T),
							V: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc.V),
							_: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc._),
							aa: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, arc.aa)
						}));
			default:
				return mdgriffith$elm_style_animation$Animation$Model$Close;
		}
	});
var mdgriffith$elm_style_animation$Animation$Model$step = F2(
	function (dt, props) {
		var stepProp = function (property) {
			switch (property.$) {
				case 0:
					var name = property.a;
					var value = property.b;
					return A2(mdgriffith$elm_style_animation$Animation$Model$ExactProperty, name, value);
				case 3:
					var name = property.a;
					var motion = property.b;
					return A2(
						mdgriffith$elm_style_animation$Animation$Model$Property,
						name,
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion));
				case 4:
					var name = property.a;
					var motion1 = property.b;
					var motion2 = property.c;
					return A3(
						mdgriffith$elm_style_animation$Animation$Model$Property2,
						name,
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion1),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion2));
				case 5:
					var name = property.a;
					var motion1 = property.b;
					var motion2 = property.c;
					var motion3 = property.d;
					return A4(
						mdgriffith$elm_style_animation$Animation$Model$Property3,
						name,
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion1),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion2),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion3));
				case 6:
					var name = property.a;
					var motion1 = property.b;
					var motion2 = property.c;
					var motion3 = property.d;
					var motion4 = property.e;
					return A5(
						mdgriffith$elm_style_animation$Animation$Model$Property4,
						name,
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion1),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion2),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion3),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion4));
				case 7:
					var name = property.a;
					var motion = property.b;
					return A2(
						mdgriffith$elm_style_animation$Animation$Model$AngleProperty,
						name,
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, motion));
				case 1:
					var name = property.a;
					var red = property.b;
					var green = property.c;
					var blue = property.d;
					var alpha = property.e;
					return A5(
						mdgriffith$elm_style_animation$Animation$Model$ColorProperty,
						name,
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, red),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, green),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, blue),
						A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, alpha));
				case 2:
					var name = property.a;
					var inset = property.b;
					var shadow = property.c;
					return A3(
						mdgriffith$elm_style_animation$Animation$Model$ShadowProperty,
						name,
						inset,
						{
							g: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, shadow.g),
							h: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, shadow.h),
							m: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, shadow.m),
							j: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, shadow.j),
							n: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, shadow.n),
							o: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, shadow.o),
							k: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, shadow.k),
							r: A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, shadow.r)
						});
				case 8:
					var points = property.a;
					return mdgriffith$elm_style_animation$Animation$Model$Points(
						A2(
							elm$core$List$map,
							function (_n1) {
								var x = _n1.a;
								var y = _n1.b;
								return _Utils_Tuple2(
									A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, x),
									A2(mdgriffith$elm_style_animation$Animation$Model$stepInterpolation, dt, y));
							},
							points));
				default:
					var cmds = property.a;
					return mdgriffith$elm_style_animation$Animation$Model$Path(
						A2(
							elm$core$List$map,
							mdgriffith$elm_style_animation$Animation$Model$stepPath(dt),
							cmds));
			}
		};
		return A2(elm$core$List$map, stepProp, props);
	});
var mdgriffith$elm_style_animation$Animation$Model$alreadyThere = F2(
	function (current, target) {
		return A2(
			elm$core$List$all,
			mdgriffith$elm_style_animation$Animation$Model$isDone,
			A2(
				mdgriffith$elm_style_animation$Animation$Model$step,
				elm$time$Time$millisToPosix(0),
				A3(mdgriffith$elm_style_animation$Animation$Model$startTowards, false, current, target)));
	});
var mdgriffith$elm_style_animation$Animation$Model$replaceProps = F2(
	function (props, replacements) {
		var replacementNames = A2(elm$core$List$map, mdgriffith$elm_style_animation$Animation$Model$propertyName, replacements);
		var removed = A2(
			elm$core$List$filter,
			function (prop) {
				return !A2(
					elm$core$List$member,
					mdgriffith$elm_style_animation$Animation$Model$propertyName(prop),
					replacementNames);
			},
			props);
		return _Utils_ap(removed, replacements);
	});
var mdgriffith$elm_style_animation$Animation$Model$resolveSteps = F3(
	function (currentStyle, steps, dt) {
		resolveSteps:
		while (true) {
			var _n0 = elm$core$List$head(steps);
			if (_n0.$ === 1) {
				return _Utils_Tuple3(currentStyle, _List_Nil, _List_Nil);
			} else {
				var currentStep = _n0.a;
				switch (currentStep.$) {
					case 4:
						var n = currentStep.a;
						if (elm$time$Time$posixToMillis(n) <= 0) {
							var $temp$currentStyle = currentStyle,
								$temp$steps = A2(elm$core$List$drop, 1, steps),
								$temp$dt = dt;
							currentStyle = $temp$currentStyle;
							steps = $temp$steps;
							dt = $temp$dt;
							continue resolveSteps;
						} else {
							return _Utils_Tuple3(
								currentStyle,
								_List_Nil,
								A2(
									elm$core$List$cons,
									mdgriffith$elm_style_animation$Animation$Model$Wait(
										elm$time$Time$millisToPosix(
											elm$time$Time$posixToMillis(n) - elm$time$Time$posixToMillis(dt))),
									A2(elm$core$List$drop, 1, steps)));
						}
					case 5:
						var msg = currentStep.a;
						var _n2 = A3(
							mdgriffith$elm_style_animation$Animation$Model$resolveSteps,
							currentStyle,
							A2(elm$core$List$drop, 1, steps),
							dt);
						var newStyle = _n2.a;
						var msgs = _n2.b;
						var remainingSteps = _n2.c;
						return _Utils_Tuple3(
							newStyle,
							A2(elm$core$List$cons, msg, msgs),
							remainingSteps);
					case 1:
						var target = currentStep.a;
						if (A2(mdgriffith$elm_style_animation$Animation$Model$alreadyThere, currentStyle, target)) {
							return _Utils_Tuple3(
								currentStyle,
								_List_Nil,
								A2(elm$core$List$drop, 1, steps));
						} else {
							var $temp$currentStyle = A3(mdgriffith$elm_style_animation$Animation$Model$startTowards, false, currentStyle, target),
								$temp$steps = A2(
								elm$core$List$cons,
								mdgriffith$elm_style_animation$Animation$Model$Step,
								A2(elm$core$List$drop, 1, steps)),
								$temp$dt = dt;
							currentStyle = $temp$currentStyle;
							steps = $temp$steps;
							dt = $temp$dt;
							continue resolveSteps;
						}
					case 2:
						var target = currentStep.a;
						if (A2(mdgriffith$elm_style_animation$Animation$Model$alreadyThere, currentStyle, target)) {
							return _Utils_Tuple3(
								currentStyle,
								_List_Nil,
								A2(elm$core$List$drop, 1, steps));
						} else {
							var $temp$currentStyle = A3(mdgriffith$elm_style_animation$Animation$Model$startTowards, true, currentStyle, target),
								$temp$steps = A2(
								elm$core$List$cons,
								mdgriffith$elm_style_animation$Animation$Model$Step,
								A2(elm$core$List$drop, 1, steps)),
								$temp$dt = dt;
							currentStyle = $temp$currentStyle;
							steps = $temp$steps;
							dt = $temp$dt;
							continue resolveSteps;
						}
					case 3:
						var props = currentStep.a;
						var $temp$currentStyle = A2(mdgriffith$elm_style_animation$Animation$Model$replaceProps, currentStyle, props),
							$temp$steps = A2(elm$core$List$drop, 1, steps),
							$temp$dt = dt;
						currentStyle = $temp$currentStyle;
						steps = $temp$steps;
						dt = $temp$dt;
						continue resolveSteps;
					case 0:
						var stepped = A2(mdgriffith$elm_style_animation$Animation$Model$step, dt, currentStyle);
						return A2(elm$core$List$all, mdgriffith$elm_style_animation$Animation$Model$isDone, stepped) ? _Utils_Tuple3(
							A2(
								elm$core$List$map,
								mdgriffith$elm_style_animation$Animation$Model$mapToMotion(
									function (m) {
										return _Utils_update(
											m,
											{a_: elm$core$Maybe$Nothing});
									}),
								stepped),
							_List_Nil,
							A2(elm$core$List$drop, 1, steps)) : _Utils_Tuple3(stepped, _List_Nil, steps);
					case 7:
						var substeps = currentStep.a;
						var $temp$currentStyle = currentStyle,
							$temp$steps = _Utils_ap(
							substeps,
							_List_fromArray(
								[
									mdgriffith$elm_style_animation$Animation$Model$Loop(substeps)
								])),
							$temp$dt = dt;
						currentStyle = $temp$currentStyle;
						steps = $temp$steps;
						dt = $temp$dt;
						continue resolveSteps;
					default:
						var n = currentStep.a;
						var substeps = currentStep.b;
						if (n <= 0) {
							var $temp$currentStyle = currentStyle,
								$temp$steps = A2(elm$core$List$drop, 1, steps),
								$temp$dt = dt;
							currentStyle = $temp$currentStyle;
							steps = $temp$steps;
							dt = $temp$dt;
							continue resolveSteps;
						} else {
							var $temp$currentStyle = currentStyle,
								$temp$steps = _Utils_ap(
								substeps,
								_Utils_ap(
									_List_fromArray(
										[
											A2(mdgriffith$elm_style_animation$Animation$Model$Repeat, n - 1, substeps)
										]),
									A2(elm$core$List$drop, 1, steps))),
								$temp$dt = dt;
							currentStyle = $temp$currentStyle;
							steps = $temp$steps;
							dt = $temp$dt;
							continue resolveSteps;
						}
				}
			}
		}
	});
var mdgriffith$elm_style_animation$Animation$Model$updateAnimation = F2(
	function (_n0, _n1) {
		var now = _n0;
		var model = _n1;
		var timing = A2(mdgriffith$elm_style_animation$Animation$Model$refreshTiming, now, model.aN);
		var _n2 = A2(
			elm$core$List$partition,
			function (_n4) {
				var wait = _n4.a;
				var mySteps = _n4.b;
				return elm$time$Time$posixToMillis(wait) <= 0;
			},
			A2(
				elm$core$List$map,
				function (_n3) {
					var wait = _n3.a;
					var mySteps = _n3.b;
					return _Utils_Tuple2(
						elm$time$Time$millisToPosix(
							elm$time$Time$posixToMillis(wait) - elm$time$Time$posixToMillis(timing.aT)),
						mySteps);
				},
				model.ae));
		var readyInterruption = _n2.a;
		var queuedInterruptions = _n2.b;
		var _n5 = function () {
			var _n6 = elm$core$List$head(readyInterruption);
			if (!_n6.$) {
				var _n7 = _n6.a;
				var wait = _n7.a;
				var interrupt = _n7.b;
				return _Utils_Tuple2(
					interrupt,
					A2(
						elm$core$List$map,
						mdgriffith$elm_style_animation$Animation$Model$mapToMotion(
							function (m) {
								return _Utils_update(
									m,
									{a_: elm$core$Maybe$Nothing});
							}),
						model.l));
			} else {
				return _Utils_Tuple2(model.aj, model.l);
			}
		}();
		var steps = _n5.a;
		var style = _n5.b;
		var _n8 = A3(mdgriffith$elm_style_animation$Animation$Model$resolveSteps, style, steps, timing.aT);
		var revisedStyle = _n8.a;
		var sentMessages = _n8.b;
		var revisedSteps = _n8.c;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					ae: queuedInterruptions,
					ab: elm$core$List$length(revisedSteps) || elm$core$List$length(queuedInterruptions),
					aj: revisedSteps,
					l: revisedStyle,
					aN: timing
				}),
			elm$core$Platform$Cmd$batch(
				A2(
					elm$core$List$map,
					function (m) {
						return A2(
							elm$core$Task$perform,
							elm$core$Basics$identity,
							elm$core$Task$succeed(m));
					},
					sentMessages)));
	});
var mdgriffith$elm_style_animation$Animation$update = F2(
	function (tick, animation) {
		return A2(mdgriffith$elm_style_animation$Animation$Model$updateAnimation, tick, animation).a;
	});
var author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var changeData = msg.a;
				var newScalar = A2(
					elm$core$Maybe$withDefault,
					0.0,
					elm$core$String$toFloat(changeData));
				var funcWithNewScalar = author$project$Main$addScalarsToFunction(
					_Utils_update(
						model,
						{v: newScalar}));
				var newStyle = A2(
					mdgriffith$elm_style_animation$Animation$interrupt,
					_List_fromArray(
						[
							mdgriffith$elm_style_animation$Animation$to(
							_List_fromArray(
								[
									mdgriffith$elm_style_animation$Animation$path(
									author$project$Main$pointsToPathCommand(
										author$project$Main$functionToPoints(funcWithNewScalar)))
								]))
						]),
					model.l);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: newScalar, l: newStyle}),
					elm$core$Platform$Cmd$none);
			case 1:
				var changeData = msg.a;
				var newScalar = A2(
					elm$core$Maybe$withDefault,
					0.0,
					elm$core$String$toFloat(changeData));
				var funcWithNewScalar = author$project$Main$addScalarsToFunction(
					_Utils_update(
						model,
						{y: newScalar}));
				var newStyle = A2(
					mdgriffith$elm_style_animation$Animation$interrupt,
					_List_fromArray(
						[
							mdgriffith$elm_style_animation$Animation$to(
							_List_fromArray(
								[
									mdgriffith$elm_style_animation$Animation$path(
									author$project$Main$pointsToPathCommand(
										author$project$Main$functionToPoints(funcWithNewScalar)))
								]))
						]),
					model.l);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{y: newScalar, l: newStyle}),
					elm$core$Platform$Cmd$none);
			case 2:
				var changeData = msg.a;
				var newFunc = (changeData === 'sin') ? elm$core$Basics$sin : ((changeData === 'cos') ? elm$core$Basics$cos : elm$core$Basics$tan);
				var funcWithScalars = author$project$Main$addScalarsToFunction(
					_Utils_update(
						model,
						{R: newFunc}));
				var newStyle = A2(
					mdgriffith$elm_style_animation$Animation$interrupt,
					_List_fromArray(
						[
							mdgriffith$elm_style_animation$Animation$to(
							_List_fromArray(
								[
									mdgriffith$elm_style_animation$Animation$path(
									author$project$Main$pointsToPathCommand(
										author$project$Main$functionToPoints(funcWithScalars)))
								]))
						]),
					model.l);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{R: newFunc, l: newStyle}),
					elm$core$Platform$Cmd$none);
			default:
				var animMsg = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							l: A2(mdgriffith$elm_style_animation$Animation$update, animMsg, model.l)
						}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Main$ChangeFunc = function (a) {
	return {$: 2, a: a};
};
var author$project$Main$ChangeInner = function (a) {
	return {$: 0, a: a};
};
var author$project$Main$ChangeOuter = function (a) {
	return {$: 1, a: a};
};
var elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var author$project$Main$defaultLineAttributes = _List_fromArray(
	[
		elm$svg$Svg$Attributes$stroke('black'),
		elm$svg$Svg$Attributes$strokeWidth('2'),
		elm$svg$Svg$Attributes$fill('none')
	]);
var elm$core$String$fromFloat = _String_fromNumber;
var elm$html$Html$button = _VirtualDom_node('button');
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$input = _VirtualDom_node('input');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$json$Json$Encode$string = _Json_wrap;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$placeholder = elm$html$Html$Attributes$stringProperty('placeholder');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$json$Json$Decode$string = _Json_decodeString;
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$path = elm$svg$Svg$trustedNode('path');
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var mdgriffith$elm_style_animation$Animation$Render$iePrefix = '-ms-';
var mdgriffith$elm_style_animation$Animation$Render$webkitPrefix = '-webkit-';
var mdgriffith$elm_style_animation$Animation$Render$prefix = function (stylePair) {
	var propValue = stylePair.b;
	var propName = stylePair.a;
	switch (propName) {
		case 'transform':
			return _List_fromArray(
				[
					stylePair,
					_Utils_Tuple2(
					_Utils_ap(mdgriffith$elm_style_animation$Animation$Render$iePrefix, propName),
					propValue),
					_Utils_Tuple2(
					_Utils_ap(mdgriffith$elm_style_animation$Animation$Render$webkitPrefix, propName),
					propValue)
				]);
		case 'transform-origin':
			return _List_fromArray(
				[
					stylePair,
					_Utils_Tuple2(
					_Utils_ap(mdgriffith$elm_style_animation$Animation$Render$iePrefix, propName),
					propValue),
					_Utils_Tuple2(
					_Utils_ap(mdgriffith$elm_style_animation$Animation$Render$webkitPrefix, propName),
					propValue)
				]);
		case 'filter':
			return _List_fromArray(
				[
					stylePair,
					_Utils_Tuple2(
					_Utils_ap(mdgriffith$elm_style_animation$Animation$Render$iePrefix, propName),
					propValue),
					_Utils_Tuple2(
					_Utils_ap(mdgriffith$elm_style_animation$Animation$Render$webkitPrefix, propName),
					propValue)
				]);
		default:
			return _List_fromArray(
				[stylePair]);
	}
};
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$attribute = elm$virtual_dom$VirtualDom$attribute;
var elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var elm$svg$Svg$Attributes$offset = _VirtualDom_attribute('offset');
var elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var elm$svg$Svg$Attributes$rx = _VirtualDom_attribute('rx');
var elm$svg$Svg$Attributes$ry = _VirtualDom_attribute('ry');
var elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var mdgriffith$elm_style_animation$Animation$Render$pathCmdValue = function (cmd) {
	var renderPoints = function (coords) {
		return A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$map,
				function (_n11) {
					var x = _n11.a;
					var y = _n11.b;
					return elm$core$String$fromFloat(x.a3) + (',' + elm$core$String$fromFloat(y.a3));
				},
				coords));
	};
	switch (cmd.$) {
		case 0:
			var x = cmd.a;
			var y = cmd.b;
			return 'm ' + (elm$core$String$fromFloat(x.a3) + (',' + elm$core$String$fromFloat(y.a3)));
		case 1:
			var x = cmd.a;
			var y = cmd.b;
			return 'M ' + (elm$core$String$fromFloat(x.a3) + (',' + elm$core$String$fromFloat(y.a3)));
		case 2:
			var x = cmd.a;
			var y = cmd.b;
			return 'l ' + (elm$core$String$fromFloat(x.a3) + (',' + elm$core$String$fromFloat(y.a3)));
		case 3:
			var x = cmd.a;
			var y = cmd.b;
			return 'L ' + (elm$core$String$fromFloat(x.a3) + (',' + elm$core$String$fromFloat(y.a3)));
		case 4:
			var a = cmd.a;
			return 'h ' + elm$core$String$fromFloat(a.a3);
		case 5:
			var a = cmd.a;
			return 'H ' + elm$core$String$fromFloat(a.a3);
		case 6:
			var a = cmd.a;
			return 'v ' + elm$core$String$fromFloat(a.a3);
		case 7:
			var a = cmd.a;
			return 'V ' + elm$core$String$fromFloat(a.a3);
		case 8:
			var control1 = cmd.a.O;
			var control2 = cmd.a.P;
			var point = cmd.a.q;
			var _n1 = point;
			var p1x = _n1.a;
			var p1y = _n1.b;
			var _n2 = control2;
			var c2x = _n2.a;
			var c2y = _n2.b;
			var _n3 = control1;
			var c1x = _n3.a;
			var c1y = _n3.b;
			return 'c ' + (elm$core$String$fromFloat(c1x.a3) + (' ' + (elm$core$String$fromFloat(c1y.a3) + (', ' + (elm$core$String$fromFloat(c2x.a3) + (' ' + (elm$core$String$fromFloat(c2y.a3) + (', ' + (elm$core$String$fromFloat(p1x.a3) + (' ' + elm$core$String$fromFloat(p1y.a3)))))))))));
		case 9:
			var control1 = cmd.a.O;
			var control2 = cmd.a.P;
			var point = cmd.a.q;
			var _n4 = point;
			var p1x = _n4.a;
			var p1y = _n4.b;
			var _n5 = control2;
			var c2x = _n5.a;
			var c2y = _n5.b;
			var _n6 = control1;
			var c1x = _n6.a;
			var c1y = _n6.b;
			return 'C ' + (elm$core$String$fromFloat(c1x.a3) + (' ' + (elm$core$String$fromFloat(c1y.a3) + (', ' + (elm$core$String$fromFloat(c2x.a3) + (' ' + (elm$core$String$fromFloat(c2y.a3) + (', ' + (elm$core$String$fromFloat(p1x.a3) + (' ' + elm$core$String$fromFloat(p1y.a3)))))))))));
		case 10:
			var control = cmd.a.N;
			var point = cmd.a.q;
			var _n7 = point;
			var p1x = _n7.a;
			var p1y = _n7.b;
			var _n8 = control;
			var c1x = _n8.a;
			var c1y = _n8.b;
			return 'q ' + (elm$core$String$fromFloat(c1x.a3) + (' ' + (elm$core$String$fromFloat(c1y.a3) + (', ' + (elm$core$String$fromFloat(p1x.a3) + (' ' + elm$core$String$fromFloat(p1y.a3)))))));
		case 11:
			var control = cmd.a.N;
			var point = cmd.a.q;
			var _n9 = point;
			var p1x = _n9.a;
			var p1y = _n9.b;
			var _n10 = control;
			var c1x = _n10.a;
			var c1y = _n10.b;
			return 'Q ' + (elm$core$String$fromFloat(c1x.a3) + (' ' + (elm$core$String$fromFloat(c1y.a3) + (', ' + (elm$core$String$fromFloat(p1x.a3) + (' ' + elm$core$String$fromFloat(p1y.a3)))))));
		case 12:
			var points = cmd.a;
			return 't ' + renderPoints(points);
		case 13:
			var points = cmd.a;
			return 'T ' + renderPoints(points);
		case 14:
			var points = cmd.a;
			return 's ' + renderPoints(points);
		case 15:
			var points = cmd.a;
			return 'S ' + renderPoints(points);
		case 16:
			var arc = cmd.a;
			var deltaAngle = arc.Q.a3 - arc.V.a3;
			if (_Utils_cmp(deltaAngle, 360 - 1.0e-6) > 0) {
				var dy = arc.T.a3 * elm$core$Basics$sin(
					elm$core$Basics$degrees(arc.V.a3));
				var dx = arc.T.a3 * elm$core$Basics$cos(
					elm$core$Basics$degrees(arc.V.a3));
				return 'A ' + (elm$core$String$fromFloat(arc.T.a3) + (',' + (elm$core$String$fromFloat(arc.T.a3) + (',0,1,1,' + (elm$core$String$fromFloat(arc._.a3 - dx) + (',' + (elm$core$String$fromFloat(arc.aa.a3 - dy) + (' A ' + (elm$core$String$fromFloat(arc.T.a3) + (',' + (elm$core$String$fromFloat(arc.T.a3) + (',0,1,1,' + (elm$core$String$fromFloat(arc._.a3 + dx) + (',' + elm$core$String$fromFloat(arc.aa.a3 + dy)))))))))))))));
			} else {
				return 'A ' + (elm$core$String$fromFloat(arc.T.a3) + (',' + (elm$core$String$fromFloat(arc.T.a3) + (' 0 ' + (((deltaAngle >= 180) ? '1' : '0') + (' ' + ('1' + (' ' + (elm$core$String$fromFloat(
					arc._.a3 + (arc.T.a3 * elm$core$Basics$cos(
						elm$core$Basics$degrees(arc.Q.a3)))) + (',' + elm$core$String$fromFloat(
					arc.aa.a3 + (arc.T.a3 * elm$core$Basics$sin(
						elm$core$Basics$degrees(arc.Q.a3))))))))))))));
			}
		case 17:
			var arc = cmd.a;
			var deltaAngle = arc.Q.a3 - arc.V.a3;
			if (_Utils_cmp(deltaAngle, 360 - 1.0e-6) > 0) {
				var dy = arc.T.a3 * elm$core$Basics$sin(
					elm$core$Basics$degrees(arc.V.a3));
				var dx = arc.T.a3 * elm$core$Basics$cos(
					elm$core$Basics$degrees(arc.V.a3));
				return 'A ' + (elm$core$String$fromFloat(arc.T.a3) + (',' + (elm$core$String$fromFloat(arc.T.a3) + (',0,1,0,' + (elm$core$String$fromFloat(arc._.a3 - dx) + (',' + (elm$core$String$fromFloat(arc.aa.a3 - dy) + (' A ' + (elm$core$String$fromFloat(arc.T.a3) + (',' + (elm$core$String$fromFloat(arc.T.a3) + (',0,1,1,' + (elm$core$String$fromFloat(arc._.a3 + dx) + (',' + elm$core$String$fromFloat(arc.aa.a3 + dy)))))))))))))));
			} else {
				return 'A ' + (elm$core$String$fromFloat(arc.T.a3) + (',' + (elm$core$String$fromFloat(arc.T.a3) + (' 0 ' + ((((arc.V.a3 - arc.Q.a3) >= 180) ? '1' : '0') + (' ' + ('0' + (' ' + (elm$core$String$fromFloat(
					arc._.a3 + (arc.T.a3 * elm$core$Basics$cos(arc.Q.a3))) + (',' + elm$core$String$fromFloat(
					arc.aa.a3 + (arc.T.a3 * elm$core$Basics$sin(arc.Q.a3)))))))))))));
			}
		default:
			return 'z';
	}
};
var mdgriffith$elm_style_animation$Animation$Render$propertyValue = F2(
	function (prop, delim) {
		switch (prop.$) {
			case 0:
				var value = prop.b;
				return value;
			case 1:
				var r = prop.b;
				var g = prop.c;
				var b = prop.d;
				var a = prop.e;
				return 'rgba(' + (elm$core$String$fromInt(
					elm$core$Basics$round(r.a3)) + (',' + (elm$core$String$fromInt(
					elm$core$Basics$round(g.a3)) + (',' + (elm$core$String$fromInt(
					elm$core$Basics$round(b.a3)) + (',' + (elm$core$String$fromFloat(a.a3) + ')')))))));
			case 2:
				var name = prop.a;
				var inset = prop.b;
				var shadow = prop.c;
				return (inset ? 'inset ' : '') + (elm$core$String$fromFloat(shadow.n.a3) + ('px' + (' ' + (elm$core$String$fromFloat(shadow.o.a3) + ('px' + (' ' + (elm$core$String$fromFloat(shadow.m.a3) + ('px' + (' ' + ((((name === 'text-shadow') || (name === 'drop-shadow')) ? '' : (elm$core$String$fromFloat(shadow.r.a3) + ('px' + ' '))) + ('rgba(' + (elm$core$String$fromInt(
					elm$core$Basics$round(shadow.k.a3)) + (', ' + (elm$core$String$fromInt(
					elm$core$Basics$round(shadow.j.a3)) + (', ' + (elm$core$String$fromInt(
					elm$core$Basics$round(shadow.h.a3)) + (', ' + (elm$core$String$fromFloat(shadow.g.a3) + ')'))))))))))))))))));
			case 3:
				var x = prop.b;
				return _Utils_ap(
					elm$core$String$fromFloat(x.a3),
					x.a8);
			case 4:
				var x = prop.b;
				var y = prop.c;
				return _Utils_ap(
					elm$core$String$fromFloat(x.a3),
					_Utils_ap(
						x.a8,
						_Utils_ap(
							delim,
							_Utils_ap(
								elm$core$String$fromFloat(y.a3),
								y.a8))));
			case 5:
				var x = prop.b;
				var y = prop.c;
				var z = prop.d;
				return _Utils_ap(
					elm$core$String$fromFloat(x.a3),
					_Utils_ap(
						x.a8,
						_Utils_ap(
							delim,
							_Utils_ap(
								elm$core$String$fromFloat(y.a3),
								_Utils_ap(
									y.a8,
									_Utils_ap(
										delim,
										_Utils_ap(
											elm$core$String$fromFloat(z.a3),
											z.a8)))))));
			case 6:
				var w = prop.b;
				var x = prop.c;
				var y = prop.d;
				var z = prop.e;
				return _Utils_ap(
					elm$core$String$fromFloat(w.a3),
					_Utils_ap(
						w.a8,
						_Utils_ap(
							delim,
							_Utils_ap(
								elm$core$String$fromFloat(x.a3),
								_Utils_ap(
									x.a8,
									_Utils_ap(
										delim,
										_Utils_ap(
											elm$core$String$fromFloat(y.a3),
											_Utils_ap(
												y.a8,
												_Utils_ap(
													delim,
													_Utils_ap(
														elm$core$String$fromFloat(z.a3),
														z.a8))))))))));
			case 7:
				var x = prop.b;
				return _Utils_ap(
					elm$core$String$fromFloat(x.a3),
					x.a8);
			case 8:
				var coords = prop.a;
				return A2(
					elm$core$String$join,
					' ',
					A2(
						elm$core$List$map,
						function (_n1) {
							var x = _n1.a;
							var y = _n1.b;
							return elm$core$String$fromFloat(x.a3) + (',' + elm$core$String$fromFloat(y.a3));
						},
						coords));
			default:
				var cmds = prop.a;
				return A2(
					elm$core$String$join,
					' ',
					A2(elm$core$List$map, mdgriffith$elm_style_animation$Animation$Render$pathCmdValue, cmds));
		}
	});
var mdgriffith$elm_style_animation$Animation$Render$renderAttrs = function (prop) {
	if (A2(
		elm$core$String$startsWith,
		'attr:',
		mdgriffith$elm_style_animation$Animation$Model$propertyName(prop))) {
		return elm$core$Maybe$Just(
			A2(
				elm$html$Html$Attributes$attribute,
				A2(
					elm$core$String$dropLeft,
					5,
					mdgriffith$elm_style_animation$Animation$Model$propertyName(prop)),
				A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
	} else {
		switch (prop.$) {
			case 8:
				var pts = prop.a;
				return elm$core$Maybe$Just(
					elm$svg$Svg$Attributes$points(
						A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
			case 9:
				var cmds = prop.a;
				return elm$core$Maybe$Just(
					elm$svg$Svg$Attributes$d(
						A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
			case 3:
				var name = prop.a;
				var m1 = prop.b;
				switch (name) {
					case 'x':
						return elm$core$Maybe$Just(
							elm$svg$Svg$Attributes$x(
								A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
					case 'y':
						return elm$core$Maybe$Just(
							elm$svg$Svg$Attributes$y(
								A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
					case 'cx':
						return elm$core$Maybe$Just(
							elm$svg$Svg$Attributes$cx(
								A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
					case 'cy':
						return elm$core$Maybe$Just(
							elm$svg$Svg$Attributes$cy(
								A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
					case 'rx':
						return elm$core$Maybe$Just(
							elm$svg$Svg$Attributes$rx(
								A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
					case 'ry':
						return elm$core$Maybe$Just(
							elm$svg$Svg$Attributes$ry(
								A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
					case 'r':
						return elm$core$Maybe$Just(
							elm$svg$Svg$Attributes$r(
								A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
					case 'offset':
						return elm$core$Maybe$Just(
							elm$svg$Svg$Attributes$offset(
								A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' ')));
					default:
						return elm$core$Maybe$Nothing;
				}
			case 6:
				var name = prop.a;
				var m1 = prop.b;
				var m2 = prop.c;
				var m3 = prop.d;
				var m4 = prop.e;
				return (name === 'viewBox') ? elm$core$Maybe$Just(
					elm$svg$Svg$Attributes$viewBox(
						A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' '))) : elm$core$Maybe$Nothing;
			default:
				return elm$core$Maybe$Nothing;
		}
	}
};
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var mdgriffith$elm_style_animation$Animation$Render$isAttr = function (prop) {
	return A2(
		elm$core$String$startsWith,
		'attr:',
		mdgriffith$elm_style_animation$Animation$Model$propertyName(prop)) || function () {
		switch (prop.$) {
			case 8:
				return true;
			case 9:
				return true;
			case 3:
				var name = prop.a;
				return (name === 'cx') || ((name === 'cy') || ((name === 'x') || ((name === 'y') || ((name === 'rx') || ((name === 'ry') || ((name === 'r') || (name === 'offset')))))));
			case 6:
				var name = prop.a;
				return name === 'viewBox';
			default:
				return false;
		}
	}();
};
var mdgriffith$elm_style_animation$Animation$Render$isFilter = function (prop) {
	return A2(
		elm$core$List$member,
		mdgriffith$elm_style_animation$Animation$Model$propertyName(prop),
		_List_fromArray(
			['filter-url', 'blur', 'brightness', 'contrast', 'grayscale', 'hue-rotate', 'invert', 'saturate', 'sepia', 'drop-shadow']));
};
var mdgriffith$elm_style_animation$Animation$Render$render3dRotation = function (prop) {
	if (prop.$ === 5) {
		var x = prop.b;
		var y = prop.c;
		var z = prop.d;
		return 'rotateX(' + (elm$core$String$fromFloat(x.a3) + (x.a8 + (') rotateY(' + (elm$core$String$fromFloat(y.a3) + (y.a8 + (') rotateZ(' + (elm$core$String$fromFloat(z.a3) + (z.a8 + ')'))))))));
	} else {
		return '';
	}
};
var mdgriffith$elm_style_animation$Animation$Render$renderValues = function (_n0) {
	var model = _n0;
	var _n1 = A2(elm$core$List$partition, mdgriffith$elm_style_animation$Animation$Render$isAttr, model.l);
	var attrProps = _n1.a;
	var styleProps = _n1.b;
	var _n2 = A3(
		elm$core$List$foldl,
		F2(
			function (prop, _n3) {
				var myStyle = _n3.a;
				var myTransforms = _n3.b;
				var myFilters = _n3.c;
				return mdgriffith$elm_style_animation$Animation$Render$isTransformation(prop) ? _Utils_Tuple3(
					myStyle,
					_Utils_ap(
						myTransforms,
						_List_fromArray(
							[prop])),
					myFilters) : (mdgriffith$elm_style_animation$Animation$Render$isFilter(prop) ? _Utils_Tuple3(
					myStyle,
					myTransforms,
					_Utils_ap(
						myFilters,
						_List_fromArray(
							[prop]))) : _Utils_Tuple3(
					_Utils_ap(
						myStyle,
						_List_fromArray(
							[prop])),
					myTransforms,
					myFilters));
			}),
		_Utils_Tuple3(_List_Nil, _List_Nil, _List_Nil),
		styleProps);
	var style = _n2.a;
	var transforms = _n2.b;
	var filters = _n2.c;
	var renderedFilters = elm$core$List$isEmpty(filters) ? _List_Nil : _List_fromArray(
		[
			_Utils_Tuple2(
			'filter',
			A2(
				elm$core$String$join,
				' ',
				A2(
					elm$core$List$map,
					function (prop) {
						var name = mdgriffith$elm_style_animation$Animation$Model$propertyName(prop);
						return (name === 'filter-url') ? ('url(\"' + (A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ', ') + '\")')) : (mdgriffith$elm_style_animation$Animation$Model$propertyName(prop) + ('(' + (A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ', ') + ')')));
					},
					filters)))
		]);
	var renderedStyle = A2(
		elm$core$List$map,
		function (prop) {
			return _Utils_Tuple2(
				mdgriffith$elm_style_animation$Animation$Model$propertyName(prop),
				A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ' '));
		},
		style);
	var renderedTransforms = elm$core$List$isEmpty(transforms) ? _List_Nil : _List_fromArray(
		[
			_Utils_Tuple2(
			'transform',
			A2(
				elm$core$String$join,
				' ',
				A2(
					elm$core$List$map,
					function (prop) {
						return (mdgriffith$elm_style_animation$Animation$Model$propertyName(prop) === 'rotate3d') ? mdgriffith$elm_style_animation$Animation$Render$render3dRotation(prop) : (mdgriffith$elm_style_animation$Animation$Model$propertyName(prop) + ('(' + (A2(mdgriffith$elm_style_animation$Animation$Render$propertyValue, prop, ', ') + ')')));
					},
					transforms)))
		]);
	return _Utils_Tuple2(
		_Utils_ap(
			renderedTransforms,
			_Utils_ap(renderedFilters, renderedStyle)),
		attrProps);
};
var mdgriffith$elm_style_animation$Animation$Render$render = function (animation) {
	var _n0 = mdgriffith$elm_style_animation$Animation$Render$renderValues(animation);
	var style = _n0.a;
	var attrProps = _n0.b;
	var otherAttrs = A2(elm$core$List$filterMap, mdgriffith$elm_style_animation$Animation$Render$renderAttrs, attrProps);
	var styleAttr = A2(
		elm$core$List$map,
		function (_n1) {
			var prop = _n1.a;
			var val = _n1.b;
			return A2(elm$html$Html$Attributes$style, prop, val);
		},
		A2(elm$core$List$concatMap, mdgriffith$elm_style_animation$Animation$Render$prefix, style));
	return _Utils_ap(styleAttr, otherAttrs);
};
var mdgriffith$elm_style_animation$Animation$render = mdgriffith$elm_style_animation$Animation$Render$render;
var author$project$Main$view = function (model) {
	var points = author$project$Main$functionToPoints(
		author$project$Main$addScalarsToFunction(model));
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text(
								'Inner: ' + elm$core$String$fromFloat(model.v))
							])),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$placeholder(''),
								elm$html$Html$Attributes$value(
								elm$core$String$fromFloat(model.v)),
								elm$html$Html$Events$onInput(author$project$Main$ChangeInner)
							]),
						_List_Nil),
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text(
								'Outer: ' + elm$core$String$fromFloat(model.y))
							])),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$placeholder(''),
								elm$html$Html$Attributes$value(
								elm$core$String$fromFloat(model.y)),
								elm$html$Html$Events$onInput(author$project$Main$ChangeOuter)
							]),
						_List_Nil),
						A2(elm$html$Html$div, _List_Nil, _List_Nil),
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Events$onClick(
								author$project$Main$ChangeFunc('sin'))
							]),
						_List_fromArray(
							[
								elm$html$Html$text('sin')
							])),
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Events$onClick(
								author$project$Main$ChangeFunc('cos'))
							]),
						_List_fromArray(
							[
								elm$html$Html$text('cos')
							])),
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Events$onClick(
								author$project$Main$ChangeFunc('tan'))
							]),
						_List_fromArray(
							[
								elm$html$Html$text('tan')
							])),
						A2(elm$html$Html$div, _List_Nil, _List_Nil),
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text(
								elm$core$String$fromFloat(model.y) + ('(sin/cos/tan)' + ('(' + (elm$core$String$fromFloat(model.v) + 'x)'))))
							]))
					])),
				A2(
				elm$svg$Svg$svg,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$width(
						elm$core$String$fromFloat(author$project$Main$graphWidth)),
						elm$svg$Svg$Attributes$height(
						elm$core$String$fromFloat(author$project$Main$graphHeight))
					]),
				_List_fromArray(
					[
						A2(
						elm$svg$Svg$path,
						_Utils_ap(
							author$project$Main$defaultLineAttributes,
							mdgriffith$elm_style_animation$Animation$render(model.l)),
						_List_Nil)
					]))
			]));
};
var elm$browser$Browser$element = _Browser_element;
var author$project$Main$main = elm$browser$Browser$element(
	{aZ: author$project$Main$init, a5: author$project$Main$subscriptions, a9: author$project$Main$update, bc: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(
	elm$json$Json$Decode$succeed(0))(0)}});}(this));