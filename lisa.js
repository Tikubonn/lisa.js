
// argumentstoarray

function slice (sequence, beginning, end){
    return Array.prototype.slice.call(sequence, beginning, end);
}

// get object keys

function keys (some){
    return Object.keys(some);
}

// strace

function Strace (){
    this.strace = [];
};

Strace.prototype.push = function (func){
    this.strace.push(func + "");
};

Strace.prototype.pop = function (func){
    this.strace.pop();
};

Strace.prototype.willstrace = function (func){
    var temp, self = this;
    return function willstrace_closure (){
        self.push(this);
        temp = func.apply(this, arguments);
        self.pop();
        return temp;
    };
};

Strace.prototype.unwindstrace = function (func){
    var temp, self = this;
    return function unwindstrace_closure (){
        try { temp = func.apply(this, arguments); }
        catch (errorn) { self.print(); throw errorn; }
        return temp;
    };
};

Strace.prototype.print = function (){
    var index;
    for (index = 0; index < this.strace.length; index++)
        console.log("" + index + ":" + this.strace[index]);
};

var strace = new Strace();

// unique class
//     <- native, function class

function Unique (){}

// name gemerator class
//     <- native, function class

function NameGenerator (source){
    this.source = source;
    this.index = 0;
}

NameGenerator.prototype.generate = function (){
    var name = "";
    var index = this.index ++;
    var source = this.source;
    do {
        name += source[index % source.length];
        index = index / source.length | 0;
    } while (index);
    return name;
};

// name generator ignroe class
//     <- native, function class

function NameGeneratorIgnore (source, ignores){
    this.source = source;
    this.index = 0;
    this.ignores = ignores;
};

NameGeneratorIgnore.prototype.generate = function (){
    var name;
    while (this.ignores.indexOf(
        (name = NameGenerator.prototype.generate.call(this))) >= 0);
    return name;
};

// constantable class
//     <- native, function class

function Constantable (){};

Constantable.prototype.constantable = false;

Constantable.prototype.constant = function (){
    this.constantable = true;
    return this;
};

Constantable.prototype.notconstant = function (){
    this.constantable = false;
    return this;
};

Constantable.prototype.inherit = function (constantable){
    this.constantable = constantable.isconstant();
    return this;
};

Constantable.prototype.isconstant = function (){
    return this.constantable;
};

Constantable.prototype.isnotconstant = function (){
    return this.isconstant() == false;
};

// evaluatable class
//     <- constantable class

function Evaluatable (){}

Evaluatable.prototype = Object.create(Constantable.prototype);
Evaluatable.prototype.onevaluate = null;
Evaluatable.prototype.onevaluatearg = null;
Evaluatable.prototype.onevaluatedata = null;

function evaluate (some){
    if (some instanceof Evaluatable == false)
        throw new Error("" + some + " is not evaluatable");
    return some.evaluate.apply(some, arguments);
};

function evaluatearg (some){
    if (some instanceof Evaluatable == false)
        throw new Error("" + some + " is not evaluatable");
    return some.evaluatearg.apply(some, arguments);
};

function evaluatedata (some){
    if (some instanceof Evaluatable == false)
        throw new Error("" + some + " is not evaluatable");
    return some.evaluatedata.apply(some, arguments);
};

function beforeevaluate (func){
    return function beforeevaluate_closure (){
        return func.apply(this, slice(arguments).map(evaluate));
    };
}

function beforeevaluatearg (func){
    return function beforeevaluatearg_closure (){
        return func.apply(this, slice(arguments).map(evaluatearg));
    };
}

function beforeevaluatedata (func){
    return function beforeevaluatedata_closure (){
        return func.apply(this, slice(arguments).map(evaluatedata));
    };
}

function afterevaluate (func){
    return function afterevaluate_closure (){
        return func.apply(this, arguments).evaluate();
    };
}

function afterevaluatearg (func){
    return function afterevaluatearg_closure (){
        return func.apply(this, arguments).evaluatearg();
    };
}

function afterevaluatedata (func){
    return function afterevaluatedata_closure (){
        return func.apply(this, slice(arguments).map(evaluatedata));
    };
}

Evaluatable.prototype.evaluate = function (){
    if (this.onevaluate == null)
        throw new Error("" + this + " onevaluate was not defined.");
    return this.onevaluate.apply(this, arguments);
};

Evaluatable.prototype.evaluatearg  = function (){
    if (this.onevaluatearg == null)
        throw new Error("" + this + " onevaluatearg was not defined.");
    return this.onevaluatearg();
};

Evaluatable.prototype.evaluate = 
    strace.willstrace(Evaluatable.prototype.evaluate);

Evaluatable.prototype.evaluatedata = function (){
    if (this.onevaluatedata == null)
        return this.evaluatearg();
    return this.onevaluatedata();
};

Evaluatable.prototype.willevaluate = function (){
    var that = this;
    return function willevaluate_closure (){
        return that.evaluate.apply(that, arguments);
    };
};

Evaluatable.prototype.willevaluatearg = function (){
    var that = this;
    return function willevaluatearg_closure (){
        return that.evaluatearg.apply(that, arguments);
    };
};

// expandable class
//     <- constantable class

function Expandable (){}

Expandable.prototype = Object.create(Evaluatable.prototype);
Expandable.prototype.onexpand = null;
Expandable.prototype.onexpandarg = null;
Expandable.prototype.onexpanddata= null;

function expand (some){
    if (some instanceof Expandable == false)
        throw new Error("" + some + " is not expandable");
    return some.expand.apply(some, arguments);
};

function expandarg (some){
    if (some instanceof Expandable == false)
        throw new Error("" + some + " is not expandable");
    return some.expandarg.apply(some, arguments);
};

function expanddata (some){
    if (some instanceof Expandable == false)
        throw new Error("" + some + " is not expandable");
    return some.expanddata.apply(some, arguments);
};

function beforeexpand (func){
    return function beforeexpand_closure (){
        return func.apply(this, slice(arguments).map(expand));
    };
}

function beforeexpandarg (func){
    return function beforeexpandarg_closure (){
        return func.apply(this, slice(arguments).map(expandarg));
    };
}

function beforeexpanddata (func){
    return function beforeexpanddata_closure (){
        return func.apply(this, slice(arguments).map(expanddata));
    };
}

function afterexpand (func){
    return function afterexpand_closure (){
        return func.apply(this, arguments).expand();
    };
}

function afterexpandarg (func){
    return function afterexpandarg_closure (){
        return func.apply(this, arguments).expandarg();
    };
}

function afterexpanddata (func){
    return function afterexpanddata_closure (){
        return func.apply(this, arguments).expanddata();
    };
}

Expandable.prototype.expand = function (){
    if (this.onexpand == null)
        throw new Error("" + this + " onexpand was not defined.");
    return this.onexpand.apply(this, arguments);
};

Expandable.prototype.expandarg = function (){
    if (this.onexpandarg == null)
        throw new Error("" + this + "onexpandarg was not defined.");
    return this.onexpandarg();
};

Expandable.prototype.expanddata = function (){
    if (this.onexpanddata == null)
        return this.expandarg();
    return this.onexpanddata();
};

Expandable.prototype.willexpand = function (){
    var that = this;
    return function willexpand_closure (){
        return that.expand.apply(that, arguments);
    };
};

Expandable.prototype.willexpandarg = function (){
    var that = this;
    return function willexpandarg_closure (){
        return that.expandarg.apply(that, arguments);
    };
};

Expandable.prototype.willexpanddata = function (){
    var that = this;
    return function willexpanddata_closure (){
        return that.expanddata.apply(that, arguments);
    };
};

// expanded class
//     <- expandable class

function Expanded (value){
    this.value = value || "";
}

Expanded.prototype = 
    Object.create(Expandable.prototype);

Expanded.prototype.toString = function (){
    return this.value;
};

Expanded.prototype.expand = function (){
    return new Expanded(this.value + "(" + slice(arguments).map(expandarg).join(",") + ")");
};

Expanded.prototype.onevaluatearg = function (){
    return this;
};

Expanded.prototype.onexpandarg = function () {
    return this;
};

Expanded.prototype.unpack = function (){
    return this.value;
};

function unpack (some){
    if (some instanceof Expanded == false)
        throw new Error("some is not expanded.");
    return some.unpack();
};

// atom class
//     <- expandable class

function AtomClass (value){
    this.value = value;
};

AtomClass.prototype = 
    Object.create(Expandable.prototype);

AtomClass.prototype.toString = function (){
    return this.value.toString();
};

AtomClass.prototype.valueOf = function (){
    return this.toString();
};

AtomClass.prototype.onevaluatearg = function (){
    return this;
};

AtomClass.prototype.onevaluatedata = function (){
    return this;
};

AtomClass.prototype.onexpandarg = function (){
    return new Expanded(this.toString());
};

AtomClass.prototype.status = function (){
    return true;
};

AtomClass.prototype.clone = function (){
    return this;
};

function value (some){
    if (some instanceof AtomClass == false) 
        throw new Error("" + some + " is not atom class."); 
    return some.value;
}

function tostring (some){
    if (some instanceof AtomClass == false)
        throw new Error("" + some + " is not atom class."); 
    return some.toString();
}

function clone (some){
    if (some instanceof AtomClass == false)
        throw new Error("" + some + " is not atom class.");
    return some.clone();
}

function beforeclone (func){
    return function beforeclone_closure (){
        return func.apply(func, slice(arguments).map(clone));
    };
}

function afterclone (func){
    return function afterclone_closure (){
        return func.apply(func, arguments).clone();
    };
}

// boolean class
//     <- atom class

function BooleanClass (value){
    this.value = value;
}

BooleanClass.prototype = 
    Object.create(AtomClass.prototype);

BooleanClass.prototype.status = function (){
    return this.value;
};

var t = new BooleanClass(true);
var f = new BooleanClass(false);

// iteratable class
//     <- atom class

function IteratorClass (){}

IteratorClass.prototype = 
    Object.create(AtomClass.prototype);

IteratorClass.prototype.isdie = function (){throw new Error("isdie was not defined.");};
IteratorClass.prototype.isalive = function (){throw new Error("isalive was not defined.");};
IteratorClass.prototype.next = function (){throw new Error("next was not defined.");};
IteratorClass.prototype.count = function (){throw new Error("count was not defined.");};

// array iteration class
//     <- iteration class

function ArrayIterationClass (array){
    this.array = array;
    this.index = 0;
}

// cons iterator class
//     <- iterator class

function ConsIteratorClass (cons){
    this.index = 0;
    this.value = cons;
}

ConsIteratorClass.prototype = 
    Object.create(IteratorClass.prototype);

ConsIteratorClass.prototype.isdie = function (){
    return this.isalive() == false;
};

ConsIteratorClass.prototype.isalive = function (){
    return this.value.status();
};

ConsIteratorClass.prototype.next = function (){
    if (this.isdie()) return null;
    var temp = this.value.car;
    this.value = this.value.cdr;
    this.index++;
    return temp;
};

ConsIteratorClass.prototype.count = function (){
    return this.index;
};

// array iterator class
//     <- iterator class

function ArrayIteratorClass (array){
    this.index = 0;
    this.value = array;
}

ArrayIteratorClass.prototype = 
    Object.create(IteratorClass.prototype);

ArrayIteratorClass.prototype.isdie = function (){
    return this.isalive() == false;
};

ArrayIteratorClass.prototype.isalive = function (){
    return this.index < this.value.length();
};

ArrayIteratorClass.prototype.next = function (){
    if (this.isdie()) return null;
    var temp = this.value.nth(this.index);
    this.index++;
    return temp;
};

ArrayIteratorClass.prototype.count = function (){
    return this.index;
};

// reference class
//     <- atom class

function ReferenceClass (){}

ReferenceClass.prototype = 
    Object.create(AtomClass.prototype);

ReferenceClass.prototype.toString = function (){
    return this.get().toString();
};

ReferenceClass.prototype.get = function (){
    throw new Error("get was not defined.");
};

ReferenceClass.prototype.set = function (){
    throw new Error("set was not defined.");
};

ReferenceClass.prototype.onevaluatearg = function (){
    return this.get();
};

ReferenceClass.prototype.onevaluate = null;
ReferenceClass.prototype.onexpand = null;
ReferenceClass.prototype.onexpandarg = null;

// ReferenceClass.prototype.onexpandarg = function (){
//     return this.get().expanddata();
// };

// array reference class 
//     <- atom class

function ArrayReferenceClass (value, index){
    this.value = value || null;
    this.index = index;
}

ArrayReferenceClass.prototype = 
    Object.create(ReferenceClass.prototype);

ArrayReferenceClass.prototype.get = function (){
    return this.value.evaluatedata().get(this.index);
};

ArrayReferenceClass.prototype.set = function (value){
    return this.value.evaluatedata().set(this.index, value);
};

ArrayReferenceClass.prototype.onexpanddata = function (){
    return new Expanded(this.value.expanddata().unpack() + "[" + this.index + "]");
};

// ArrayReferenceClass.prototype.get = function (){
//     return this.value.value[this.index];
// };

// ArrayReferenceClass.prototype.set = function (value){
//     this.value.value[this.index] = value;
//     return value;
// };

// cons reference class
//     <- reference class

function ConsReferenceClass (value){
    this.value = value || null;
}

ConsReferenceClass.prototype = 
    Object.create(ConsReferenceClass.prototype);

ConsReferenceClass.prototype.get = function (){
    return this.value.car;
};

ConsReferenceClass.prototype.set = function(value){
    this.value.car = value;
    return value;
};

// nil reference class
//     <- reference class

function NilReferenceClass (value){}

NilReferenceClass.prototype = 
    Object.create(ReferenceClass.prototype);

NilReferenceClass.prototype.get = function (){
        return nil;
};

NilReferenceClass.prototype.set = function (){
        throw new Error("nil reference could not assign.");
};

var nilf = new NilReferenceClass();

// symbol reference class
//      <- reference class

function SymbolReferenceClass (){};

SymbolReferenceClass.prototype = 
    Object.create(ReferenceClass.prototype);

// symbol value reference class

function SymbolValueReferenceClass (value){
    this.value = value || null;
}

SymbolValueReferenceClass.prototype = 
    Object.create(SymbolReferenceClass.prototype);

SymbolValueReferenceClass.prototype.get = function (){
    return this.value.getvaluee();
};

SymbolValueReferenceClass.prototype.set = function (value){
    return this.value.setvalue(value);
};

SymbolValueReferenceClass.prototype.onexpandarg = function (){
    return new Expanded(this.value.getvaluename());
};

// symbol function reference class

function SymbolFunctionReferenceClass (value){
    this.value = value || null;
}

SymbolFunctionReferenceClass.prototype = 
    Object.create(SymbolReferenceClass.prototype);

SymbolFunctionReferenceClass.prototype.get = function (){
    return this.value.getfunce();
};

SymbolFunctionReferenceClass.prototype.set = function (func){
    return this.value.setfunc(func);
};

SymbolFunctionReferenceClass.prototype.onexpandarg = function (){
    return new Expanded(this.value.getfuncname());
};

// number class
//     <- atom class

function NumberClass (number){
    this.value = number;
}

NumberClass.prototype =
    Object.create(AtomClass.prototype);

NumberClass.prototype.clone = function (){
    return new NumberClass(this.value);
};

NumberClass.prototype.add = function (num){
    throw new Error("number.class.add was not defined.");
};

NumberClass.prototype.sub = function (num){
    throw new Error("number.class.sub was not defined.");
};

NumberClass.prototype.mul = function (num){
    throw new Error("number.class.mul was not defined.");
};

NumberClass.prototype.div = function (num){
    throw new Error("number.class.div was not defined.");
};

NumberClass.prototype.mod = function (num){
    throw new Error("number.class.mod was not defined.");
};

// float class
//     <- float class

function FloatClass (number){
    this.value = number;
};

FloatClass.prototype = 
    Object.create(NumberClass.prototype);

FloatClass.prototype.clone = function (){
    return new FloatClass(this.value);
};

FloatClass.prototype.add = function (num){
    if (num instanceof NumberClass == false)
        throw new Error("num " + num + " is not number class.");
    return new FloatClass(this.value + num.value);
};

FloatClass.prototype.sub = function (num){
    if (num instanceof NumberClass == false)
        throw new Error("num " + num + " is not number class.");
    return new FloatClass(this.value - num.value);
};

FloatClass.prototype.mul = function (num){
    if (num instanceof NumberClass == false)
        throw new Error("num " + num + " is not number class.");
    return new FloatClass(this.value * num.value);
};

FloatClass.prototype.div = function (num){
    if (num instanceof NumberClass == false)
        throw new Error("num " + num + " is not number class.");
    return new FloatClass(this.value / num.value);
};

FloatClass.prototype.mod = function (num){
    throw new Error("float instance " + this + " could not do mod.");
};

// int class
//     <- number class

function IntClass (number){
    this.value = number;
};

IntClass.prototype = 
    Object.create(NumberClass.prototype);

IntClass.prototype.toString = function (){
    return "(" + this.value.toString() + "|0)";
};

IntClass.prototype.clone = function (){
    return new IntClass(this.value);
};

IntClass.prototype.add = function (num){
    if (num instanceof NumberClass == false)
        throw new Error("num " + num + " is not number class");
    return num instanceof IntClass ?
        new IntClass(this.value + num.value):
        num.add(this);
};

IntClass.prototype.sub = function (num){
    if (num instanceof NumberClass == false)
        throw new Error("num " + num + " is not number class");
    return num instanceof IntClass ?
        new IntClass(this.value - num.value): 
        num.sub(this);
};

IntClass.prototype.mul = function (num){
    if (num instanceof NumberClass == false)
        throw new Error("num " + num + " is not number class");
    return num instanceof IntClass ?
        new IntClass(this.value * num.value): 
        num.mul(this);
};

IntClass.prototype.div = function (num){
    if (num instanceof NumberClass == false)
        throw new Error("num " + num + " is not number class");
    return num instanceof IntClass ?
        new IntClass(this.value / num.value): 
        num.div(this);
};

IntClass.prototype.mod = function (num){
    if (num instanceof NumberClass == false)
        throw new Error("num " + num + " is not number class");
    return num instanceof IntClass ?
        new IntClass(this.value % num.value): 
        num.mod(this);
};

function makeint (num){
    return new IntClass(num);
};

// char class
//      <- int class

function CharClass (code){
    this.value = code;
}

CharClass.prototype = 
    Object.create(IntClass.prototype);

CharClass.prototype.toString = function (){
    return String.fromCharCode(this.value);
};

CharClass.prototype.clone = function (){
    return new CharClass(this.value);
};

CharClass.prototype.add = function (num){
    if (num instanceof IntClass == false)
        throw new Error("num " + num + " is not int class.");
    return new CharClass(this.value + num.value);
};

CharClass.prototype.sub = function (num){
    if (num instanceof IntClass == false)
        throw new Error("num " + num + " is not int class.");
    return new CharClass(this.value - num.value);
};

CharClass.prototype.mul = function (num){
    if (num instanceof IntClass == false)
        throw new Error("num " + num + " is not int class.");
    return new CharClass(this.value * num.value);
};

CharClass.prototype.div = function (num){
    if (num instanceof IntClass == false)
        throw new Error("num " + num + " is not int class.");
    return new CharClass(this.value / num.value);
};

CharClass.prototype.mod = function (num){
    if (num instanceof IntClass == false)
        throw new Error("num " + num + " is not int class.");
    return new CharClass(this.value % num.value);
};

function atoi (chars){
    return chars.charCodeAt();
};

function atoc (chars){
    return new CharClass(atoi(chars));
};

// sequencial class
//     <- atom class

function SequencialClass (){}

SequencialClass.prototype =
    Object.create(AtomClass.prototype);

SequencialClass.prototype.get = function (){throw new Error("get was not defined.");};
SequencialClass.prototype.set = function (){throw new Error("set was not defined.");};
SequencialClass.prototype.toArray = function (){throw new Error("toArray was not defined.");};
SequencialClass.prototype.toPlain = function (){throw new Error("toPlain was not defined.");};
SequencialClass.prototype.every = function (){throw new Error("every was not defined.");};
SequencialClass.prototype.map = function (){throw new Error("map was not defined.");};
SequencialClass.prototype.filter = function (){throw new Error("filter was not defined.");};
SequencialClass.prototype.reduce = function (){throw new Error("reduce was not defined.");};
SequencialClass.prototype.findif = function (){throw new Error("findif was not defined.");};
SequencialClass.prototype.positionif = function (){throw new Error("positionif was not defined.");};
SequencialClass.prototype.reverse = function (){throw new Error("reverse was not defined.");};
SequencialClass.prototype.concat = function (){throw new Error("concat was not defined.");};
SequencialClass.prototype.slice = function (){throw new Error("slice was not defined.");};
SequencialClass.prototype.copy = function (){throw new Error("clone was not defined.");};
SequencialClass.prototype.nth = function (){throw new Error("nth was not defined.");};
SequencialClass.prototype.last = function (){throw new Error("last was not defined.");};
SequencialClass.prototype.length = function (){throw new Error("length was not defined.");};
SequencialClass.prototype.iter = function (){throw new Error("iter was not defined.");};
SequencialClass.prototype.push = function (){throw new Error("push was not defined.");};
SequencialClass.prototype.pop = function (){throw new Error("pop was not defined.");};

// array class
//     <- sequencial class

function ArrayClass (array){
    this.value = array;
}

ArrayClass.prototype = 
    Object.create(SequencialClass.prototype);

ArrayClass.prototype.get = function (index){ // ** get value directly
    return this.value[index];
};

ArrayClass.prototype.set = function (index, value){ // ** set value directly
    this.value[index] = value;
    return value;
};

ArrayClass.prototype.toArray = function (){ // ** should update here
    return this.value;
};

ArrayClass.prototype.toPlain = function (){
    return this.value.join(",");
};

ArrayClass.prototype.iter = function (){
    return new ArrayIteratorClass(this);
};

ArrayClass.prototype.every = function (func){
    this.value.every(func.willevaluate); return nil;
};

ArrayClass.prototype.map = function (func){
    return new ArrayClass(this.value.map(func.willevaluate()));
};

ArrayClass.prototype.filter = function (func){
    return new ArrayClass(this.value.filter(func.willevaluate()));
};

ArrayClass.prototype.reduce = function (func){
    return this.value.reduce(func.willevaluate());
};

ArrayClass.prototype.findif = function (func){
    var index, found;
    for (found = null, index = 0; index < this.length(); index++)
        if ((found = this.nth(index)).status())
            return found;
    return nil;
};

ArrayClass.prototype.positionif = function (func){
    var index, count;
    for (count = 0, index = 0; index < this.length(); index++, count++)
        if (this.nth(index).status())
            return count;
    return nil;
};

ArrayClass.prototype.nth = function (index){
    return new ArrayReferenceClass(this, index);
}

ArrayClass.prototype.last = function (){
    return this.length() == 0 ? nil : this.nth(this.length() -1);
};

ArrayClass.prototype.length = function (){
    return this.value.length;
};

ArrayClass.prototype.reverse = function (){
    this.value.reverse();
    return this;
};

ArrayClass.prototype.copy = function (){
    return new ArrayClass(this.value.slice());
};

ArrayClass.prototype.push = function (element){
    return this.value.push(element);
};

ArrayClass.prototype.pop = function (){
    return this.value.pop();
};

// class array class 
//     <- array class

function ClassArrayClass (array, classe){
    this.value = array;
    this.classe = classe;
}

ClassArrayClass.prototype =
    Object.create(ArrayClass.prototype);

// stringn class
//     <- class array class

function StringClass (value){
    this.value = value || [];
}

StringClass.prototype = 
    Object.create(ClassArrayClass.prototype);

StringClass.prototype.onevaluate = null;
StringClass.prototype.onexpand = null;

StringClass.prototype.onevaluatearg = function (){
    return this;
};

StringClass.prototype.toString = function (){
    return '"' + this.value.join("") + '"';
}

StringClass.prototype.toPlain = function (){
    return this.value.join("");
};

StringClass.prototype.map = function (func){ // ** should check again
    return new StringClass(this.value.map(func.willevaluate()));
};

StringClass.prototype.filter = function (func){ // ** should check again
    return new StringClass(this.value.filter(func.willevaluate()));
};

StringClass.prototype.copy = function (){
    return new StringClass(this.value.slice());
};

function makestring (sentence){
    return new StringClass(slice(sentence).map(atoc));
};

// cons class

function ConsClass (car, cdr){
    this.car = car || nil;
    this.cdr = cdr || nil;
}

ConsClass.prototype = 
    Object.create(SequencialClass.prototype);

ConsClass.prototype.onexpand = function (){
    var func = this.expandarg();
    return func.expand.apply(func, arguments);
};

ConsClass.prototype.onevaluate = function (){
    var func = this.evaluatearg();
    return func.evaluate.apply(func, arguments);
};

ConsClass.prototype.onexpandarg = function (){
    var func = this.car;
    var args = this.cdr.toArray();
    return func.expand.apply(func, args);
};

ConsClass.prototype.onevaluatearg = function (){
    var func = this.car;
    var args = this.cdr.toArray();
    return func.evaluate.apply(func, args);
};

ConsClass.prototype.onexpanddata = function (){
    return new Expanded(this.toString());
};

ConsClass.prototype.toPlain = function (){
    return this.toArray().join(",");
};

ConsClass.prototype.toString = function (){
    return "[" + this.toArray().join(",") + "]";
};

ConsClass.toCons = function (sequence){
    var cons, index;
    for (cons = nil, index = 0; index < sequence.length; index++)
        cons = new ConsClass(sequence[index], cons);
    return cons.reverse();
};

ConsClass.prototype.toCons = function (){
    return this;
};

ConsClass.prototype.clone = function (){ // ** should update here
    var ncons, cons;
    for (ncons = nil, cons = this; cons != nil; cons = cons.cdr)
        if (cons.car instanceof UnQuoteAtClass){
            var consa, consb;
            for (consa = cons.car.evaluatearg(),
                 consb = consa;
                 consb != nil && consb.cdr != nil;
                 consb = consb.cdr);
            consb.cdr = ncons;
            ncons = consa;
        }
    else if (cons.car instanceof UnQuoteClass)
            ncons = new ConsClass(cons.car.evaluatearg(), ncons);
        else ncons = new ConsClass(cons.car, ncons);
    return ncons.reverse();
};

ConsClass.prototype.toArray = function (){
    this.shouldlinear();
    var sequence, cons;
    for (sequence = [], cons = this; cons != nil; cons = cons.cdr)
        sequence.push(cons.car);
    return sequence;
};

ConsClass.prototype.iter = function (){
    return new ConsIteratorClass(this);
};

ConsClass.prototype.every = function (func){
    var cons;
    for (cons = this; cons != nil; cons = cons.cdr)
        func.evaluate(cons.car);
    return nil;
};

ConsClass.prototype.map = function (func){
    var ncons, cons;
    for (ncons = nil, cons = this; cons != nil; cons = cons.cdr)
        ncons = new ConsClass(func.evaluate(cons.car), ncons);
    return ncons.reverse();
};

ConsClass.prototype.filter = function (func){
    var ncons, cons;
    for (ncons = nil, cons = this; cons != nil; cons = cons.cdr)
        if (func.evaluate(cons.car))
            ncons = new ConsClass(cons.car, ncons);
    return ncons.reverse();
};

ConsClass.prototype.reduce = function (func){
    if (this.length() == 0) return nil;
    if (this.length() == 1) return this.car;
    var sum, cons;
    for (sum = cons.car, cons = this; cons != nil; cons = cons.cdr)
        sum = func.evaluate(sum, cons.car);
    return sum;
};

ConsClass.prototype.findif = function (func){
    var  cons;
    for (cons = this; cons != nil; cons = cons.cdr)
        if (func.evaluate(cons.car))
            return cons.car;
    return nil;
};

ConsClass.prototype.positionif = function (func){
    var count, cons;
    for (count = 0, cons = this; cons != nil; cons = cons.cdr, count++)
        if (func.evaluate(cons.car))
            return count;
    return null;
};

ConsClass.prototype.nth = function (index){
    var cons;
    for (cons = this; cons != nil && index; cons = cons.cdr);
    return new ConsReferenceClass(cons);
};

ConsClass.prototype.last = function (){
    var cons;
    for (cons = this; cons != nil && cons.cdr != nil; cons = cons.cdr);
    return cons.car;
};

ConsClass.prototype.length = function (){
    var count, cons;
    for (cons = this; cons != nil; cons = cons.cdr) count++;
    return count;
};

ConsClass.prototype.reversesafe = function (){
    var ncons, cons;
    for (ncons = nil, cons = this; cons != nil; cons = cons.cdr)
        ncons = new ConsClass(cons.car, ncons);
    return ncons;
};

ConsClass.prototype.reverse = function (){
    var cons, consa, consb;
    for (cons = this, consb = nil; cons != nil;){
        consa = cons.cdr;
        cons.cdr = consb;
        consb = cons;
        cons = consa;
    };
    return consb;
};

ConsClass.prototype.copy = function (){
    var ncons, cons;
    for (ncons = nil, cons = this; cons; cons = cons.cdr)
        ncons = new ConsClass(cons.car, ncons);
    return ncons.reverse();
};

ConsClass.prototype.islinear = function (){
    var cons;
    for (cons = this; cons != nil; cons = cons.cdr)
        if (cons.cdr != nil && cons.cdr instanceof ConsClass == false)
            return false;
    return true;
};

ConsClass.prototype.shouldlinear = function (){
    if (this.islinear() == false) throw("list should be linear.");
};

function makecons (car, cdr){
    return new ConsClass(car, cdr);
};

function makelist (){
    return ConsClass.toCons(arguments);
};

// nil class
//     <- sequential class

function NilClass (){}

NilClass.prototype =  
    Object.create(ConsClass.prototype);

NilClass.prototype.car = nil;
NilClass.prototype.cdr = nil;

NilClass.prototype.onevaluatearg = function (){
    return this;
};

NilClass.prototype.onexpandarg = function (){
    return new Expanded(this.toString());
};

NilClass.prototype.onevaluate = 
    NilClass.prototype.onevaluatearg;

NilClass.prototype.onexpand = 
    NilClass.prototype.onexpandarg;

NilClass.prototype.status = function (){
    return false;
};

NilClass.prototype.toString = function (){
    return "null";
};

NilClass.prototype.toArray = function (){
    return [];
};

NilClass.prototype.every = function (func){
    return nil;
};

NilClass.prototype.map = function (func){
    return nil;
};

NilClass.prototype.filter = function (func){
    return nil;
};

NilClass.prototype.reduce = function (func){
    return nil;
};

NilClass.prototype.findif = function (func){
    return nil;
};

NilClass.prototype.positionif = function (func){
    return nil;
};

NilClass.prototype.nth = function (index){
    return nil;
};

NilClass.prototype.length = function (){
    return 0;
};

NilClass.prototype.reverse = function (){
    return this;
};

var nil = new NilClass();

// cons stack class
//     <- cons class

function ConsStackClass (car, cdr){
    this.car = car || nil;
    this.cdr = cdr || nil;
}

ConsStackClass.prototype = 
    Object.create(ConsClass.prototype);

ConsStackClass.prototype.push = function (element){
    var ncons = new ConsClass(element);
    if (this.car == nil){
        this.car = ncons;
        this.cdr = ncons;
    }
    else this.cdr.cdr = ncons;
    return element;
};

ConsStackClass.prototype.pop = function (){
    if (this.car == nil) return nil;
    var element = this.car.car;
    this.car = this.car.cdr;
    return element;
};

ConsStackClass.prototype.iter = function (){
    return this.car.iter();
};

// quote family class 
//     <- atom class

function QuoteFamilyClass (value){
    this.value = value;
}

QuoteFamilyClass.prototype = 
    Object.create(AtomClass.prototype);

// quote class
//     <- quote family class

function QuoteClass (value){
    this.value = value;
};

QuoteClass.prototype = 
    Object.create(QuoteFamilyClass.prototype);

QuoteClass.prototype.toString = function (){
    return "/*--quote--*/" + this.value.toString();
};

QuoteClass.prototype.onevaluate = function (){
    return this.value.evaluate(this.value, arguments);
};

QuoteClass.prototype.onexpand = function (){
    return this.value.expand(this.value, arguments);
};

QuoteClass.prototype.onevaluatearg = function (){
    return this.value.clone();
};

QuoteClass.prototype.onexpandarg = function (){
    return this.value.expanddata();
};

function makequote  (some){
    return new QuoteClass(some);
};

// unquote class
//     <- quote family class

function UnQuoteClass (value){
    this.value = value;
};

UnQuoteClass.prototype = 
    Object.create(QuoteFamilyClass.prototype);

UnQuoteClass.prototype.onevaluate = null;
UnQuoteClass.prototype.onexpand = null;

UnQuoteClass.prototype.toString  = function (){
    return "/*--unquote--*/" + this.value.toString();
};

UnQuoteClass.prototype.onevaluatearg = function (){
    return this.value.evaluatearg();
};

UnQuoteClass.prototype.onexpandarg = function (){
    return this.value.expandarg();
};

function makeunquote (some){
    return new UnQuoteClass(some);
};

// unquoteat class
//     <- quote family class

function UnQuoteAtClass (value){
    this.value = value;
};

UnQuoteAtClass.prototype = 
    Object.create(UnQuoteClass.prototype);

UnQuoteAtClass.prototype.toString = function (){
    return "/*--unquoteat--*/" + this.value.toString();
};

function makeunquoteat (some){
    return new UnQuoteAtClass(some);
};

// callable class
//     <- atom class

function CallableClass (){}

CallableClass.prototype =
    Object.create(AtomClass.prototype);

CallableClass.prototype.label = "<#callable class>";

CallableClass.prototype.toString = function (){
    return this.label;
};

// function class
//     <- callable class

function FunctionClass (){}

FunctionClass.prototype = 
    Object.create(CallableClass.prototype);

FunctionClass.prototype.label =  "<#function class>";

// special function class
//     <- function class

function SpecialFunctionClass (){}

SpecialFunctionClass.prototype =
    Object.create(FunctionClass.prototype);

SpecialFunctionClass.label =  "<#special function class>";

// optimize function class
//     <- function class

function OptimizeFunctionClass (){}

OptimizeFunctionClass.prototype = 
    Object.create(FunctionClass.prototype);

OptimizeFunctionClass.prototype.label = "<#optimize function class>";

function isoptimizable (some){
    return some instanceof Expanded == false ||
        some instanceof SymbolFamilyClass == false || // ** should check again
        some.isconstant() == false;
}

function isoptimizableall (sequence){
    var index;
    for (index = 0; index < sequence.length; index++)
        if (isoptimizable(sequence[index]) == false)
            return false;
    return true;
}

// reduce optimize function class
//     <- optimize function class

function ReduceOptimizeFunctionClass (func, evaluatedefault, expanddefault){
    this.func = func || null;
    this.evaluatedefault = evaluatedefault || null;
    this.expanddefault = expanddefault || null;
}

ReduceOptimizeFunctionClass.prototype = 
    Object.create(OptimizeFunctionClass.prototype);

ReduceOptimizeFunctionClass.prototype.evaluate = 
    beforeevaluatearg(OptimizeFunctionClass.prototype.evaluate);

ReduceOptimizeFunctionClass.prototype.expand = 
    beforeevaluatearg(OptimizeFunctionClass.prototype.expand);

ReduceOptimizeFunctionClass.prototype.onevaluate = function (){
    if (arguments.length == 0) return this.evaluatedefault;
    if (arguments.length == 1) return arguments[0];
    var sum, index;
    for (sum = arguments[0], index = 1; index < arguments.length; index++)
        sum = this.func.evaluate(sum, arguments[index]);
    return sum;
};

ReduceOptimizeFunctionClass.prototype.onexpand = function (){
    if (arguments.length == 0) return this.expanddefault;
    if (arguments.length == 1) return arguments[0];
    var sequence, index;
    for (sequence = [arguments[0]], index = 1; index < arguments.length; index++)
        if (isoptimizable(sequence[sequence.length -1]) == false ||
            isoptimizable(arguments[index]) == false)
            sequence.push(arguments[index]);
        else sequence[sequence.length -1] = 
            sequence[sequence.length -1] = 
            this.func.evaluate(
                sequence[sequence.length -1],
                arguments[index]);
    var sum, indexc;
    for (sum = "", indexc = 0; indexc < sequence.length; indexc++)
        sum += (indexc ? "+" : "") +  sequence[indexc];
    return new Expanded(sum);
};

ReduceOptimizeFunctionClass.prototype.expandarg = function (){
    return new Expanded("function(){Array.prototype.slice.call(arguments).reduce(" + this.func.expandarg() + ");}");
};

// primitive function class
//     <- function class

function PrimitiveFunctionClass (){}

PrimitiveFunctionClass.prototype =
    Object.create(FunctionClass.prototype);

PrimitiveFunctionClass.prototype.evaluate = 
    beforeevaluatearg(FunctionClass.prototype.evaluate);

PrimitiveFunctionClass.prototype.expand = 
    beforeexpandarg(FunctionClass.prototype.expand);

PrimitiveFunctionClass.prototype.label = "<#primitive function class>";

// user function class
//     <- function class

function UserFunctionClass (args, rest){
    this.args = args || null;
    this.rest = rest || null;
};

UserFunctionClass.prototype = 
    Object.create(FunctionClass.prototype);

UserFunctionClass.prototype.label = "<#user function class>";

UserFunctionClass.prototype.onevaluate = function (){

    // bound initalize

    var bound, consa, consb;
    bound = makecons(synprogn);
    consa = this.args;
    consb = ConsClass.toCons(arguments);

    // argument binding.

    for (; consa != nil && consb != nil; 
         consa = consa.cdr, consb = consb.cdr){
        if (consa.car == makeintern2("&rest")) break;
        if (consa.car == makeintern2("&optional")) break;
        bound = 
            makecons(
                makecons(macdeflvar,
                         makecons(consa.car,
                                  makecons(consb.car))),
                bound);
    };

    // optional argument binding.
 
    if (consa.car == makeintern2("&optional")){
        consa = consa.cdr;
        for (; consa != nil && consb != nil;
             consa = consa.cdr, consb = consb.cdr){
            if (consa.car == makeintern2("&rest")) break;
            bound =
                makecons(
                    makecons(macdeflvar,
                             (consb == nil) ?
                             (consa.car instanceof ConsClass == false) ?
                             (makecons(consa.car, makecons(nil))):
                             (makecons(consa.car.car, makecons(consa.car.cdr.car))):
                             (consa.car instanceof ConsClass == false) ?
                             (makecons(consa.car, makecons(consb.car))):
                             (makecons(consa.car.car, makecons(consb.car)))),
                    bound);
        }};
    
    // rest argument binding.

    if (consa.car == makeintern2("&rest")){
        consa = consa.cdr;
        bound = 
            makecons(
                makecons(macdeflvar,
                         makecons(consa.car,
                                  makecons(consb))),
                bound);
    };

    // reverse binding arguments.

    bound = bound.reverse();
    
    // build formula and evaluate
   
    return makecons(synblock,
                    makecons(bound, this.rest)).evaluatearg();
 
    // var formula, ncons, bindsi, index;
    // for (ncons = new ConsClass(synprogn), bindsi = this.args.iter(), index = 0;
    //      bindsi.isalive() && index < arguments.length; index++)
    //     ncons = makecons(
    //         makecons(macdeflvar,
    //                  makecons(bindsi.next(),
    //                           makecons(arguments[index]))),
    //         ncons);
    // ncons = ncons.reverse();
    // // return makecons(synblock,
    // //                 makecons(ncons,
    // //                          makecons(this.rest))).evaluatearg();
    // formula = 
    //     makecons(synblock,
    //              makecons(ncons, this.rest));
    // return formula.evaluatearg();
    // // return makecons(
    // //     synprogn,
    // //     makecons(
    // //         ncons,
    // //         makecons(
    // //             makecons(
    // //                 synblock_func,
    // //                 makecons(this.rest))))).evaluatearg();
};

UserFunctionClass.prototype.onexpandarg = function (){
    return new Expanded(
        "function(" + this.args.toArray().map(getvaluename).join(",") + ")" + 
            "{" + new ConsClass(synblock_func,
                                ConsClass.toCons(this.rest)).expandarg() + "}");
};

// macro class
//     <- atom class

function MacroClass (){}

// primitive macro class
//     <- macro class

function PrimitiveMacroClass (){}

// user macro class
//     <- macro class

function UserMacroClass (args, rest){
    this.args = args || null;
    this.rest = rest || null;
};

MacroClass.prototype = 
    Object.create(CallableClass.prototype);

MacroClass.prototype.evaluate = 
    afterevaluatearg(CallableClass.prototype.evaluate);

MacroClass.prototype.expand = 
    afterexpandarg(CallableClass.prototype.evaluate);

PrimitiveMacroClass.prototype = 
    Object.create(MacroClass.prototype);

UserMacroClass.prototype = 
    Object.create(MacroClass.prototype);

UserMacroClass.prototype.onevaluate = 
    UserFunctionClass.prototype.onevaluate;

// symbol family class
//     <- atom class

function SymbolFamilyClass (){}

SymbolFamilyClass.prototype = 
    Object.create(AtomClass.prototype);

SymbolFamilyClass.prototype.getvalue = function (){throw new Error("getvalue was not defined.");};
SymbolFamilyClass.prototype.getfunc = function (){throw new Error("getfunc was not defined.");};
SymbolFamilyClass.prototype.setvalue = function (){throw new Error("setvalue was not defined.");};
SymbolFamilyClass.prototype.setfunc = function (){throw new Error("setfunc was not defined.");};

// symbol class
//     <- symbol family class

function SymbolClass (name, value, func){
    this.name = name ? name.copy() : null;
    this.value = value || null;
    this.func = func || null;   
}

SymbolClass.prototype = 
    Object.create(SymbolFamilyClass.prototype);

SymbolClass.prototype.toString = function (){
    return this.name.toPlain();
};

SymbolClass.prototype.getvalue = function (){
    if (this.value == null)
        throw new Error("symbol " + this + " has no value.");
    return this.value;
};

SymbolClass.prototype.getfunc = function (){
    if (this.func == null)
        throw new Error("symbol " + this + " has no func.");
    return this.func;
};

// SymbolClass.prototype.getvaluee = function (){
//     if (this.getvalue() == null) throw new Error("symbol " + this + " has no value.");
//     return this.getvalue();
// };

// SymbolClass.prototype.getfunce = function (){
//     if (this.getfunc() == null) throw new Error("symbol " + this + " has no func.");
//     return this.getfunc();
// };

SymbolClass.prototype.setvalue = function (value){
    this.value = value;
    return value;
};

SymbolClass.prototype.setfunc = function (func){
    this.func = func;
    return func;
};

SymbolClass.prototype.onevaluate = function (){
    var func = this.getfunce();
    return func.evaluate.apply(func, arguments);
};

SymbolClass.prototype.onevaluatearg = function (){
    return this.getvalue();
};

SymbolClass.prototype.onexpand = function (){
    var func = this.getfunc();
    return func.expand.apply(func, arguments);
};

SymbolClass.prototype.onexpandarg = function (){
    return new Expanded(this.toString());
};

// intern symbol class
//     <- symbol family class

function InternSymbolClass (name){
    this.name = name ? name.copy() : null;
}

InternSymbolClass.prototype = 
    Object.create(SymbolFamilyClass.prototype);

InternSymbolClass.prototype.toString = function (){
    return this.name.toPlain();
};

InternSymbolClass.prototype.getvalue = function (){
    return inp.scope.finde(this.name).getvalue();
};

InternSymbolClass.prototype.getfunc = function (){
    return inp.scope.finde(this.name).getfunc();
};

InternSymbolClass.prototype.setvalue = function (value){
    return inp.scope.finde(this.name).setvalue(value);
};

InternSymbolClass.prototype.setfunc = function (func){
    return inp.scope.finde(this.name).setfunc(func);
};

InternSymbolClass.prototype.onevaluate = function (){
    var func = inp.scope.finde(this.name);
    return func.evaluate.apply(func, arguments);
};

InternSymbolClass.prototype.onexpand = function (){
    var func = inp.scope.finde(this.name);
    return func.expand.apply(func, arguments);
};

InternSymbolClass.prototype.onevaluatearg = function (){
    var func = inp.scope.finde(this.name);
    return func.evaluatearg.apply(func, arguments);
};

InternSymbolClass.prototype.onexpandarg = function (){
    var func = inp.scope.finde(this.name);
    return func.expandarg.apply(func, arguments);
};

InternSymbolClass.prototype.getvaluename = function (){
    return inp.scope.finde(this.name).getvaluename();
};

InternSymbolClass.prototype.getfuncname = function (){
    return inp.scope.finde(this.name).getfuncname();
};

function makeintern (name){
    return new InternSymbolClass(makestring(name));
};

// interned symbol class 

var interneds = [];

function InternSymbolClass2 (name){
    var index;
    for (index = 0; index < interneds.length; index++)
        if (interneds[index].name.toString() == name.toString())
            return interneds[index];
    InternSymbolClass.apply(this, arguments);
};

InternSymbolClass2.prototype = 
    Object.create(InternSymbolClass);

function makeintern2 (name){
    return new InternSymbolClass2(makestring(name));
};

// variable symbol class
//     <- symbol class

function VariableSymbolClass (name, value, func){
    this.name = name ? name.copy() : null;
    this.value = value || null;
    this.func = func || null;
    this.valuename = null;
    this.funcname = null;
};

VariableSymbolClass.prototype = 
    Object.create(SymbolClass.prototype);

VariableSymbolClass.prototype.toString = function (){
    return this.getvaluename();
};

VariableSymbolClass.prototype.toString = function (){
    return this.getvaluename() + "/*--" + this.name + "--*/";
};

VariableSymbolClass.prototype.getvaluename = function (){
    if (this.valuename == null)
        this.valuename = inp.namegen.generate();
    return this.valuename;
};

VariableSymbolClass.prototype.getfuncname = function (){
    if (this.funcname == null)
        this.funcname = inp.namegen.generate();
    return this.funcname;
};

VariableSymbolClass.prototype.onexpandarg = function (){
    return new Expanded(this.getvaluename());
};

VariableSymbolClass.prototype.onexpand = function (){
    var func = this.getfunc();
    if (func == null || func instanceof UserFunctionClass)
        return new Expanded(
            this.getfuncname() + "(" +
                slice(arguments).map(expandarg).join(",") + ")");
    return func.expand.apply(func, arguments);
};

function getvaluename (some){
    if (some instanceof SymbolFamilyClass == false)
        throw new Error("some is not symbol family class.");
    return some.getvaluename();
}

function  getfuncname (some){
    if (some instanceof SymbolFamilyClass == false)
        throw new Error("some is not symbol family class.");
    return some.getfuncname();
}

function makevar (name, value, func){
    return new VariableSymbolClass(makestring(name), value, func);
};

// stream class
//     <- atom class

function StreamClass (direction){
    this.direction = direction;
}

StreamClass.prototype = 
    Object.create(AtomClass.prototype);

StreamClass.direction = {};
StreamClass.direction.input = (1|0);
StreamClass.direction.output = (2|0);

StreamClass.onevaluate = null;
StreamClass.onexpand = null;
StreamClass.onexpandarg = null;
StreamClass.onexpanddata = null;

StreamClass.prototype.iseof = function (){throw new Error("iseof was not defined.");};
StreamClass.prototype.isalive = function (){throw new Error("isalive was not defined.");};
StreamClass.prototype.look = function (){throw new Error("look was not defined.");};
StreamClass.prototype.get = function (){throw new Error("get was not defined.");};
StreamClass.prototype.put = function (){throw new Error("put was not defined.");};

StreamClass.prototype.isin = function (){
    return new Boolean(this.direction & StreamClass.direction.input);
};

StreamClass.prototype.isout = function (){
    return new Boolean(this.direction & StreamClass.direction.output);
};

StreamClass.prototype.shouldin = function (){
    if (this.isin() == false) throw new Error("stream should input direction!");
};

StreamClass.prototype.shouldout = function (){
    if (this.isout() == false) throw new Error("stream should output direction!");
};

// string stream class 
//     <- stream class

function StringStreamClass (direction, source){
    this.direction = direction;
    this.source = source;
    this.sourceout = new StringClass();
    this.index = 0;
}

StringStreamClass.prototype = 
    Object.create(StreamClass.prototype);

StringStreamClass.prototype.iseof = function (){
    this.shouldin();
    return this.source.length() <= this.index;
};

StringStreamClass.prototype.isalive = function (){
    this.shouldin();
    return this.iseof() == false;
};

StringStreamClass.prototype.look = function (){
    this.shouldin();
    return this.iseof() ? nilf : this.source.nth(this.index);
};

StringStreamClass.prototype.get = function (){
    this.shouldin();
    return this.iseof() ? nilf : this.source.nth(this.index++);
};

StringStreamClass.prototype.put = function (charInstance){
    this.shouldout();
    return this.source.push(charInstance);
};

// obarray class 
//     <- native, function class

function Obarray (){
    this.obarray = {};
}

Obarray.prototype.find = function (name){
    return this.obarray[name.toPlain()] || null;
};

Obarray.prototype.set = function (name, sym){
    this.obarray[name.toPlain()] = sym;
    return sym;
};

Obarray.prototype.intern = function (name){
    var found;
    if ((found = this.find(name)))
        return found;
    var sym = new VariableSymbolClass(name);
    return this.set(name, sym);
};

Obarray.prototype.list = function (){
    var sequence, names, index;
    for (sequence = [], names = Object.keys(this.obarray), 
         index = 0; index < names.length; index++)
        sequence.push(this.obarray[names[index]]);
    return sequence;
};

function list (some){
    if (some.list == undefined)
        throw new Error("some has not method name of list.");
    return some.list();
};

function listin (some){
    if (some.listin == undefined)
        throw new Error("some has not method name of listin.");
    return some.listin();
};

// obarrays class
//     <- native, function class

function Obarrays (parent){
    this.obarray = new Obarray();
    this.parent = parent || null;
};

Obarrays.prototype.length = function (){ // ** for debug
    var count, current;
    for (count = 0, current = this; current; current = current.parent, count++);
    return count;
};

Obarrays.prototype.list = function (){
    var sequence, current;
    for (sequence = [], current = this; current; current = current.parent)
        sequence = sequence.concat(current.obarray.list());
    return sequence;
};

Obarrays.prototype.find = function (name){
    var current, found;
    for (current = this; current; current = current.parent)
        if ((found = current.obarray.find(name)))
            return found;
    return null;
};

Obarrays.prototype.finde = function (name){
    var found;
    if ((found = this.find(name)) == null)
        throw new Error("obarrays " + name + " was not found.");
    return found;
};

Obarrays.prototype.intern = function (name){
    var found;
    if ((found = this.find(name))) 
        return found;
    return this.obarray.intern(name);
};

Obarrays.prototype.internf = function (name){
    return this.obarray.intern(name);
};

Obarrays.prototype.nest = function (){
    return new Obarrays(this);
};

Obarrays.prototype.exit = function (){
    return this.parent;
};

// obscope class
//   <- native, function class

function Obscope (parent){
    this.obarray = new Obarrays(null);
    this.obarrays = new Array();
    this.parent = parent || null;
};

Obscope.prototype.length = function (){ // ** debug
    var count, current;
    for (count = 0, current = this; current; current = current.parent)
        count += current.obarray.length();
    return count;
};

Obscope.prototype.list = function (){
    var sequence, current;
    for (sequence = [], current = this; current; current = current.parent)
        sequence = sequence.concat(current.listin());
    return sequence;
};

Obscope.prototype.listin = function (){
    return this.obarray.list();
};

Obscope.prototype.find = function (name){
    var found, current;
    for (current = this; current; current = current.parent)
        if ((found = current.obarray.find(name)))
            return found;
    return null;
};

Obscope.prototype.finde = function (name){
    var found = this.find(name);
    if (found) return found;
    throw new Error("obscope " + name + " was not found.");
};

Obscope.prototype.intern = function (name){
    var found = this.find(name);
    if (found) return found;
    return this.obarray.intern(name);
};

Obscope.prototype.internf = function (name){
    this.obarray.internf(name);
    return this.obarray.internf(name);
};

Obscope.prototype.nestin = function (){
    this.obarray = this.obarray.nest();
};

Obscope.prototype.exitin = function (){
    this.obarrays.push(this.obarray);
    this.obarray = this.obarray.exit();
};

Obscope.prototype.nest = function (){
    return new Obscope(this);
};

Obscope.prototype.exit = function (){
    return this.parent;
};

// interneds class
//     <- native, function class ** save for memory space

function Interneds (){
    this.interneds = {};
}

Interneds.prototype.add = function (name){
    if (this.interneds[name.toPlain()])
        return this.interneds[name.toPlain()];
    this.interneds[name.toPlain()] = new InternSymbolClass(name);
    return this.interneds[name.toPlain()];
};
        
// readerscope class
//     <- native, function class

function ReaderScope (method){
    this.scope = {};
    this.method = method || null;
}

ReaderScope.prototype.get = function (charInstance){
    return this.scope[charInstance.toString()] || null;
};

ReaderScope.prototype.getcurrent = function (){
    return this.method || null;
};

ReaderScope.prototype.dig = function (charInstance){
    if(this.scope[charInstance.toString()] == null)
        this.scope[charInstance.toString()] = new ReaderScope();
    return this.scope[charInstance.toString()];
};

ReaderScope.prototype.set = function (charInstance, method){
    return this.dig(charInstance).setcurrent(method);
};

ReaderScope.prototype.setcurrent = function (method){
    return this.method = method;
};

// interpreter class
//     <- native, function class

function Interpreter (){
    this.scope = new Obscope();
    this.scoperoot = this.scope;
    this.interneds = new Interneds();
    this.readerscope = new ReaderScope();
    this.namegen = new NameGenerator("abcdefghijklmnopqrstuvwxyz");
};

Interpreter.prototype.nestin = function (){
    inp.scope.nestin();
    return null;
};

Interpreter.prototype.exitin = function (){
    inp.scope.exitin();
    return null;
};

Interpreter.prototype.nest = function (){
    this.scope = this.scope.nest();
    return null;
};

Interpreter.prototype.exit  =function (){
    this.scope = this.scope.exit();
    return null;
};

var inp = new Interpreter();

// define primitive values

inp.scope.intern(makestring("nil")).setvalue(nil);
inp.scope.intern(makestring("t")).setvalue(t);

// define reader methods

var rdignoreindent = new PrimitiveFunctionClass();
var rdread = new PrimitiveFunctionClass();
var rdreadintern = new PrimitiveFunctionClass();
var rdreadminus = new PrimitiveFunctionClass();
var rdreadnumber = new PrimitiveFunctionClass();
var rdreadstring = new PrimitiveFunctionClass();
var rdreadopenbrace = new PrimitiveFunctionClass();
var rdreadclosebrace = new PrimitiveFunctionClass();
var rdreadclosebrace_unique = new Unique();
var rdreadquote = new PrimitiveFunctionClass();
var rdreadunquote = new PrimitiveFunctionClass();
var rdreadunquoteat = new PrimitiveFunctionClass();
var rdreadchar = new PrimitiveFunctionClass();
var rdreadpre  = new PrimitiveFunctionClass();
var rdreadnative = new PrimitiveFunctionClass();

rdignoreindent.onevaluate = function (stream){
    while (stream.isalive() && 
           stream.look().get().value == " ".charCodeAt() ||
           stream.look().get().value == "\t".charCodeAt())
        stream.get();
    return nil;
};

rdread.onevaluate = function (stream){
    rdignoreindent.evaluate(stream);
    var rdscope, rdscopec;
    var charInstance, charInstancec;
    for (rdscope = inp.readerscope, charInstance = nil; stream.isalive();){
        charInstancec = stream.look();
        rdscopec = rdscope.get(charInstancec);
        if (rdscopec == null) break;
        charInstance = stream.get();
        rdscope = rdscopec;
    }
    if (rdscope.getcurrent() == null)
        throw new Error("method was not found.");
    return rdscope.getcurrent().evaluate(stream, charInstance);
};

rdreadintern.onevaluate = function (stream){
    rdignoreindent.evaluate(stream);
    var name;
    for (name = new StringClass(); stream.isalive();)
        if (inp.readerscope.get(stream.look()) || 
            stream.look().get().value == (" ".charCodeAt()) || 
            stream.look().get().value == ("\t".charCodeAt())) break;
        else name.push(stream.get());
    if (name.length() == 0)
        return nil;
    inp.scoperoot.intern(name);
    // return new InternSymbolClass(name);
    return inp.interneds.add(name);
};

rdreadminus.onevaluate = function (stream){
    var num = rdread.evaluate(stream);
    num.value *= -1;
    return num;
};

rdreadnumber.onevaluate = function (stream, nc){
    var charInstance, num;
    for (num = nc.toString(); stream.isalive();)
        if ("123456890.".indexOf(stream.look().toString()) >= 0)
            num += stream.get().toString();
        else break;
    return num.indexOf(".") >= 0 ?
        new FloatClass(parseFloat(num)).constant():
        new IntClass(parseInt(num)).constant();
};

rdreadstring.onevaluate = function (stream, quote){
    var charInstance, content;
    for (content = new StringClass(); 
         stream.isalive() && (charInstance = stream.get()).status();)
        if (charInstance.get().value == quote.value) break;
        else  content.push(charInstance);
    return content.constant();
};

rdreadopenbrace.onevaluate = function (stream){
    var ncons, element;
    for (ncons = nil; stream.isalive() && 
         (element = rdread.evaluate(stream)) != rdreadclosebrace_unique;)
        ncons = new ConsClass(element, ncons);
    return ncons.reverse().constant();
};

rdreadclosebrace.onevaluate = function (stream){
    return rdreadclosebrace_unique;
};

rdreadquote.onevaluate = function (stream){
    return new QuoteClass(rdread.evaluate(stream));
};

rdreadunquote.onevaluate = function (stream){
    return new UnQuoteClass(rdread.evaluate(stream));
};

rdreadunquoteat.onevaluate = function (stream){
    return new UnQuoteAtClass(rdread.evaluate(stream));
};

rdreadchar.onevaluate = function (stream){
    return new CharClass(stream.get());
};

rdreadpre.onevaluate = function (stream){
    var formula = rdread.evaluate(stream);
    return formula.evaluatearg();
};

rdreadnative.onevaluate = function (stream){
    var sym = rdread.evaluate(stream);
    return new Expanded(sym.name.toPlain());
};

inp.readerscope.setcurrent(rdreadintern);
inp.readerscope.dig("'").setcurrent(rdreadquote);
inp.readerscope.dig(",").setcurrent(rdreadunquote);
inp.readerscope.dig(",").dig("@").setcurrent(rdreadunquoteat);
inp.readerscope.dig("-").setcurrent(rdreadminus);
inp.readerscope.dig('"').setcurrent(rdreadstring);
inp.readerscope.dig("(").setcurrent(rdreadopenbrace);
inp.readerscope.dig(")").setcurrent(rdreadclosebrace);
inp.readerscope.dig("0").setcurrent(rdreadnumber);
inp.readerscope.dig("1").setcurrent(rdreadnumber);
inp.readerscope.dig("2").setcurrent(rdreadnumber);
inp.readerscope.dig("3").setcurrent(rdreadnumber);
inp.readerscope.dig("4").setcurrent(rdreadnumber);
inp.readerscope.dig("5").setcurrent(rdreadnumber);
inp.readerscope.dig("6").setcurrent(rdreadnumber);
inp.readerscope.dig("7").setcurrent(rdreadnumber);
inp.readerscope.dig("8").setcurrent(rdreadnumber);
inp.readerscope.dig("9").setcurrent(rdreadnumber);
inp.readerscope.dig("?").setcurrent(rdreadchar);
inp.readerscope.dig("@").dig(".").setcurrent(rdreadpre);
inp.readerscope.dig("@").dig("@").setcurrent(rdreadnative);

// define syntax functions

var synif = new SpecialFunctionClass();
var synblock = new SpecialFunctionClass();
var synblock_func = new SpecialFunctionClass();
var synprogn = new SpecialFunctionClass();
var synprogn_func = new SpecialFunctionClass();
var synprogn_source = new SpecialFunctionClass();
var synand = new SpecialFunctionClass();
var synor = new SpecialFunctionClass();
var synnot = new SpecialFunctionClass();
var synsetf = new SpecialFunctionClass();
var synlocal = new SpecialFunctionClass();
var synglobal = new SpecialFunctionClass();

inp.scope.intern(makestring("if")).setfunc(synif);
inp.scope.intern(makestring("block")).setfunc(synblock);
inp.scope.intern(makestring("progn")).setfunc(synprogn);
inp.scope.intern(makestring("and")).setfunc(synand);
inp.scope.intern(makestring("or")).setfunc(synor);
inp.scope.intern(makestring("not")).setfunc(synnot);
inp.scope.intern(makestring("setf")).setfunc(synsetf);

synif.label = "<#syntax if>";
synblock.label = "<#syntax block>";
synblock_func.label = "<#syntax block func>";
synprogn.label = "<#syntax progn>";
synprogn_func.label = "<#syntax progn func>";
synprogn_source.label = "<#syntax progn source>";
synand.label = "<#syntax and>";
synor.label = "<#syntax or>";
synnot.label = "<#syntax not>";
synsetf.label = "<#syntax setf>";

synif.onevaluate = function (cond, truecase, falsecase){
    if (cond.evaluatearg().status())
        return truecase.evaluatearg();
    return falsecase.evaluatearg();
};

synif.onexpand = function (cond, truecase, falsecase){
    return new Expanded(
        "(" + cond.expandarg().unpack() + "?" +
            truecase.expandarg().unpack() + ":" + 
            falsecase.expandarg().unpack() + ")");
};
        
synblock.onevaluate = function (){
    inp.nestin();
    var temp = synprogn.evaluate.apply(synprogn, arguments);
    inp.exitin();
    return temp;
};

synblock.onexpand = function (){
    inp.nestin();
    var temp = synprogn.expand.apply(synprogn, arguments);
    inp.exitin();
    return temp;
};

synblock_func.onevaluate = function (){
    inp.nest();
    var temp = synprogn_func.evaluate.apply(synprogn_func, arguments);
    inp.exit();
    return temp;
};

synblock_func.onexpand = function (){ // ** should think again
    inp.nest();
    var temp = synprogn_func.expand.apply(synprogn_func, arguments);
    var variables = [].concat.apply([], inp.scope.obarrays.map(list));
    inp.exit();
    return new Expanded(
        (variables.length == 0 ? "" :
         ("var " + variables.map(getvaluename).join(",") + ";")) +
            temp);
};

synprogn.onevaluate = function (){
    var res, index;
    for (res = nil, index = 0; index < arguments.length; index++)
        res = arguments[index].evaluatearg();
    return res;
};

synprogn.onexpand = function (){
    var sum, index;
    for (sum = "", index = 0; index < arguments.length; index++)
        sum += (index ? "," : "") + arguments[index].expandarg().unpack();
    return new Expanded("(" + sum + ")");
};

synprogn_func.onevaluate = 
    synprogn.onevaluate;

synprogn_func.onexpand = function (){
    var sum, index;
    for (sum = "", index = 0; index < (arguments.length -1); index++)
        sum += arguments[index].expandarg().unpack() + ";";
    sum += "return " + arguments[index].expandarg().unpack() + ";";
    return new Expanded(sum);
};

synprogn_source.onevaluate = 
    synprogn.onevaluate;

synprogn_source.onexpand = function (){
    var sum, index;
    for (sum = "", index = 0; index < arguments.length; index++)
        sum += arguments[index].expandarg().unpack() + ";";
    return new Expanded(sum);
};

synand.onevaluate = function (){
    var res, index;
    for (res = t, index = 0; index < arguments.length; index++){
        if ((res  = arguments[index].evaluatearg()) == nil)
            return nil;}
    return res;
};

synand.onexpand = function (){
    return new Expanded("(" + slice(arguments).map(expandarg).map(unpack).join("&&") + ")");
};

synor.onevaluate = function (){
    var res, index;
    for (res = nil, index = 0; index < arguments.length; index++)
        if ((res = arguments[index].evaluatearg()))
            return res;
    return nil;
};

synor.onexpand = function (){
    return new Expanded("(" + slice(arguments).map(expandarg).map(unpack).join("||") + ")");
};

synnot.onevaluate = function (some){
    return some.evaluatearg().status() ? nil : t;
};

synnot.onexpand = function (some){
    return new Expanded("(!" + some.expandarg().unpack() + ")");
};

synsetf.onevaluate = function (formula, value){
    // return formula.evaluatearg().set(value.evaluatearg());
    // var valued = value.evaluatearg();
    // var formulaed = formula.evaluatearg();
    // return formulaed.set(valued);
    return formula.evaluatearg().set(value.evaluatearg());
};

synsetf.onexpand = function (formula, value){
    var formulad = formula.evaluatearg();
    var valued = value.evaluatearg();
    formulad.set(valued);
    return new Expanded(
        formulad.expandarg() + "=" +
            valued.expandarg());
};

// define basic macro functions

var macprog1 = new PrimitiveMacroClass();
var macwhen = new PrimitiveMacroClass();
var macunless = new PrimitiveMacroClass();
var maclambda = new PrimitiveMacroClass();
var macdefun = new PrimitiveMacroClass();
var macmacro = new PrimitiveMacroClass();
var macdefmacro = new PrimitiveMacroClass();
var macsetq = new PrimitiveMacroClass();
var macincf = new PrimitiveMacroClass();
var macdecf = new PrimitiveMacroClass();
var maclet = new PrimitiveMacroClass();
var macflet = new PrimitiveMacroClass();
var macmlet = new PrimitiveMacroClass();
var macdefvar = new PrimitiveMacroClass();
var macdeflvar = new PrimitiveMacroClass();

inp.scope.intern(makestring("prog1")).setfunc(macprog1);
inp.scope.intern(makestring("when")).setfunc(macwhen);
inp.scope.intern(makestring("unless")).setfunc(macunless);
inp.scope.intern(makestring("lambda")).setfunc(maclambda);
inp.scope.intern(makestring("defun")).setfunc(macdefun);
inp.scope.intern(makestring("macro")).setfunc(macmacro);
inp.scope.intern(makestring("defmacro")).setfunc(macdefmacro);
inp.scope.intern(makestring("setq")).setfunc(macsetq);
inp.scope.intern(makestring("incf")).setfunc(macincf);
inp.scope.intern(makestring("decf")).setfunc(macdecf);
inp.scope.intern(makestring("let")).setfunc(maclet);
inp.scope.intern(makestring("flet")).setfunc(macflet);
inp.scope.intern(makestring("mlet")).setfunc(macmlet);
inp.scope.intern(makestring("defvar")).setfunc(macdefvar);
inp.scope.intern(makestring("deflvar")).setfunc(macdeflvar);

macprog1.onevaluate = function (n){
    var sym = makevar("");
    return makecons(maclet,
                    makecons(
                        makecons(
                            makecons(sym,
                                     makecons(n))),
                        makecons(
                            makecons(synprogn,
                                     makecons(
                                         makecons(synprogn,
                                                  ConsClass.toCons(slice(arguments, 1))),
                                         makecons(sym))))));
};

macincf.onevaluate = function (formula){
    return new ConsClass(synsetf,
                         new ConsClass(formula,
                                       new ConsClass(
                                           new ConsClass(basadd, 
                                                         new ConsClass(formula,
                                                                       new ConsClass(
                                                                           new IntClass(1)))))));
};

macdecf.onevaluate = function (formula){
    return new ConsClass(synsetf,
                         new ConsClass(formula,
                                       new ConsClass(
                                           new ConsClass(bassub,
                                                         new ConsClass(formula,
                                                                       new ConsClass(
                                                                           new IntClass(1)))))));
};

macdefvar.onevaluate = function (sym, value){
    return new ConsClass(synsetf,
                         new ConsClass(
                             new ConsClass(bassymbolvalue,
                                           new ConsClass(
                                               new ConsClass(basglobal,
                                                             new ConsClass(
                                                                 new QuoteClass(sym))))),
                             new ConsClass(value)));
}

macdeflvar.onevaluate = function (sym, value){
    return new ConsClass(synsetf,
                         new ConsClass(
                             new ConsClass(bassymbolvalue,
                                           new ConsClass(
                                               new ConsClass(baslocal,
                                                             new ConsClass(
                                                                 new QuoteClass(sym))))),
                             new ConsClass(value)));
};

macwhen.onevaluate = function (cond){
    return new ConsClass(synif,
                         new ConsClass(cond,
                                       new ConsClass(
                                           new ConsClass(synprogn, 
                                                         ConsClass.toCons(slice(arguments, 1))),
                                           new ConsClass(nil))));
};

macunless.onevaluate = function (cond){
    return new ConsClass(synif,
                         new ConsClass(cond,
                                       new ConsClass(nil,
                                                     new ConsClass(
                                                         new ConsClass(synprogn,
                                                                       ConsClass.toCons(slice(arguments, 1)))))));
};

maclambda.onevaluate = function (args){
    // return new UserFunctionClass(null, args, slice(arguments, 1));
    // return new UserFunctionClass(null, args, ConsClass.toCons(slice(arguments, 1)));
    return new UserFunctionClass(args, ConsClass.toCons(slice(arguments, 1)));
};

macdefun.onevaluate = function (name){
    return new ConsClass(synsetf,
                         new ConsClass(
                             new QuoteClass(
                                 new SymbolFunctionReferenceClass(name)),
                             new ConsClass(
                                 new ConsClass(maclambda,
                                               ConsClass.toCons(slice(arguments, 1))))));
};

macmacro.onevaluate = function (args){
    // return new UserMacroClass(null, args, slice(arguments, 1));
    // return new UserMacroClass(null, args, ConsClass.toCons(slice(arguments, 1)));
    return new UserMacroClass(args, ConsClass.toCons(slice(arguments, 1)));
};

macdefmacro.onevaluate = function (name){
    return new ConsClass(synsetf,
                     new ConsClass(
                         new QuoteClass(
                             new SymbolFunctionReferenceClass(name)),
                         new ConsClass(
                             new ConsClass(macmacro,
                                           ConsClass.toCons(slice(arguments, 1))))));
};

macsetq.onevaluate = function (sym, value){
    return new ConsClass(synsetf,
                         new ConsClass(
                             new QuoteClass(
                                 new SymbolValueReferenceClass(sym)),
                             new ConsClass(value)));
};

maclet.onevaluate = function (binds){ // ** must update here.
    var ncons, bindsi;
    for (ncons = new ConsClass(synprogn), bindsi = binds.iter(); bindsi.isalive();)
        ncons = new ConsClass(
            new ConsClass(macdeflvar, bindsi.next()), ncons);
    ncons = ncons.reverse();
    return new ConsClass(synblock,
                         new ConsClass(ncons,
                                       new ConsClass(
                                           new ConsClass(synprogn,
                                                         ConsClass.toCons(slice(arguments, 1))))));
};

macflet.onevaluate = function (binds){ // ** must update here
    var ncons, bindsi, bind;
    for (ncons = new ConsClass(synprogn), bindsi = binds.iter(); bindsi.isalive();){
        bind = bindsi.next();
        ncons = new ConsClass(
            new ConsClass(synsetf,
                          new ConsClass(
                              new ConsClass(bassymbolfunction,
                                            new ConsClass(
                                                new QuoteClass(bind.car))),
                              new ConsClass(
                                  new ConsClass(maclambda, bind.cdr)))), ncons);}
    ncons = ncons.reverse();
    return new ConsClass(synblock,
                         new ConsClass(ncons,
                                       new ConsClass(
                                           new ConsClass(synprogn, 
                                                         ConsClass.toCons(slice(arguments, 1))))));
};

macmlet.onevaluate = function (binds){ // ** must update here.
    var ncons, bindsi, bind;
    for (ncons = new ConsClass(synprogn), bindsi = binds.iter(); bindsi.isalive();){
        bind = bindsi.next();
        ncons = new ConsClass(
            new ConsClass(synsetf,
                          new ConsClass(
                              new ConsClass(bassymbolfunction,
                                            new ConsClass(
                                                new QuoteClass(bind.car))),
                              new ConsClass(
                                  new ConsClass(macmacro, bind.cdr)))), ncons);}
    ncons = ncons.reverse();
    return new ConsClass(synblock,
                         new ConsClass(ncons,
                                       new ConsClass(
                                           new ConsClass(synprogn, 
                                                         ConsClass.toCons(slice(arguments, 1))))));
};

// define basic functions

var basadd = new PrimitiveFunctionClass();
var bassub = new PrimitiveFunctionClass();
var basmul = new PrimitiveFunctionClass();
var basdiv = new PrimitiveFunctionClass();
var basmod = new PrimitiveFunctionClass();
var basconcat = new PrimitiveFunctionClass();
var baslist = new PrimitiveFunctionClass();
var bascar = new PrimitiveFunctionClass();
var bascdr = new PrimitiveFunctionClass();
var bascons = new PrimitiveFunctionClass();
var basevery = new PrimitiveFunctionClass();
var basmap = new PrimitiveFunctionClass();
var basfilter = new PrimitiveFunctionClass();
var basreduce = new PrimitiveFunctionClass();
var basfindif = new PrimitiveFunctionClass();
var baspositionif = new PrimitiveFunctionClass();
var bascopy = new PrimitiveFunctionClass();
var basreverse = new PrimitiveFunctionClass();
var basnreverse = new PrimitiveFunctionClass();
var basslice = new PrimitiveFunctionClass();
var basnth = new PrimitiveFunctionClass();
var baslength = new PrimitiveFunctionClass();
var basnull = new PrimitiveFunctionClass();
var basreadchar = new PrimitiveFunctionClass();
var basreadline = new PrimitiveFunctionClass();
var bassymbolname = new PrimitiveFunctionClass();
var bassymbolvalue = new PrimitiveFunctionClass();
var bassymbolfunction = new PrimitiveFunctionClass();
var basintern = new PrimitiveFunctionClass();
var basmakesymbol = new PrimitiveFunctionClass();
var basfuncall = new PrimitiveFunctionClass();
var basapply = new PrimitiveFunctionClass();
var basglobal = new PrimitiveFunctionClass();
var baslocal = new PrimitiveFunctionClass();

inp.scope.intern(makestring("concat")).setfunc(basconcat);
inp.scope.intern(makestring("+")).setfunc(basadd);
inp.scope.intern(makestring("-")).setfunc(bassub);
inp.scope.intern(makestring("*")).setfunc(basmul);
inp.scope.intern(makestring("/")).setfunc(basdiv);
inp.scope.intern(makestring("%")).setfunc(basmod);
inp.scope.intern(makestring("car")).setfunc(bascar);
inp.scope.intern(makestring("cdr")).setfunc(bascdr);
inp.scope.intern(makestring("list")).setfunc(baslist);
inp.scope.intern(makestring("cons")).setfunc(bascons);
inp.scope.intern(makestring("every")).setfunc(basevery);
inp.scope.intern(makestring("map")).setfunc(basmap);
inp.scope.intern(makestring("filter")).setfunc(basfilter);
inp.scope.intern(makestring("reduce")).setfunc(basreduce);
inp.scope.intern(makestring("find-if")).setfunc(basfindif);
inp.scope.intern(makestring("position-if")).setfunc(baspositionif);
inp.scope.intern(makestring("copy")).setfunc(bascopy);
inp.scope.intern(makestring("reverse")).setfunc(basreverse);
inp.scope.intern(makestring("nreverse")).setfunc(basnreverse);
inp.scope.intern(makestring("slice")).setfunc(basslice);
inp.scope.intern(makestring("nth")).setfunc(basnth);
inp.scope.intern(makestring("length")).setfunc(baslength);
inp.scope.intern(makestring("null")).setfunc(basnull);
inp.scope.intern(makestring("read-char")).setfunc(basreadchar);
inp.scope.intern(makestring("read")).setfunc(rdread);
inp.scope.intern(makestring("symbol-name")).setfunc(bassymbolname);
inp.scope.intern(makestring("symbol-value")).setfunc(bassymbolvalue);
inp.scope.intern(makestring("symbol-function")).setfunc(bassymbolfunction);
inp.scope.intern(makestring("intern")).setfunc(basintern);
inp.scope.intern(makestring("make-symbol")).setfunc(basmakesymbol);
inp.scope.intern(makestring("funcall")).setfunc(basfuncall);
inp.scope.intern(makestring("apply")).setfunc(basapply);
inp.scope.intern(makestring("global")).setfunc(basglobal);
inp.scope.intern(makestring("local")).setfunc(baslocal);

basadd.label = "<#primitive +>";
bassub.label = "<#primitive ->";
basmul.label = "<#primitive *>";
basdiv.label = "<#primitive />";
basmod.label = "<#primitive %>";
basconcat.label = "<#primitive concat>";
baslist.label = "<#primitive list>";
bascar.label = "<#primitive car>";
bascdr.label = "<#primitive cdr>";
bascons.label = "<#primitive cons>";
basevery.label = "<#primitive every>";
basmap.label = "<#primitive map>";
basfilter.label = "<#primitive filter>";
basreduce.label = "<#primitive reduce>";
basfindif.label = "<#primitive findif>";
baspositionif.label = "<#primitive positionif>";
bascopy.label = "<#primitive copy>";
basreverse.label = "<#primitive reverse>";
basnreverse.label = "<#primitive nreverse>";
basslice.label = "<#primitive slice>";
basnth.label = "<#primitive nth>";
baslength.label = "<#primitive length>";
basnull.label = "<#primitive null>";
basreadchar.label = "<#primitive readchar>";
basreadline.label = "<#primitive readline>";
bassymbolname.label = "<#primitive symbolname>";
bassymbolvalue.label = "<#primitive symbolvalue>";
bassymbolfunction.label = "<#primitive symbolfunction>";
basintern.label = "<#primitive intern>";
basmakesymbol.label = "<#primitive makesymbol>";
basfuncall.label = "<#primitive funcall>";
basapply.label = "<#primitive apply>";
basglobal.label = "<#primitive global>";
baslocal.label = "<#primitive local>";

basglobal.onevaluate = function (sym){
    if (sym instanceof InternSymbolClass)
        return inp.scoperoot.intern(sym.name);
    return sym;
};

baslocal.onevaluate = function (sym){
    if (sym instanceof InternSymbolClass)
        return inp.scope.internf(sym.name);
    return sym;
};

basadd.onevaluate = function (){
    return slice(arguments).reduce(function (a,b){
        if (a instanceof NumberClass == false) throw new Error("" + a + " a is not number class."); 
        if (b instanceof NumberClass == false) throw new Error("" + b + " b is not number class.");
        return a.add(b);});
};

bassub.onevaluate = function (){
    return slice(arguments).reduce(function (a,b){
        if (a instanceof NumberClass == false) throw new Error("" + a + " a is not number class."); 
        if (b instanceof NumberClass == false) throw new Error("" + b + " b is not number class."); 
        return a.sub(b);});
};

basmul.onevaluate = function (){
    return slice(arguments).reduce(function (a,b){
        if (a instanceof NumberClass == false) throw new Error("" + a + " a is not number class."); 
        if (b instanceof NumberClass == false) throw new Error("" + b + " b is not number class."); 
        return a.mul(b);});
};

basdiv.onevaluate = function (){
    return slice(arguments).reduce(function (a,b){
        if (a instanceof NumberClass == false) throw new Error("" + a + " a is not number class."); 
        if (b instanceof NumberClass == false) throw new Error("" + b + " b is not number class."); 
        return a.div(b);});
};

basmod.onevaluate = function (){
    return slice(arguments).reduce(function (a,b){
        if (a instanceof NumberClass == false) throw new Error("" + a + " a is not number class."); 
        if (b instanceof NumberClass == false) throw new Error("" + b + " b is not number class."); 
        return a.mod(b);});
};

basadd.onexpand = function (){
    return new Expanded("(" + slice(arguments).join("+") + ")");
};

bassub.onexpand = function (){
    return new Expanded("(" + slice(arguments).join("-") + ")");
};

basmul.onexpand = function (){
    return new Expanded("(" + slice(arguments).join("*") + ")");
};

basdiv.onexpand = function (){
    return new Expanded("(" + slice(arguments).join("/") + ")");
};

basmod.onexpmod = function (){
    return new Expanded("(" + slice(arguments).join("%") + ")");
};

basadd.onexpandarg = function (){
    return new Expanded("function(){Array.prototype.slice(arguments).reduce(function(a,b){return a+b;})");
};

bassub.onexpandarg = function (){
    return new Expanded("function(){Array.prototype.slice(arguments).reduce(function(a,b){return a-b;})");
};

basmul.onexpandarg = function (){
    return new Expanded("function(){Array.prototype.slice(arguments).reduce(function(a,b){return a*b;})");
};

basdiv.onexpandarg = function (){
    return new Expanded("function(){Array.prototype.slice(arguments).reduce(function(a,b){return a/b;})");
};

basmod.onexpandarg = function (){
    return new Expanded("function(){Array.prototype.slice(arguments).reduce(function(a,b){return a%b;})");
};

basconcat.onevaluate = function (){
    // var sum, index;
    // for (sum = [], index = 0; index < arguments.length; index++)
    //     sum = sum.concat(arguments[index].value);
    // return new StringClass(sum);
    return new StringClass(Array.prototype.concat.apply([], slice(arguments).map(value)));
};

basconcat.onexpand = function (){
    // var sum, index;
    // for (sum = "", index = 0; index < arguments.length; index++)
    //     sum += (index ? "+" : "") + arguments[index].unpack();
    // return new Expanded("(" + sum + ")");
    return new Expanded("(" + slice(arguments).map(expandarg).map(unpack).join("+") + ")");
};

bascar.onevaluate = function (cons){
    return cons.car;
};

bascar.onexpand = function (cons){ // ** must update here!
    return new Expanded(cons + "[0]");
};

bascdr.onevaluate = function (cons){
    return cons.cdr;
};

bascdr.onexpand = function (cons){ // ** must update here!
    return new Expanded(cons + ".slice(1)");
};

bascons.onevaluate = function (car, cdr){
    return new ConsClass(car, cdr);
};

bascons.onexpand = function (car, cdr){ // ** must update here!
    return new Expanded("[" + car + "].concat(" + cdr + ")");
};

baslist.onevaluate = function (){
    return new ConsClass.toCons(slice(arguments));
};

baslist.onexpand = function (){
    return new ConsClass.toCons(slice(arguments)).expanddata(); // ** should check again
};

basevery.onevaluate = function (func, sequence){
    return sequence.every(func);
};

basmap.onevaluate = function (func, sequence){
    return sequence.map(func);
};

basmap.onexpand = function (func, sequence){
    return new Expanded(sequence + ".map(" + func + ")");
};

basfilter.onevaluate = function (func, sequence){
    return sequence.filter(func);
};

basfilter.onexpand = function (func, sequence){
    return new Expanded(sequence + ".filter(" + func + ")");
};

basreduce.onevaluate = function (func, sequence){
    return sequence.reduce(func);
};

basreduce.onexpand = function (func, sequence){
    return new Expanded(sequence + ".reduce(" + func + ")");
};

basfindif.onevaluate = function (func, sequence){
    return sequence.findif(func);
};

basfindif.onexpand = function (func, sequence){
    throw "find-if is not defined yet.";
};

baspositionif.onevaluate = function (func, sequence){
    return sequence.positionif(func);
};

baspositionif.onexpand = function (func, sequence){
    throw "position-if is not defined yet.";
};

basslice.onevaluate = function (beginning, end,  sequence){
    return sequence.slice(beginning, end);
};

basslice.onexpand = function (beginning, end, sequence){
    return new Expanded(sequence + ".slice(" + beginning + "," + end + ")");
};

basnth.onevaluate = function (index, sequence){
    return sequence.nth(index.value);
};

basnth.onexpand = function (index, sequence){
    throw "nth is not defined yet.";
};

bascopy.onevaluate = function (sequence){
    return sequence.copy();
};

bascopy.onexpand = function (sequence){
    return new Expanded(sequence + ".slice()");
};

basreverse.onevaluate = function (sequence){
    return sequence.copy().reverse();
};

basreverse.onexpand = function (sequence){
    return new Expanded(sequence + ".slice().reverse()");
};

basnreverse.onevaluate = function (sequence){
    return sequence.reverse();
};

basnreverse.onexpand = function(sequence){
    return new Expanded(sequence + ".reverse()");
};

baslength.onevaluate = function (sequence){
    return new IntClass(sequence.length());
};

baslength.onexpand = function (sequence){
    return new Expanded(sequence + ".length");
};

basnull.onevaluate = function (sequence){
    return new BooleanClass(sequence == nil);
};

basnull.onexpand = function (sequence){
    return new Expanded("(" + sequence + ".length==0)");
};

basreadchar.onevaluate = function (stream){
    return stream.get();
};

bassymbolname.onevaluate = function (sym){
    return sym.name.copy();
};

bassymbolvalue.onevaluate = function (sym){
    return new SymbolValueReferenceClass(sym);
};

bassymbolfunction.onevaluate = function (sym){
    return new SymbolFunctionReferenceClass(sym);
};

basintern.onevaluate = function (name){
    inp.scope.intern(name);
    return new InternSymbolClass(name);
};

basmakesymbol.onevaluate = function (name){
    return new VariableSymbolClass(name);
};

basfuncall.onevaluate = function (func){
    return func.evaluate.apply(func, slice(arguments, 1));
};

basfuncall.onexpand = function (func){
    return new Expanded(func + "(" + slice(arguments, 1).join(",") + ")");
};

basapply.onevaluate = function (func, sequence){
    return func.evaluate.apply(func, sequence.toArray());
};

basapply.onexpand = function (func, sequence){
    return new Expanded(func + "(" + sequence.toArray().join(",") + ")");
};

// // define optimize function 

// var optconcat = new OptimizeFunctionClass();
//  var optadd = new OptimizeFunctionClass();
// var optsub = new OptimizeFunctionClass();
// var optmul = new OptimizeFunctionClass();
// var optdiv = new OptimizeFunctionClass();
// var optmod = new OptimizeFunctionClass();

// inp.scope.intern(makestring("concat")).setfunc(optconcat);
// inp.scope.intern(makestring("+")).setfunc(optadd);
// inp.scope.intern(makestring("-")).setfunc(optsub);
// inp.scope.intern(makestring("*")).setfunc(optmul);
// inp.scope.intern(makestring("/")).setfunc(optdiv);
// inp.scope.intern(makestring("%")).setfunc(optmod);

// optconcat.onevaluate = 
//     beforeevaluatearg(basconcat.onevaluate);

// optconcat.onexpand = function (){
//     if (isoptimizableall(arguments))
//         return basconcat.expand.apply(basconcat, arguments);
//     return basconcat.evaluate.apply(basconcat, arguments).constant();
// };

// optadd.onevaluate = 
//     beforeevaluatearg(basadd.onevaluate);

// optadd.onexpand = function (){
//     if (isoptimizableall(arguments))
//         return basconcat.expand.apply(basadd, arguments);
//     return basconcat.evaluate.apply(basadd, arguments).constant();
// };

// optsub.onevaluate = 
//     beforeevaluatearg(bassub.onevaluate);

// optsub.onexpand = function (){
//     if (isoptimizableall(arguments))
//         return basconcat.expand.apply(bassub, arguments);
//     return basconcat.evaluate.apply(bassub, arguments).constant();
// };

// optmul.onevaluate = 
//     beforeevaluatearg(basmul.onevaluate);

// optmul.onexpand = function (){
//     if (isoptimizableall(arguments))
//         return basconcat.expand.apply(basmul, arguments);
//     return basconcat.evaluate.apply(basmul, arguments).constant();
// };

// optdiv.onevaluate = 
//     beforeevaluatearg(basdiv.onevaluate);

// optdiv.onexpand = function (){
//     if (isoptimizableall(arguments))
//         return basconcat.expand.apply(basdiv, arguments);
//     return basconcat.evaluate.apply(basdiv, arguments).constant();
// };

// optmod.onevaluate = 
//     beforeevaluatearg(basmod.onevaluate);

// optmod.onexpand = function (){
//     if (isoptimizableall(arguments))
//         return basconcat.expand.apply(basmod, arguments);
//     return basconcat.evaluate.apply(basmod, arguments).constant();
// };

// define debug function

var debprint = new PrimitiveFunctionClass();

inp.scope.intern(new StringClass(slice("print").map(atoc))).setfunc(debprint);

debprint.onevaluate = function (some){
    console.log(some.toString());
    return some;
};

debprint.onexpand = function (some){
    var sym = new VariableSymbolClass();
    return makecons(maclet,
                makecons(
                    makecons(
                        makecons(sym,
                             makecons(some))),
                    makecons(
                        makecons(
                            new Expanded("console.log"),
                            makecons(sym))))).expandarg();
};

// define basic number methods

var basnumadd2 = new PrimitiveFunctionClass();
var basnumsub2 = new PrimitiveFunctionClass();
var basnummul2 = new PrimitiveFunctionClass();
var basnumdiv2 = new PrimitiveFunctionClass();
var basnummod2 = new PrimitiveFunctionClass();
var basnumand2 = new PrimitiveFunctionClass();
var basnumor2 = new PrimitiveFunctionClass();
var basnumnot2 = new PrimitiveFunctionClass();
var basnumeq2 = new PrimitiveFunctionClass();
var basnumless2 = new PrimitiveFunctionClass();
var basnumlesseq2 = new PrimitiveFunctionClass();
var basnumlarge2 = new PrimitiveFunctionClass();
var basnumlargeq2 = new PrimitiveFunctionClass();
var basintadd2 = new PrimitiveFunctionClass();
var basintsub2 = new PrimitiveFunctionClass();
var basintmul2 = new PrimitiveFunctionClass();
var basintdiv2 = new PrimitiveFunctionClass();
var basintmod2 = new PrimitiveFunctionClass();
var basintand2 = new PrimitiveFunctionClass();
var basintor2 = new PrimitiveFunctionClass();
var basintnot2 = new PrimitiveFunctionClass();
var basfloatadd2 = new PrimitiveFunctionClass();
var basfloatsub2 = new PrimitiveFunctionClass();
var basfloatmul2 = new PrimitiveFunctionClass();
var basfloatdiv2 = new PrimitiveFunctionClass();

basnumeq2.onevaluate = function (a, b){
    return (a.value == b.value) ? t : nil;
};

basnumless2.onevaluate = function (a, b){
    return (a.value < b.value) ? t : nil;
};

basnumlesseq2.onevaluate = function (a, b){
    return (a.value <= b.value) ? t : nil;
};

basnumlarge2.onevaluate = function (a, b){
    return (a.value > b.value) ? t : nil;
};

basnumlargeq2.onevaluate = function (a, b){
    return (a.value >= b.value) ? t : nil;
};

basfloatadd2.onevaluate = function (a, b){
    return new FloatClass(a.value + b.value);
};

basfloatsub2.onevaluate = function (a, b){
    return new FloatClass(a.value - b.value);
};

basfloatmul2.onevaluate = function (a, b){
    return new FloatClass(a.value * b.value);
};

basfloatdiv2.onevaluate = function (a, b){
    return new FloatClass(a.value / b.value);
};

basintadd2.onevaluate = function (a, b){
    if (b instanceof IntClass)
        return new IntClass(a.value + b.value);
    return basnumadd2.evaluate(b, a);
};

basintsub2.onevaluate = function (a, b){
    if (b instanceof IntClass)
        return new IntClass(a.value - b.value);
    return basnumsub2.evaluate(b, a);
};

basintmul2.onevaluate = function (a, b){
    if (b instanceof IntClass)
        return new IntClass(a.value * b.value);
    return basnummul2.evaluate(b, a);
};

basintdiv2.onevaluate = function (a, b){
    if (b instanceof IntClass)
        return new IntClass(a.value / b.value);
    return basnumdiv2.evaluate(b, a);
};

basintmod2.onevaluate = function (a, b){
    if (b instanceof IntClass)
        return new IntClass(a.value % b.value);
    return basnummod2.evaluate(b, a);
};

basintand2.onevaluate = function (a, b){
    if (b instanceof IntClass)
        return new IntClass(a.value & b.value);
    return basnumand2.evaluate(b, a);
};

basintor2.onevaluate = function (a, b){
    if (b instanceof IntClass)
        return new IntClass(a.value | b.value);
    return basnumor2.evaluate(b, a);
};

basintnot2.onevaluate = function (a){
    return new IntClass(~a.value);
};

// define basic function methods

var basfnfuncall = new PrimitiveFunctionClass();
var basfnapply = new PrimitiveFunctionClass();

// define basic cons methods

var basconmap = new UserFunctionClass();
var basconmap_func = inp.scope.intern(makestring("func"));
var basconmap_sequence = inp.scope.intern(makestring("sequence"));

var basconfilter = new UserFunctionClass();
var basconfilter_func = inp.scope.intern(makestring("func"));
var basconfilter_sequence = inp.scope.intern(makestring("sequence"));

var basconreduce = new UserFunctionClass();
var basconreduce_func = inp.scope.intern(makestring("func"));
var basconreduce_sequence = inp.scope.intern(makestring("sequence"));

var basconreducein = new UserFunctionClass();
var basconreducein_func = inp.scope.intern(makestring("func"));
var basconreducein_sum = inp.scope.intern(makestring("sum"));
var basconreducein_sequence = inp.scope.intern(makestring("sequence"));

var basconcons = new PrimitiveFunctionClass();
var basconcar = new PrimitiveFunctionClass();
var basconcdr = new PrimitiveFunctionClass();

basconmap.label = "<#primitive cons map>";
basconfilter.label = "<#primitive cons filter>";
basconreduce.label = "<#primitive cons reduce>";
basconreducein.label = "<#primitive cons reducein>";
basconcons.label = "<#primitive cons cons>";
basconcar.label = "<#primitive cons car>";
basconcdr.label = "<#primitive cons cdr>";

/* -- 
    (defun map (func sequence)
        (and sequence
            (cons (funcall func (car sequence))
                (cdr sequence))))
-- */

basconmap.args = makelist(
    basconmap_func,
    basconmap_sequence);

basconmap.rest = 
    makelist(
        synand,
        basconmap_sequence,
        makelist(
            bascons,
            makelist(
                basfuncall,
                basconmap_func,
                makelist(
                    bascar,
                    basconmap_sequence)),
            makelist(
                basconmap,
                basconmap_func,
                makelist(
                    bascdr,
                    basconmap_sequence))));

// define basic macros

var macnull = new UserMacroClass();
var macnull_some = inp.scope.intern(makestring("some"));

var macnot = new UserMacroClass();
var macnot_some = inp.scope.intern(makestring("some"));

var macand = new UserMacroClass();
var macand_rest = inp.scope.intern(makestring("&rest"));
var macand_args = inp.scope.intern(makestring("args"));

var macor = new UserMacroClass();
var macor_rest = inp.scope.intern(makestring("&rest"));
var macor_args = inp.scope.intern(makestring("args"));

macnull.args = makelist(macnull_some);
macnull.rest = makelist(synif, macnull_some, t, nil);

macnot.args = makelist(macnot_some);
macnot.rest = makelist(synif, macnot_some, nil, t);

/* -- 
    (if (null args) t
        (if (null (cdr args)) (car args)
            `(if ,(car args) (and ,@(cdr args)) nil)))
-- */

macand.args = makelist(
    macand_rest,
    macand_args);

macand.rest = 
    makelist(
        synif,
        makelist(
            macnull, 
            macand_args),
        t,
        makelist(
            synif,
            makelist(
                macnull,
                makelist(
                    basconcdr,
                    macand_args)),
            makelist(
                basconcar,
                macand_args),
            makequote(
                makelist(
                    synif,
                    makeunquote(
                        makelist(
                            basconcar,
                            macand_args)),
                    makelist(
                        macand,
                        makeunquoteat(
                            makelist(
                                basconcdr,
                                macand_args))),
                    nil))));

/* -- 
    (if (null args) nil
        `(if ,(car args) ,(car args)
            (or ,@(cdr args))))
-- */

macor.args = makelist(
    macor_rest,
    macor_args);

macor.rest = 
    makelist(
        synif,
        makelist(
            macnull,
            macor_args),
        nil,
        makequote(
            makelist(
                synif,
                makeunquote(
                    makelist(
                        basconcar,
                        macor_args)),
                makeunquote(
                    makelist(
                        basconcar,
                        macor_args)),
                makelist(
                    macor,
                    makeunquoteat(
                        makelist(
                            basconcdr,
                            macor_args))))));

// ** test code

// var source = 
//         makecons(macprog1,
//                  makecons(makeint(1),
//                           makecons(makeint(2), 
//                                    makecons(makeint(3)))));

//// console.log(source);
// console.log(source.evaluatearg());
// console.log(source.evaluatearg().expandarg());

var source = 
        makelist(
            basconmap,
            makelist(
                maclambda,
                makelist(inp.scope.intern(makestring("a"))),
                makeint(10)),
            makelist(
                baslist, 
                makeint(1),
                makeint(2),
                makeint(3)));

strace.unwindstrace(function (){
    // console.log(source);
    console.log(source.evaluatearg());
    console.log(source.evaluatearg().expandarg());
})();
