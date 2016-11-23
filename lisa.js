
// argumentstoarray

function slice (sequence, beginning, end){
    return Array.prototype.slice.call(sequence, beginning, end);
};

// get object keys

function keys (some){
    return Object.keys(some);
};

// strace

function Strace (){
    this.strace = [];
};

Strace.prototype.push = function (message){
    this.strace.push(message);
    return this.strace.length -1;
};

Strace.prototype.pop = function (index){
    while (this.strace.length > index)
        this.strace.pop();
    return this.strace.length;
};

Strace.prototype.clear = function (){
    this.strace = [];
};

Strace.prototype.willstrace = function (){
    var message = arguments.length == 2 ? arguments[0] : "";
    var func = arguments.length == 2 ? arguments[1] : arguments[0];
    var temp;
    var self = this;
    return function willstrace_closure (){
        var index;
        index = self.push(message + this.toLisp());
        temp = func.apply(this, arguments);
        self.pop(index);
        return temp;
    };
};

Strace.prototype.unwindstrace = function (func){
    var temp, self = this;
    return function unwindstrace_closure (){
        try { temp = func.apply(this, arguments); }
        catch (errorn) { 
            self.print();
            throw errorn;
        };
        return temp;
    };
};

Strace.prototype.print = function (){
    var message, index;
    for (message = "lisa trace:\n",
         index = 0; index < this.strace.length; index++)
        message += "    " + index + ": " + this.strace[index] + "\n";
    console.log(message);
};

var strace = new Strace();
var stracedb = new Strace();

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
    strace.willstrace("evaluate <- ", Evaluatable.prototype.evaluate);

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

Expanded.prototype.toLisp =
    Expanded.prototype.toString;

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

AtomClass.prototype.toLisp = function (){
    return this.toString();
};

AtomClass.prototype.toPlain = function (){
    return this.toString();
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

function value (atom){
    if (atom instanceof AtomClass == false) 
        throw new Error("" + atom + " is not atom class."); 
    return atom.value;
};

function tostring (atom){
    if (atom instanceof AtomClass == false)
        throw new Error("" + atom + " is not atom class."); 
    return atom.toString();
};

function tolisp (atom){
    if (atom instanceof AtomClass == false)
        throw new Error("" + atom + " is not atom class.");
    return atom.toLisp();
};

function toplain (atom){
    if (atom instanceof AtomClass == false)
        throw new Error("" + atom + "is not atom class.");
    return atom.toplain();
};

function clone (atom){
    if (atom instanceof AtomClass == false)
        throw new Error("" + atom + " is not atom class.");
    return atom.clone();
};

function beforeclone (func){
    return function beforeclone_closure (){
        return func.apply(func, slice(arguments).map(clone));
    };
};

function afterclone (func){
    return function afterclone_closure (){
        return func.apply(func, arguments).clone();
    };
};

// true class
//     <- atom class

function TrueClass (){}

TrueClass.prototype = 
    Object.create(AtomClass.prototype);

TrueClass.prototype.toString = function (){ return "true"; };
TrueClass.prototype.toLisp = function (){ return "t"; };
TrueClass.prototype.status = function (){ return true; };

var t = new TrueClass();

// reference class
//     <- atom class

function ReferenceClass (){}

ReferenceClass.prototype = 
    Object.create(AtomClass.prototype);

ReferenceClass.prototype.toString = function (){
    return this.get().toString();
};

ReferenceClass.prototype.toLisp = function (){
    return this.get().toLisp();
};

ReferenceClass.prototype.get = function (){
    throw new Error("reference " + this.toLisp() + "get has not defined yet.");
};

ReferenceClass.prototype.set = function (){
    throw new Error("reference " + this.toLisp() + "set has not defined yet.");
};

ReferenceClass.prototype.onevaluatearg = function (){
    return this.get().evaluatearg();
};

ReferenceClass.prototype.onexpandarg = function (){
    return this.get().expandarg();
};

ReferenceClass.prototype.onevaluate = function (){
    var value = this.get();
    return value.evaluate.apply(value, arguments);
};

ReferenceClass.prototype.onexpand = function (){
    var value = this.get();
    return value.expand.apply(value, arguments);
};

function getreference (reference){
    return reference instanceof ReferenceClass ?
        reference.get() : 
        reference;
};

function beforegetreference (func){
    return function beforegetreference_closure (){
        return func.apply(this, slice(arguments).map(getreference));
    };
};

function aftergetreference (func){
    return function aftergetreference_closure (){
        return getreference(func.apply(this, arguments));
    };
};

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

// // cons reference class
// //     <- reference class

function ConsReferenceClass (cons){
    
    if (cons instanceof ConsClass == false)
        throw new Error("" + cons.toLisp() + " is should cons instance.");
    
    this.cons = cons || null;
};

ConsReferenceClass.prototype = 
    Object.create(ReferenceClass.prototype);

ConsReferenceClass.prototype.get = function (){
    return this.cons;
};

ConsReferenceClass.prototype.getcar = function (){
    return this.get().getcar();
};

ConsReferenceClass.prototype.getcdr = function (){
    return this.get().getcdr();
};

ConsReferenceClass.prototype.setcar = function (value){
    return this.get().setcar(value);
};

ConsReferenceClass.prototype.setcdr = function (value){
    return this.get().setcdr(value);
};

ConsReferenceClass.prototype.getcar = function (){
    return this.get().getcar();
};

ConsReferenceClass.prototype.getcdr = function (){
    return this.get().getcdr();
};

function makeconsreference (cons){
    return new ConsReferenceClass(cons);
};

// cons car reference class
//     <- cons reference class

function ConsCarReferenceClass (cons){
    ConsReferenceClass.apply(this, arguments);
};

ConsCarReferenceClass.prototype =
    Object.create(ConsReferenceClass.prototype);

ConsCarReferenceClass.prototype.get = function (){
    return this.cons.getcar();
};

ConsCarReferenceClass.prototype.set = function (value){
    return this.cons.setcar(value);
};

function makeconscarreference (cons){
    return new ConsCarReferenceClass(cons);
};

// cons cdr reference class
//     <- cons reference class

function ConsCdrReferenceClass (){
    ConsReferenceClass.apply(this, arguments);
};

ConsCdrReferenceClass.prototype =
    Object.create(ConsReferenceClass.prototype);

ConsCdrReferenceClass.prototype.get = function (){
    return this.cons.getcdr();
};

ConsCdrReferenceClass.prototype.set = function (value){
    return this.cons.setcdr(value);
};

function makeconscdrreference (cons){
    return new ConsCdrReferenceClass(cons);
};

// symbol reference class
//      <- reference class

function SymbolReferenceClass (value){
    this.value = value;
};

SymbolReferenceClass.prototype = 
    Object.create(ReferenceClass.prototype);

// symbol value reference class

function SymbolValueReferenceClass (){
    SymbolReferenceClass.apply(this, arguments);
};

SymbolValueReferenceClass.prototype = 
    Object.create(SymbolReferenceClass.prototype);

SymbolValueReferenceClass.prototype.get = function (){

    var message = "symbol value reference <- " + this.toLisp() + "";
    strace.push(message);
    stracedb.push(message);
    
    return this.value.getvalue();
};

SymbolValueReferenceClass.prototype.set = function (value){
    return this.value.setvalue(value);
};

SymbolValueReferenceClass.prototype.onexpandarg = function (){
    return new Expanded(this.value.getvaluename());
};

// symbol function reference class

function SymbolFunctionReferenceClass (){
    SymbolReferenceClass.apply(this, arguments);
};

SymbolFunctionReferenceClass.prototype = 
    Object.create(SymbolReferenceClass.prototype);

SymbolFunctionReferenceClass.prototype.get = function (){
    return this.value.getfunc();
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
};

NumberClass.prototype =
    Object.create(AtomClass.prototype);

NumberClass.prototype.clone = function (){
    return new NumberClass(this.value);
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

function makefloat (num) {
    return new FloatClass(num);
};

// int class
//     <- number class

function IntClass (number){
    this.value = number;
};

IntClass.prototype = 
    Object.create(NumberClass.prototype);

IntClass.prototype.clone = function (){
    return new IntClass(this.value);
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

CharClass.prototype.toLisp = function (){
    return "?" + String.fromCharCode(this.value);
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
    return "" + this.value.map(toplain).join(",") + "";
};

ArrayClass.prototype.toString = function (){
    return "[" + this.value.map(tostring).join(",") + "]";
};

ArrayClass.prototype.toLisp = function (){
    return "#(" + this.value.map(tolisp).join(" ") + ")";
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

StringClass.prototype.toString = function (){
    return '"' + this.value.join("") + '"';
};

StringClass.prototype.toPlain = function (){
    return "" + this.value.join("") + "";
};

StringClass.prototype.toLisp = function (){
    return this.toString();
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

ConsClass.prototype.getcar = function (){
    return this.car;
};

ConsClass.prototype.getcdr = function (){
    return this.cdr;
};

ConsClass.prototype.setcar = function (value){
    this.car = value;
    return value;
};

ConsClass.prototype.setcdr = function (value){
    this.cdr = value;
    return value;
};

ConsClass.prototype.toPlain = function (){
    return "" + this.toArray().map(tostring).join(",") + "";
};

ConsClass.prototype.toLisp = function (){
    return "(" + this.toArray().map(tolisp).join(" ") + ")";
};

ConsClass.prototype.toString = function (){
    return "[" + this.toArray().map(tostring).join(",") + "]";
};

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

ConsClass.toCons = function (sequence){
    var cons, index;
    for (cons = nil, index = 0; index < sequence.length; index++)
        cons = makecons(sequence[index], cons);
    return cons.reverse();
};

ConsClass.prototype.toCons = function (){
    return this;
};

ConsClass.prototype.clone = function (){ // ** should update here
    var ncons, cons;
    for (ncons = nil, cons = this; cons != nil; cons = cons.cdr){
        if (cons.car instanceof UnQuoteAtClass){
            var consa, consb;
            for (consa = cons.car.clone().reverse(), // like as (append seq seq2)
                 consb = consa;
                 consb != nil && consb.cdr != nil;
                 consb = consb.cdr);
            consb.cdr = ncons;
            ncons = consa;}
        else if (cons.car instanceof UnQuoteClass)
            ncons = makecons(cons.car.clone(), ncons);
        else ncons = makecons(cons.car.clone(), ncons);}
    return ncons.reverse();
};

ConsClass.prototype.toArray = function (){
    this.shouldlinear();
    var sequence, cons;
    for (sequence = [], cons = this; cons != nil; cons = cons.cdr)
        sequence.push(cons.car);
    return sequence;
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

NilClass.prototype.getcar = function (){return nil;};
NilClass.prototype.getcdr = function (){return nil;};
NilClass.prototype.setcar = function (){throw new Error("nil cannot setcar.");};
NilClass.prototype.setcdr = function (){throw new Error("nil cannot setcdr.");};

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

NilClass.prototype.status = function (){ return false; };
NilClass.prototype.toLisp = function (){ return "nil"; };
NilClass.prototype.toString = function (){ return "null"; };

var nil = new NilClass();

// quote family class 
//     <- atom class

function QuoteFamilyClass (value){
    this.value = value;
}

QuoteFamilyClass.prototype = 
    Object.create(AtomClass.prototype);

// unquote class
//     <- quote family class

function UnQuoteClass (value){
    this.value = value;
};

UnQuoteClass.prototype = 
    Object.create(QuoteFamilyClass.prototype);

UnQuoteClass.prototype.onexpand = function (){ throw new Error("unquote cannot expand"); };
UnQuoteClass.prototype.onevaluate = function (){ throw new Error("unquote is cannot evaluate"); };
UnQuoteClass.prototype.onexpandarg = function (){ throw new Error("unquote cannot expandarg"); };
UnQuoteClass.prototype.onevaluatearg = function (){ throw new Error("unquote is cannot evaluatearg"); };

UnQuoteClass.prototype.toString  = function (){
    return "/*--unquote--*/" + this.value.toString();
};

UnQuoteClass.prototype.toLisp = function (){
    return "," + this.value.toString();
};

UnQuoteClass.prototype.clone = function (){
    return getreference(this.value.evaluatearg());
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

UnQuoteAtClass.prototype.onexpand = function (){ throw new Error("unquoteat cannot expand"); };
UnQuoteAtClass.prototype.onevaluate = function (){ throw new Error("unquoteat is cannot evaluate"); };
UnQuoteAtClass.prototype.onexpandarg = function (){ throw new Error("unquoteat cannot expandarg"); };
UnQuoteAtClass.prototype.onevaluatearg = function (){ throw new Error("unquoteat is cannot evaluatearg"); };

UnQuoteAtClass.prototype.toString = function (){
    return "/*--unquoteat--*/" + this.value.toString();
};

UnQuoteAtClass.prototype.toLisp = function (){
    return ",@" + this.value.toLisp();
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

// primitive function class
//     <- function class

function PrimitiveFunctionClass (){}

PrimitiveFunctionClass.prototype =
    Object.create(FunctionClass.prototype);

PrimitiveFunctionClass.prototype.evaluate = 
    beforeevaluatearg(
        beforegetreference(
            FunctionClass.prototype.evaluate));

PrimitiveFunctionClass.prototype.expand = 
    beforeexpandarg(
        beforegetreference(
            FunctionClass.prototype.expand));

PrimitiveFunctionClass.prototype.label = "<#primitive function class>";

// user function class
//     <- function class

function UserFunctionClass (args, rest){
    this.args = args || null;
    this.rest = rest || null;
};

UserFunctionClass.prototype = 
    Object.create(FunctionClass.prototype);

UserFunctionClass.prototype.toLisp = function (){
    return makecons(synlambda, makecons(this.args, this.rest)).toLisp();
};

UserFunctionClass.prototype.onevaluate = function (){

    // bound initalize

    var formula;
    var bound, consa, consb;
    bound = makecons(synprogn);
    consa = this.args;
    consb = ConsClass.toCons(arguments);

    // argument binding.

    for (; consa != nil && consb != nil; 
         consa = consa.cdr, consb = consb.cdr){
        if (consa.car == makeintern("&rest")) break;
        if (consa.car == makeintern("&optional")) break;
        bound = 
            makecons(
                makecons(macdeflvar,
                         makecons(consa.car,
                                  makecons(consb.car))),
                bound);
    };

    // optional argument binding.
 
    if (consa.car == makeintern("&optional")){
        consa = consa.cdr;
        for (; consa != nil && consb != nil;
             consa = consa.cdr, consb = consb.cdr){
            if (consa.car == makeintern("&rest")) break;
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

    if (consa.car == makeintern("&rest")){
        consa = consa.cdr;
        bound =
            makecons(
                makelist(
                    macdeflvar,
                    consa.car,
                    makelist(
                        synquote,
                        consb)),
                bound);
    };

    // reverse binding arguments.

    bound = bound.reverse();
    
    // build formula.
 
    formula = makecons(synblock,
                    makecons(bound, this.rest));

    var message;    
    message = ("user function <- " + formula.toLisp() + "");
    strace.push(message);
    stracedb.push(message);

    // evaluate formula.
    
    return formula.evaluatearg();
};

// macro class
//     <- atom class

function MacroClass (){}

MacroClass.prototype = 
    Object.create(CallableClass.prototype);

MacroClass.prototype.evaluate = 
    beforegetreference(
        aftergetreference(
            afterevaluatearg(
                CallableClass.prototype.evaluate)));

MacroClass.prototype.expand = 
    beforegetreference(
        aftergetreference(
            afterexpandarg(
                CallableClass.prototype.evaluate)));

// primitive macro class
//     <- macro class

function PrimitiveMacroClass (){}

PrimitiveMacroClass.prototype = 
    Object.create(MacroClass.prototype);

// user macro class
//     <- macro class

function UserMacroClass (args, rest){
    this.args = args || null;
    this.rest = rest || null;
};

UserMacroClass.prototype = 
    Object.create(MacroClass.prototype);

UserMacroClass.prototype.onevaluate = function (){

    var bound, consa, consb;
    bound = makecons(synprogn);
    consa = this.args;
    consb = ConsClass.toCons(arguments);

    for (; consa != nil && consb != nil; 
         consa = consa.cdr, consb = consb.cdr){
        if (consa.car == makeintern("&rest")) break;
        if (consa.car == makeintern("&optional")) break;
        bound =
            makecons( // (cons ... bound)
                makelist( // (setf (symbolvalue (local (quote consa.car))) consb.car)
                    synsetf,
                    makelist(
                        bassymbolvalue,
                        makelist(
                            baslocal,
                            makelist(
                                synquote,
                                consa.car))),
                    makelist(
                        synquote,
                        consb.car)),
                bound);
    };
 
    if (consa.car == makeintern("&optional")){ // ** should check here
        consa = consa.cdr;
        for (; consa != nil && consb != nil;
             consa = consa.cdr, consb = consb.cdr){
            if (consa.car == makeintern("&rest")) break;
            bound =
                makecons(
                    makecons(macdeflvar,
                             (consb == nil) ?
                             (consa.car instanceof ConsClass == false) ?
                             (makecons(consa.car, makecons(nil))):
                             (makecons(consa.car.car, makecons(consa.car.cdr.car))):
                             (consa.car instanceof ConsClass == false) ?
                             (makecons(consa.car, makecons(makelist(synquote, consb.car)))):
                             (makecons(consa.car.car, makecons(makelist(synquote, consb.car))))),
                    bound);
        }};
    
    if (consa.car == makeintern("&rest")){
        consa = consa.cdr;
        bound =
            makecons(
                makelist(
                    synsetf,
                    makelist(
                        bassymbolvalue,
                        makelist(
                            baslocal,
                            makelist(
                                synquote,
                                consa.car))),
                    makelist(
                        synquote,
                        consb)),
                bound);
    };

    bound = bound.reverse();

    var formula =
            makecons(synblock,
                     makecons(bound, this.rest));

    var message =  "user function <- " + formula.toLisp();
    strace.push(message);
    stracedb.push(message);

    var expantion = formula.evaluatearg();

    var message2 = "macro expantion <- " + expantion.toLisp();
    strace.push(message2);
    stracedb.push(message2);
    
    return expantion;
};

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

SymbolClass.prototype.setvalue = function (value){
    this.value = value;
    return value;
};

SymbolClass.prototype.setfunc = function (func){
    this.func = func;
    return func;
};

SymbolClass.prototype.onevaluate = function (){
    var func = this.getfunc();
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

var interneds = [];

function InternSymbolClass (name){

    // find aleady interneds

    var index;
    for (index = 0; index < interneds.length; index++)
        if (name.toString() == interneds[index].name.toString())
            return interneds[index];
    
    // make the instance

    this.name = name ? name.copy() : null;
    interneds.push(this);
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
StreamClass.direction.io =
    StreamClass.direction.input |
    StreamClass.direction.output;

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
    return this.iseof() ? nil : this.source.nth(this.index);
};

StringStreamClass.prototype.get = function (){
    this.shouldin();
    return this.iseof() ? nil : this.source.nth(this.index++);
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
    return this.obarray[name.toPlain()] = sym;
};

Obarray.prototype.intern = function (name){
    if (this.find(name) == null)
        this.set(name, new VariableSymbolClass(name));
    return new InternSymbolClass(name);
};

// obarrays class
//     <- native, function class

function Obarrays (parent){
    this.obarray = new Obarray();
    this.parent = parent || null;
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
    if (this.find(name) == null)
        return this.obarray.intern(name);
    return new InternSymbolClass(name);
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
    if (this.find(name) == null)
        return this.obarray.intern(name);
    return new InternSymbolClass(name);
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
var rdreadquoteback = new PrimitiveFunctionClass();
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
    return new InternSymbolClass(name);
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
    return makelist(synquote, rdread.evaluate(stream));
};

rdreadquoteback.onevaluate = function (stream){
    return makelist(synquoteback, rdread.evaluate(stream));
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
inp.readerscope.dig("`").setcurrent(rdreadquoteback);
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
var synprogn = new SpecialFunctionClass();
var synsetf = new SpecialFunctionClass();
var synquote = new SpecialFunctionClass();
var synquoteback = new SpecialFunctionClass();
var synlambda = new SpecialFunctionClass();
var synmacro = new SpecialFunctionClass();
var syninvisible = new SpecialFunctionClass();

inp.scope.intern(makestring("if")).setfunc(synif);
inp.scope.intern(makestring("block")).setfunc(synblock);
inp.scope.intern(makestring("progn")).setfunc(synprogn);
inp.scope.intern(makestring("setf")).setfunc(synsetf);
inp.scope.intern(makestring("quote")).setfunc(synquote);
inp.scope.intern(makestring("quoteb")).setfunc(synquoteback);

synif.label = "$if";
synblock.label = "$block";
synprogn.label = "$progn";
synsetf.label = "$setf";
synquote.label = "$quote";
synquoteback.label = "$quoteback";
synlambda.label = "$lambda";
synmacro.label = "$macro";
syninvisible.label = "$invisible";

synif.onevaluate = function (cond, truecase, falsecase){
    if (getreference(cond.evaluatearg()) != nil)
        return getreference(truecase.evaluatearg());
    return getreference(falsecase.evaluatearg());
};

synif.onexpand = function (cond, truecase, falsecase){
    return new Expanded(
        "(" + cond.expandarg() + "==null?" + 
            falsecase.expandarg() + ":" + 
            truecase.expandarg() + ")");
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

synprogn.onevaluate = function (){
    var res, index;
    for (res = nil, index = 0; index < arguments.length; index++)
        res = arguments[index].evaluatearg();
    return res;
};

synprogn.onexpand = function (){
    var source, index;
    for (source = "", index = 0; index < arguments.length; index++)
        source += (index ? "," : "") + arguments[index].expandarg();
    return new Expanded("(" + source + ")");
};

synsetf.onevaluate = function (formula, value){

    var message = "set formula = " + formula.toLisp() + " = " + value.toLisp() + "";
    strace.push(message);
    stracedb.push(message);

    var valued = getreference(value.evaluatearg());
    var formulaed = formula.evaluatearg();
    return formulaed.set(valued);
};

synsetf.onexpand = function (formula, value){

    var message = "set formula expand = " + formula.toLisp() + " = " + value.toLisp() + "";
    strace.push(message);
    stracedb.push(message);
    
    var valued = getreference(value.evaluatearg());
    var formulaed = formula.evaluatearg();
    formulaed.set(valued);

    return new Expanded("(" + formulaed.expandarg() + "=" + valued.expandarg() + ")");
};

synquote.onevaluate = function (some){
    
    var message = "unquote " + some.toLisp() + "";
    strace.push(message);
    stracedb.push(message);
    
    return some;
};

synquote.onexpand = function (some){
    return this.evaluate.apply(this, arguments).expandarg();
};

synquoteback.onevaluate = function (some){

    var message  = "unquoteback " + some.toLisp() + " = " + some.clone().toLisp() + "";
    strace.push(message);
    stracedb.push(message);
    
    return some.clone();
};

synquoteback.onexpand = function (some){
    return this.evaluate.apply(this, arguments).expandarg();
};

synlambda.onevaluate = function (args){
    return new UserFunctionClass(args, ConsClass.toCons(slice(arguments, 1)));
};

synmacro.onevaluate = function (args){
    return new UserMacroClass(args, ConsClass.toCons(slice(arguments, 1)));
};

syninvisible.onevaluate = 
    synprogn.onevaluate;

syninvisible.onexpand = function (){
    this.evaluate.apply(this, arguments);
    return nil.expandarg();
};

// define basic scope methods

var baslocal = new PrimitiveFunctionClass();
var basglobal = new PrimitiveFunctionClass();

baslocal.label = "$local";
basglobal.label = "$global";

baslocal.onevaluate = function (sym){
    if (sym instanceof SymbolFamilyClass == false)
        throw new Error("" + sym + " is not symbol family instance.");
    return inp.scope.internf(sym.name);
};

basglobal.onevaluate = function (sym){
    if (sym instanceof SymbolFamilyClass == false)
        throw new Error("" + sym + " is not symbol family instance.");
    return inp.scoperoot.internf(sym.name);
};

// define basic symbol methods

var bassymbolfunction = new PrimitiveFunctionClass();
var bassymbolvalue = new PrimitiveFunctionClass();
var bassymbolname = new PrimitiveFunctionClass();
var bassymbolintern = new PrimitiveFunctionClass();
var bassymbolmake = new PrimitiveFunctionClass();

bassymbolfunction.label = "$symbol-function";
bassymbolvalue.label = "$symbol-value";
bassymbolname.label = "$symbol-name";
bassymbolintern.label = "$symbol-intern";
bassymbolmake.label = "$symbol-make";

bassymbolfunction.onevaluate = function (sym){
    return new SymbolFunctionReferenceClass(sym);
};

bassymbolvalue.onevaluate = function (sym){
    return new SymbolValueReferenceClass(sym);
};

bassymbolname.onevaluate = function (sym){
    return sym.name;
};

bassymbolintern.onevaluate = function (name){
    return new InternSymbolClass(name);
};

bassymbolmake.onevaluate = function (name){
    return new VariableSymbolClass(name);
};

// define temp method

var basadd = new PrimitiveFunctionClass();
var bassub = new PrimitiveFunctionClass();

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

basfnfuncall.label = "$funcall";
basfnapply.label = "$apply";

basfnfuncall.onevaluate = function (func){
    return func.evaluate.apply(func, slice(arguments, 1));
};

basfnapply.onevaluate = function (func, args){
    return func.evaluate.apply(func, args.toArray());
};

// define basic debug methods

var basprint = new PrimitiveFunctionClass();
var basstrace = new PrimitiveFunctionClass();
var basstracedb = new PrimitiveFunctionClass();

basprint.label = "$print";
basstrace.label = "$strace";
basstracedb.label = "$stracedb";

basprint.onevaluate = function (some){
    console.log(some.toLisp());
    return some;
};

basstrace.onevaluate = function (){
    strace.print();
    return nil;
};

basstracedb.onevaluate = function (){
    stracedb.print();
    return nil;
};

basprint.onexpand = function (some){ // ** should redefine again.
    return makelist(
        synprogn,
        makelist(new Expanded("console.log"), some),
        some
    ).expandarg();
};

basstrace.onexpand = function (){
    basstrace.evaluate.apply(this, arguments);
    return nil.expandarg();
};

basstracedb.onexpand = function (){
    basstracedb.evaluate.apply(this, arguments);
    return nil.expandarg();
};

// define basci logic methods

var baseq2 = new PrimitiveFunctionClass();
var baseq = new PrimitiveFunctionClass();

baseq2.label = "$eq2";
baseq.label = "$eq";

baseq2.onevaluate = function (a,b){
    return a == b ? t : nil;
};

// define basic cons methods

var basconconsp = new PrimitiveFunctionClass();
var basconcons = new PrimitiveFunctionClass();
var basconcar = new PrimitiveFunctionClass();
var basconcdr = new PrimitiveFunctionClass();
var basconlist = new PrimitiveFunctionClass();
var basconcaar = new UserFunctionClass();
var basconcaar_cons = makeintern("cons");
var basconcdar = new UserFunctionClass();
var basconcdar_cons = makeintern("cons");
var basconcadr = new UserFunctionClass();
var basconcadr_cons = makeintern("cons");
var basconcddr = new UserFunctionClass();
var basconcddr_cons = makeintern("cons");

basconcons.label = "$cons";
basconcar.label = "$car";
basconcdr.label = "$cdr";
basconcaar.label = "$caar";
basconcadr.label = "$cadr";
basconcdar.label = "$cdar";
basconcddr.label = "$cddr";
basconlist.label = "$list";

inp.scope.intern(makestring("cons")).setfunc(basconcons);
inp.scope.intern(makestring("car")).setfunc(basconcar);
inp.scope.intern(makestring("cdr")).setfunc(basconcdr);
inp.scope.intern(makestring("caar")).setfunc(basconcaar);
inp.scope.intern(makestring("cadr")).setfunc(basconcadr);
inp.scope.intern(makestring("cdar")).setfunc(basconcdar);
inp.scope.intern(makestring("cddr")).setfunc(basconcddr);
inp.scope.intern(makestring("list")).setfunc(basconlist);

basconconsp.onevaluate = function (cons){
    return cons instanceof ConsClass ? t : nil;
};

basconcons.onevaluate = function (car, cdr){
    return new ConsReferenceClass(
        new ConsClass(car, cdr));
};

basconcar.onevaluate = function (cons){
    return new ConsCarReferenceClass(cons);
};

basconcdr.onevaluate = function (cons){
    return new ConsCdrReferenceClass(cons);
};

basconlist.onevaluate = function (){
    return new ConsReferenceClass(
        ConsClass.toCons(arguments));
};

/* --
    (defun caar (cons)
        (car (car cons)))
-- */

basconcaar.args = 
    makelist(basconcaar_cons);

basconcaar.rest = 
    makelist(
        makelist(
            basconcar,
            makelist(
                basconcar,
                basconcaar_cons)));

/* --
    (defun cadr (cons)
        (car (cdr cons)))
-- */

basconcadr.args = 
    makelist(basconcadr_cons);

basconcadr.rest = 
    makelist(
        makelist(
            basconcar,
            makelist(
                basconcdr,
                basconcadr_cons)));

/* --
    (defun cdar (cons)
        (cdr (car cons)))
-- */

basconcdar.args = 
    makelist(basconcdar_cons);

basconcdar.rest = 
    makelist(
        makelist(
            basconcdr,
            makelist(
                basconcar,
                basconcdar_cons)));

/* --
    (defun cddr (cons)
        (cdr (cdr cons)))
-- */

basconcddr.args = 
    makelist(basconcddr_cons);

basconcddr.rest = 
    makelist(
        makelist(
            basconcdr,
            makelist(
                basconcdr,
                basconcddr_cons)));

// define basic macros

var basnull = new UserFunctionClass();
var basnull_some = makeintern("some");
var basnot = new UserFunctionClass();
var basnot_some = makeintern("some");
var macand = new UserMacroClass();
var macand_rest = makeintern("&rest");
var macand_args = makeintern("args");
var macor = new UserMacroClass();
var macor_rest = makeintern("&rest");
var macor_args = makeintern("args");
var macwhen = new UserMacroClass();
var macwhen_cond = makeintern("cond");
var macwhen_rest = makeintern("&rest");
var macwhen_args = makeintern("args");
var macunless = new UserMacroClass();
var macunless_cond = makeintern("cond");
var macunless_rest = makeintern("&rest");
var macunless_args = makeintern("args");
var maccond = new UserMacroClass();
var maccond_rest = makeintern("&rest");
var maccond_args = makeintern("args");
var maccase = new UserMacroClass();
var maccase_cond = makeintern("cond");
var maccase_rest = makeintern("&rest");
var maccase_args = makeintern("args");
var macdefun = new UserMacroClass();
var macdefun_name = makeintern("name");
var macdefun_rest = makeintern("&rest");
var macdefun_args = makeintern("args");
var macdefmacro = new UserMacroClass();
var macdefmacro_name = makeintern("name");
var macdefmacro_rest = makeintern("&rest");
var macdefmacro_args = makeintern("args");
var macsetq = new UserMacroClass();
var macsetq_sym = makeintern("sym");
var macsetq_value = makeintern("value");
var maclet = new UserMacroClass();
var maclet_rest = makeintern("&rest");
var maclet_args = makeintern("args");
var macletin = new UserMacroClass();
var macletin_binds = makeintern("binds");
var macletin_rest = makeintern("&rest");
var macletin_args = makeintern("args");
var macflet = new UserMacroClass();
var macflet_rest = makeintern("&rest");
var macflet_args = makeintern("args");
var macfletin = new UserMacroClass();
var macfletin_binds = makeintern("binds");
var macfletin_rest = makeintern("&rest");
var macfletin_args = makeintern("args");
var macmlet = new UserMacroClass();
var macmlet_rest = makeintern("&rest");
var macmlet_args = makeintern("args");
var macmletin = new UserMacroClass();
var macmletin_binds = makeintern("binds");
var macmletin_rest = makeintern("&rest");
var macmletin_args = makeintern("args");
var macdefvar = new UserMacroClass();
var macdefvar_sym = makeintern("sym");
var macdefvar_value = makeintern("value");
var macdeflvar = new UserMacroClass();
var macdeflvar_sym = makeintern("sym");
var macdeflvar_value = makeintern("value");
var macprog1 = new UserMacroClass();
var macprog1_rest = makeintern("&rest");
var macprog1_args = makeintern("args");
var macprog1_temp = makeintern("temp");

basnull.label = "$null";
basnot.label = "$not";
macand.label = "$and";
macor.label = "$or";
macwhen.label = "$when";
macunless.label = "$unless";
maccond.label = "$cond";
maccase.label = "$case";
macdefun.label = "$defun";
macdefmacro.label = "$defmacro";
macsetq.label = "$setq";
maclet.label = "$let";
macletin.label = "$letin";
macflet.label = "$flet";
macfletin.label = "$fletin";
macmlet.label = "$mlet";
macmletin.label = "$mletin";
macdefvar.label = "$defvar";
macdeflvar.label = "$deflvar";
macprog1.label = "$prog1";

inp.scope.intern(makestring("null")).setfunc(basnull);
inp.scope.intern(makestring("not")).setfunc(basnot);
inp.scope.intern(makestring("and")).setfunc(macand);
inp.scope.intern(makestring("or")).setfunc(macor);
inp.scope.intern(makestring("when")).setfunc(macwhen);
inp.scope.intern(makestring("unless")).setfunc(macunless);
inp.scope.intern(makestring("cond")).setfunc(maccond);
inp.scope.intern(makestring("case")).setfunc(maccase);
inp.scope.intern(makestring("defun")).setfunc(macdefun);
inp.scope.intern(makestring("defmacro")).setfunc(macdefmacro);
inp.scope.intern(makestring("setq")).setfunc(macsetq);
inp.scope.intern(makestring("let")).setfunc(maclet);
inp.scope.intern(makestring("defvar")).setfunc(macdefvar);
inp.scope.intern(makestring("deflvar")).setfunc(macdeflvar);

/* -- 
    (if some nil t)
-- */

basnull.args = makelist(basnull_some);
basnull.rest = 
    makelist(
        makelist(
            synif,
            basnull_some,
            nil,
            t));

/* --
    (if some nil t)
-- */

basnot.args = makelist(basnot_some);
basnot.args = 
    makelist(
        makelist(
            synif,
            basnot_some,
            nil,
            t));
            
/* -- 
    (if (null args) t
        (if (null (cdr args)) (car args)
            `(if ,(car args) (and ,@(cdr args)) nil)))
-- */

macand.args = makelist(
    macand_rest,
    macand_args);

macand.rest = 
    makelist( // (if (null args) t ...
        synif,
        makelist(
            basnull,
            macand_args),
        t,
        makelist( // (if (null (cdr args)) (car args) ...
            synif,
            makelist(
                basnull,
                makelist(
                    basconcdr,
                    macand_args)),
            makelist(
                basconcar,
                macand_args),
            makelist( // (list 'if (car args) (cons 'and (cdr args) '())
                basconlist,
                synif,
                makelist(
                    basconcar,
                    macand_args),
                makelist(
                    basconcons,
                    macand,
                    makelist(
                        basconcdr,
                        macand_args)),
                makelist())));

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
        makelist( // (if (null args) nil ...
            synif,
            makelist(
                basnull,
                macor_args),
            nil,
            makelist( // (if (null (cdr args)) (car args) ...
                synif,
                makelist(
                    basnull,
                    makelist(
                        basconcdr,
                        macor_args)),
                makelist(
                    basconcar,
                    macor_args),
                makelist( // (list if (car args) (car args) (cons 'or (cdr args))
                    basconlist,
                    synif,
                    makelist(
                        basconcar,
                        macor_args),
                    makelist(
                        basconcar, 
                        macor_args),
                    makelist(
                        basconcons,
                        macor,
                        makelist(
                            basconcdr,
                            macor_args))))));

/* -- 
    (defmacro when (cond &rest args)
        (list synif cond (cons progn args) nil)))
-- */

macwhen.args = makelist(
    macwhen_cond,
    macwhen_rest,
    macwhen_args);

macwhen.rest =
    makelist(
        makelist(
            basconlist,
            synif,
            macwhen_cond,
            makelist(
                basconcons,
                synprogn,
                macwhen_args),
            nil));

/* --
    (defmacro unless (cond &rest args)
        (list synif cond nil (list progn args)))
-- */

macunless.args = makelist(
    macunless_cond,
    macunless_rest,
    macunless_args);

macunless.rest =
    makelist(
        makelist(
            basconlist,
            synif,
            macunless_cond,
            nil,
            makelist(
                basconcons,
                synprogn,
                macunless_args)));

/* --
    (defmacro cond (&rest args)
        (if (null args) nil
            (list if 
                (caar conds)
                (progn (cdar args))
                (cons cond (cdr args)))))
-- */

maccond.args = makelist(
    maccond_rest,
    maccond_args);

maccond.rest =
    makelist(
        makelist( // (if (null args) nil ...
            synif,
            makelist(
                basnull,
                maccond_args),
            nil,
            makelist( // (list if (caar args) (list progn (cdar args)) ...
                basconlist,
                synif,
                makelist(
                    basconcaar,
                    maccond_args),
                makelist(
                    basconcons,
                    synprogn,
                    makelist(
                        basconcdar,
                        maccond_args)),
                makelist( // (list cond ,@(cdr args))
                    basconcons,
                    maccond,
                    makelist(
                        basconcdr,
                        maccond_args)))));

/* --
    (defmacro case (cond &rest args)
        (if (null args) nil
            (list if 
                (list eq cond (caar args))
                (list progn (cdar args))
                (cons case (cons cond (cdr args))))))
-- */

maccase.args = makelist(
    maccase_cond,
    maccase_rest,
    maccase_args);

maccase.rest =
    makelist(
        makelist( // (if (null args) nil ...
            synif,
            makelist(
                basnull,
                maccase_args),
            nil,
            makelist( // (list if ...
                basconlist,
                synif,
                makelist( // (list eq2 cond (caar args)) ...
                    basconlist,
                    baseq2,
                    maccase_cond,
                    makelist(
                        basconcaar,
                        maccase_args)),
                makelist( // (cons progn (cdar args)) ...
                    basconcons,
                    synprogn,
                    makelist(
                        basconcdar,
                        maccase_args)),
                makelist( // (cons case (cons cond (cdr args))) 
                    basconcons,
                    maccase,
                    makelist(
                        basconcons,
                        maccase_cond,
                        makelist(
                            basconcdr,
                            maccase_args))))));

/* --
    (defmacro defun (name &rest args)
        (setf `(symbol-function ,name)
            (cons lambda args)))
 -- */

macdefun.args = makelist(
    macdefun_name,
    macdefun_rest,
    macdefun_args);

macdefun.rest =
    makelist(
        makelist(
            synquoteback,
            makelist(
                synsetf,
                makelist(
                    bassymbolfunction,
                    makelist(
                        baslocal,
                        makelist(
                            synquoteback,
                            makeunquote(
                                macdefun_name)))),
                makelist(
                    synlambda,
                    makeunquoteat(
                        macdefun_args)))));
            
/* --
    (defmacro defmacro (name &rest args)
        (setf (symbol-function name)
            (cons macro args)))
-- */

macdefmacro.args = makelist(
    macdefmacro_name,
    macdefmacro_rest,
    macdefmacro_args);

macdefmacro.rest =
    makelist(
        makelist(
            synquoteback,
            makelist(
                syninvisible,
                makelist(
                    synsetf,
                    makelist(
                        bassymbolfunction,
                        makelist(
                            baslocal,
                            makelist(
                                synquoteback,
                                makeunquote(
                                    macdefun_name)))),
                    makelist(
                        synlambda,
                        makeunquoteat(
                            macdefun_args))))));

/* --
    (defmacro setq (sym value)
        (setf (symbol-value (quoteback (unquote sym)))
            value))
-- */

macsetq.args = makelist(
    macsetq_sym,
    macsetq_value);

macsetq.rest =
    makelist(
        makelist(
            synquoteback,
            makelist(
                synsetf,
                makelist(
                    bassymbolvalue,
                    makelist(
                        synquote,
                        makeunquote(
                            macsetq_sym))),
                makeunquote(
                    macsetq_value))));

/* --
    (defmacro let (&rest args)
        (list block (cons letin args)))
-- */

maclet.args = makelist(
    maclet_rest,
    maclet_args);

maclet.rest =
    makelist(
        makelist(
            basconlist,
            synblock,
            makelist(
                basconcons,
                macletin,
                maclet_args)));

/* --
    (defmacro letin (binds &rest args)
        (if (null binds) args
            (list progn
                (cons deflvar (car binds))
                (cons letin (cons (cdr binds) args)))))
-- */

macletin.args = makelist(
    macletin_binds,
    macletin_rest,
    macletin_args);

macletin.rest = 
    makelist(
        makelist(
            synif,
            makelist(
                basnull,
                macletin_binds),
            makelist(
                synquoteback,
                makelist(
                    synprogn,
                    makeunquoteat(
                        macletin_args))),
            makelist(
                synquoteback,
                makelist(
                    makelist(
                        macdeflvar,
                        makeunquoteat(
                            makelist(
                                basconcar,
                                macletin_binds))),
                    makelist(
                        macletin,
                        makeunquote(
                            makelist(
                                basconcdr,
                                macletin_binds)),
                        makeunquoteat(
                            macletin_args))
                ))));

/* --
    (defmacro flet (&rest args)
        (block (fletin ,@args)))
 -- */

macflet.args = makelist(
    macflet_rest,
    macflet_args);

macflet.rest =
    makelist(
        makelist(
            synquoteback,
            makelist(
                synblock,
                makelist(
                    macfletin,
                    makeunquoteat(
                        macflet_args)))));

/* -- 
    (defmacro fletin (binds &rest args)
        (if (null binds) '(progn ,@args)
            '(progn (setf (symbol-function (local ',(car bind)))
                (fletin ,(cdr binds) ,@args))))
-- */

macfletin.args = makelist(
    macfletin_binds,
    macfletin_rest,
    macfletin_args);

macfletin.rest =
    makelist(
        makelist( // (if (null binds) ...
            synif,
            makelist(
                basnull,
                macletin_binds),
            makelist( // (progn ,@args)
                synquoteback,
                makelist(
                    synprogn,
                    makeunquoteat(
                        macletin_args))),
            makelist(
                synquoteback,
                makelist( // (progn ...
                    synprogn,
                    makelist( // (setf ...
                        synsetf,
                        makelist( // (symbolfunction (local (quote ,(caar binds))))
                            bassymbolfunction,
                            makelist(
                                baslocal,
                                makelist(
                                    synquote,
                                    makeunquote(
                                        makelist(
                                            basconcaar,
                                            macletin_binds))))),
                        makelist( // (lambda ,@(cdar binds))
                            synlambda,
                            makeunquoteat(
                                makelist(
                                    basconcdar,
                                    macletin_binds)))),
                    makelist( // (letin ,(cdr binds) ,@args)
                        macletin,
                        makeunquote(
                            makelist(
                                basconcdr,
                                macletin_binds)),
                        makeunquoteat(
                            macletin_args))))));

/* --
    (defmacro flet (&rest args)
        (block (fletin ,@args)))
 -- */

macmlet.args = makelist(
    macmlet_rest,
    macmlet_args);

macmlet.rest =
    makelist(
        makelist(
            synquoteback,
            makelist(
                synblock,
                makelist(
                    macmletin,
                    makeunquoteat(
                        macmlet_args)))));

/* -- 
    (defmacro mletin (binds &rest args)
        (if (null binds) '(progn ,@args)
            '(progn (setf (symbol-function (local ',(car bind)) ... )
                (mletin ,(cdr binds) ,@args))))
-- */

macmletin.args = makelist(
    macmletin_binds,
    macmletin_rest,
    macmletin_args);

macmletin.rest =
    makelist(
        makelist( // (if (null binds) ...
            synif,
            makelist(
                basnull,
                macletin_binds),
            makelist( // (progn ,@args)
                synquoteback,
                makelist(
                    synprogn,
                    makeunquoteat(
                        macletin_args))),
            makelist(
                synquoteback,
                makelist( // (progn ...
                    synprogn,
                    makelist( // (setf ...
                        synsetf,
                        makelist( // (symbolfunction (local (quote ,(caar binds))))
                            bassymbolfunction,
                            makelist(
                                baslocal,
                                makelist(
                                    synquote,
                                    makeunquote(
                                        makelist(
                                            basconcaar,
                                            macletin_binds))))),
                        makelist( // (macro ,@(cdar binds))
                            synmacro,
                            makeunquoteat(
                                makelist(
                                    basconcdar,
                                    macletin_binds)))),
                    makelist( // (letin ,(cdr binds) ,@args)
                        macletin,
                        makeunquote(
                            makelist(
                                basconcdr,
                                macletin_binds)),
                        makeunquoteat(
                            macletin_args))))));
/* --
    (defmacro defvar (sym value)
        (setf (symbol-value (global ,sym)) ,value))
 -- */

macdefvar.args = makelist(
    macdefvar_sym,
    macdefvar_value);

macdefvar.rest =
    makelist(
        makelist(
            synquoteback,
            makelist(
                synsetf,
                makelist(
                    bassymbolvalue,
                    makelist(
                        basglobal,
                        makelist(
                            synquote,
                            makeunquote(
                                macdefvar_sym)))),
                makeunquote(
                    macdefvar_value))));

/* --
    (defmacro deflvar (sym value)
        (setf (symbol-value (local ,sym)) ,value))
 -- */

macdeflvar.args = makelist(
    macdeflvar_sym,
    macdeflvar_value);

macdeflvar.rest =
    makelist(
        makelist(
            synquoteback,
            makelist(
                synsetf,
                makelist(
                    bassymbolvalue,
                    makelist(
                        baslocal,
                        makelist(
                            synquote,
                            makeunquote(
                                macdeflvar_sym)))),
                makeunquote(
                    macdeflvar_value))));

/* --
    (defmacro prog1 (&rest args)
        (let ((temp (make-symbol "")))
            `(progn (setq ,temp ,(car args))
                ,@(cdr args) ,temp)))
 -- */

macprog1.args = makelist(
    macprog1_rest,
    macprog1_args);

macprog1.rest =
    makelist(
        
        // (let ((temp (make-symbol "anonymous"))) ...
        
        makelist( 
            maclet,
            makelist(
                makelist(
                    macprog1_temp,
                    makelist(
                        bassymbolmake,
                        makestring("anonymous")))),

            // `(progn ...
            
            makelist(
                synquoteback,
                makelist(
                    synprogn,

                    // (setq ,temp ,(car args))
                    
                    makelist(
                        macsetq,
                        makeunquote(
                            macprog1_temp),
                        makeunquote(
                            makelist(
                                basconcar,
                                macprog1_args))),

                    // ,@(cdr args)

                    makeunquoteat(
                        makelist(
                            basconcdr,
                            macprog1_args)),

                    // ,temp

                    makeunquote(
                        macprog1_temp)

                ))));
                

// define basic cons methods

var basconmap = new UserFunctionClass();
var basconmap_func = makeintern("func");
var basconmap_sequence = makeintern("sequence");
var basconfilter = new UserFunctionClass();
var basconfilter_func = makeintern("func");
var basconfilter_sequence = makeintern("sequence");
var basconreduce = new UserFunctionClass();
var basconreduce_func = makeintern("func");
var basconreduce_sequence = makeintern("sequence");
var basconreducein = new UserFunctionClass();
var basconreducein_func = makeintern("func");
var basconreducein_sum = makeintern("sum");
var basconreducein_sequence = makeintern("sequence");
var basconlength = new UserFunctionClass();
var basconlength_sequence = makeintern("sequence");
var basconnth = new UserFunctionClass();
var basconnth_index = makeintern("index");
var basconnth_sequence = makeintern("sequence");
var basconappend2 = new UserFunctionClass();
var basconappend2_sequence = makeintern("sequence");
var basconappend2_sequencec = makeintern("sequencec");
var basconappend = new UserFunctionClass();
var basconappend_rest = makeintern("&rest");
var basconappend_args = makeintern("args");
var basconfindif = new UserFunctionClass();
var basconfindif_func = makeintern("func");
var basconfindif_sequence = makeintern("sequence");
var basconpositionif = new UserFunctionClass();
var basconpositionif_func = makeintern("func");
var basconpositionif_sequence = makeintern("sequence");
var basconpositionifin = new UserFunctionClass();
var basconpositionifin_func = makeintern("func");
var basconpositionifin_sequence = makeintern("sequence");
var basconpositionifin_count = makeintern("count");
var basconcopy = new UserFunctionClass();
var basconcopy_sequence = makeintern("sequence");
var basconnreverse = new UserFunctionClass();
var basconnreverse_sequence = makeintern("sequence");
var basconnreversein = new UserFunctionClass();
var basconnreversein_sequence = makeintern("sequence");
var basconnreversein_before = makeintern("before");
var basconnreversein_after = makeintern("after");

basconmap.label = "$map";
basconfilter.label = "$filter";
basconreduce.label = "$reduce";
basconreducein.label = "$reducein";
basconlength.label = "$length";
basconnth.label = "$nth";
basconappend2.label = "$append2";
basconappend.label = "$append";
basconfindif.label = "$findif";
basconpositionif.label = "$positionif";
basconcopy.label = "$copy";
basconnreverse.label = "$nreverse";
basconnreversein.label = "$nreversein";

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
        makelist( // (if (null sequence) nil ...
            synif,
            makelist(
		basnull,
                basconmap_sequence),
            nil,
            makelist( // (cons (funcall func (car sequence)) ...
                basconcons,
                makelist(
                    basfnfuncall,
                    basconmap_func,
                    makelist(
                        basconcar,
                        basconmap_sequence)),
                makelist( // (map func (cdr sequence))
                    basconmap,
                    basconmap_func,
                    makelist(
                        basconcdr,
                        basconmap_sequence)))));

/* -- 
    (defun filter (func sequence)
        (and sequence
            (if (funcall func (car sequence))
                (cons (car sequence) (filter func (cdr sequence)))
                (filter func (cdr sequence)))))
-- */

basconfilter.args = makelist(
    basconfilter_func,
    basconfilter_sequence);

basconfilter.rest = 
    makelist(
        makelist( // (if (null sequence) nil ...
            synif,
            makelist(
                basnull,
                basconfilter_sequence),
            nil,
            makelist( // (if (funcall func (car sequence)) (cons (car sequence) (filter func (cdr sequence)) ..
                synif,
                makelist(
                    basfnfuncall,
                    basconfilter_func,
                    makelist(
                        basconcar,
                        basconfilter_sequence)),
                makelist(
                    basconcons,
                    makelist(
                        basconcar,
                        basconfilter_sequence),
                    makelist(
                        basconfilter,
                        basconfilter_func,
                        makelist(
                            basconcdr,
                            basconfilter_sequence))),
                makelist( // (filter func (cdr sequence))
                    basconfilter,
                    basconfilter_func,
                    makelist(
                        basconcdr,
                        basconfilter_sequence)))));

/* -- 
    (defun reduce (func sequence)
        (if (null sequence) nil
            (if (null (cdr sequence)) (car sequence)
                (reducein func (car sequence) (cdr sequence)))))
 -- */

basconreduce.args = makelist(
    basconreduce_func,
    basconreduce_sequence);

basconreduce.rest = 
    makelist(
        makelist( // (if (null sequence) nil ...
            synif,
            makelist(
                basnull,
                basconreduce_sequence),
            nil,
            makelist( // (if (null (cdr sequence)) (car sequence) ...
                synif,
                makelist(
                    basnull,
                    makelist(
                        basconcdr,
                        basconreduce_sequence)),
                makelist(
                    basconcar,
                    basconreduce_sequence),
                makelist( // (reducein func (car sequence) (cdr sequence))
                    basconreducein,
                    basconreduce_func,
                    makelist(
                        basconcar,
                        basconreduce_sequence),
                    makelist(
                        basconcdr,
                        basconreduce_sequence)))));

/* --
    (defun reducein (func sum sequence)
        (if (null sequence) sum
            (reducein func 
                (funcall func sum (car sequence))
                (cdr sequence))))
-- */

basconreducein.args = makelist(
    basconreducein_func,
    basconreducein_sum,
    basconreducein_sequence);

basconreducein.rest = 
    makelist(
        makelist( // (if (null sequence) sum ...
            synif,
            makelist(
                basnull,
                basconreducein_sequence),
            basconreducein_sum,
            makelist( // (reducein func (funcall func sum (car sequence)) (cdr sequence))
                basconreducein,
                basconreducein_func,
                makelist(
                    basfnfuncall,
                    basconreducein_func,
                    basconreducein_sum,
                    makelist(
                        basconcar,
                        basconreducein_sequence)),
                makelist(
                    basconcdr,
                    basconreducein_sequence))));

/* --
    (defun length (sequence)
        (if (null sequence) 0
            (+ 1 (length (cdr sequence)))))
-- */

basconlength.args = makelist(
    basconlength_sequence);

basconlength.rest = 
    makelist(
        makelist( // (if (null sequence) 0 ...
            synif,
            makelist(
                basnull,
                basconlength_sequence),
            makeint(0),
            makelist( // (+ 1 (length (cdr sequence)))
                basadd,
                makeint(1),
                makelist(
                    basconlength,
                    makelist(
                        basconcdr,
                        basconlength_sequence)))));

/* -- 
    (defun nth (index sequence)
        (if (null sequence) nil
            (if (= index 0) (car sequence)
                (nth (- index 1) (cdr sequence)))))
-- */

basconnth.args = makelist(
    basconnth_index,
    basconnth_sequence);

basconnth.rest = 
    makelist(
        makelist( // (if (null sequence) nil ...
            synif,
            makelist(
                basnull,
                basconnth_sequence),
            nil,
            makelist( // (if (= index 0) (car sequence) ...
                synif,
                makelist(
                    basnumeq2,
                    basconnth_index,
                    makeint(0)),
                makelist(
                    basconcar,
                    basconnth_sequence),
                makelist( // (nth (- index 1) (cdr sequence))
                    basconnth,
                    makelist(
                        bassub,
                        basconnth_index,
                        makeint(1)),
                    makelist(
                        basconcdr,
                        basconnth_sequence)))));

/* --
    (defun append2 (sequence sequencec)
        (if (null sequence) sequencec
            (cons (car sequence)
                (append2 (cdr sequence) sequencec))))
-- */

basconappend2.args = makelist(
    basconappend2_sequence,
    basconappend2_sequencec);

basconappend2.rest = 
    makelist(
        makelist( // (if (null sequence) sequencec ...
            synif,
            makelist(
                basnull,
                basconappend2_sequence),
            basconappend2_sequencec,
            makelist( // (cons (car sequence) (append2 (cdr sequence) sequencec))
                basconcons,
                makelist(
                    basconcar,
                    basconappend2_sequence),
                makelist(
                    basconappend2,
                    makelist(
                        basconcdr,
                        basconappend2_sequence),
                    basconappend2_sequencec))));

/* --
    (defun append (&rest args)
        (reduce append2 args))
-- */

basconappend.args = makelist(
    basconappend_rest,
    basconappend_args);

basconappend.rest = 
    makelist(
        makelist( // (reduce append2 args)
            basconreduce,
            basconappend2,
            basconappend_args));

/* --
    (defun findif (func sequence)
        (if (null sequence) nil
            (if (funcall func (car sequence)) (car sequence)
                (findif func (cdr sequence)))))
-- */

basconfindif.args = makelist(
    basconfindif_func,
    basconfindif_sequence);

basconfindif.rest = 
    makelist(
        makelist( // (if (null sequence) nil ...
            synif,
            makelist(
                basnull,
                basconfindif_sequence),
            nil,
            makelist( // (if (funcall (car sequence)) (car sequence) ...
                synif,
                makelist(
                    basfnfuncall,
                    basconfindif_func,
                    makelist(
                        basconcar,
                        basconfindif_sequence)),
                makelist(
                    basconcar,
                    basconfindif_sequence),
                makelist( // (findif func (cdr sequence))
                    basconfindif,
                    basconfindif_func,
                    makelist(
                        basconcdr,
                        basconfindif_sequence)))));

/* --
    (defun positionif (func sequence)
        (positionifin func sequence 0))
-- */

basconpositionif.args = makelist(
    basconpositionif_func,
    basconpositionif_sequence);

basconpositionif.rest = 
    makelist(
        makelist( // (positionifin func sequence)
            basconpositionifin,
            basconpositionif_func,
            basconpositionif_sequence,
            makeint(0)));

/* --
    (defun positionifin (func sequence count)
        (if (null sequence) nil
            (if (funcall (car sequence)) count
                (positionifin func (cdr sequence) (+ count 1)))))
-- */

basconpositionifin.args = makelist(
    basconpositionifin_func,
    basconpositionifin_sequence,
    basconpositionifin_count);

basconpositionifin.rest = 
    makelist(
        makelist( // (if (null sequence) nil ...
	    synif,
            makelist(
                basnull,
                basconpositionifin_sequence),
            nil,
            makelist( // (if (funcall func (car sequence)) count ...
                synif,
                makelist(
                    basfnfuncall,
                    basconpositionifin_func,
                    makelist(
                        basconcar,
                        basconpositionifin_sequence)),
                basconpositionifin_count,
                makelist( // (positionifin func (cdr sequence) (+ count 1))
                    basconpositionifin,
                    basconpositionifin_func,
                    makelist(
                        basconcdr,
                        basconpositionifin_sequence),
                    makelist(
                        basadd,
                        basconpositionifin_count,
                        makeint(1))))));

/* --
    (defun copy (sequence)
        (if (null sequence) nil
            (cons (car sequence) 
                (copy (cdr sequence)))))
-- */

basconcopy.args = makelist(
    basconcopy_sequence);

basconcopy.rest = 
    makelist(
        makelist( // (if (null sequence) nil ...
            synif,
            makelist(
                basnull,
                basconcopy_sequence),
            nil,
            makelist( // (cons (car sequence) (copy (cdr sequence)))
                basconcons,
                makelist(
                    basconcar,
                    basconcopy_sequence),
                makelist(
                    basconcopy,
                    makelist(
                        basconcdr,
                        basconcopy_sequence)))));

/* --
    (defun nreverse (sequence)
        (nreversein nil sequence (cdr sequence)))
-- */

basconnreverse.args = makelist(
    basconnreverse_sequence);

basconnreverse.rest = 
    makelist(
        makelist(
            basconnreversein,
            nil,
            basconnreverse_sequence,
            makelist(
                basconcdr,
                basconnreverse_sequence)));

/* --
    (defun nreversein (before sequence after)
        (if (null sequence) nil
            (progn 
                (setf (cdr sequence) before)
                (if (null after) sequence
                    (nreversein sequence after (cdr after))))))
-- */

basconnreversein.args = makelist(
    basconnreversein_before,
    basconnreversein_sequence,
    basconnreversein_after);

basconnreversein.rest = 
    makelist(
        makelist(
            synif,
            makelist(
                basnull,
                basconnreversein_sequence),
            nil,
            makelist(
                synprogn,
                makelist(
                    synsetf,
                    makelist(
                        basconcdr,
                        basconnreversein_sequence),
                    basconnreversein_before),
                makelist(
                    synif,
                    makelist(
                        basnull,
                        basconnreversein_after),
                    basconnreversein_sequence,
                    makelist(
                        basconnreversein,
                        basconnreversein_sequence,
                        basconnreversein_after,
                        makelist(
                            basconcdr,
                            basconnreversein_after))))));

// ** test code

var source;

// source =
//     makelist(
//         synprogn,
//         makelist(
//             macdefmacro,
//             makeintern("zero"),
//             nil,
//             makeint(0)),
//         makelist(
//             makeintern("zero")));

// source = 
//     makelist(
//         maclet,
//         makelist(
//             makelist(
//                 makeintern("moco"),
//                 makestring("moco")),
//             makelist(
//                 makeintern("chibi"),
//                 makestring("chibi")),
//             makelist(
//                 makeintern("tikubonn"),
//                 makestring("tikubonn"))),
//         makelist(
//             basprint,
//             makeintern("moco")),
//         makelist(
//             basprint,
//             makeintern("chibi")),
//         makelist(
//             basprint,
//             makeintern("tikubonn")));
               
// source = 
//     makelist(
//         synsetf,
//         makelist(
//             bassymbolvalue,
//             makelist(
//                 baslocal,
//                 makelist(
//                     synquote,
//                     makeintern("moco")))),
//         makestring("moco"));

// source = 
// makelist(
//     macand,
//     makeint(1),
//     makeint(2),
//     makeint(3));

// source = 
// makelist(
//     macor,
//     makeint(1),
//     makeint(2),
//     makeint(3));

// source =
//     makelist(
//         macmlet,
//         makelist(
//             makelist(
//                 makeintern("hello"),
//                 makelist(),
//                 makelist(
//                     synquote,
//                     makelist(
//                         basprint,
//                         makestring("hello"))))),
//         makelist(
//             makeintern("hello")),
//         nil);

// source =
//     makelist(
//         macflet,
//         makelist(
//             makelist(
//                 makeintern("hello"),
//                 makelist(),
//                 makelist(
//                     basprint,
//                     makestring("hello")))),
//         makelist(
//             makeintern("hello")),
//         nil);

// source =
//     makelist(
//         macprog1,
//         makelist(
//             basprint,
//             makestring("tikubonn")),
//         makelist(
//             basprint,
//             makestring("moco")),
//         makelist(
//             basprint,
//             makestring("chibi")));

// source =
//     makelist(
//         synprogn,
//         makelist(
//             macdefvar,
//             makeintern("name"),
//             makestring("moco")),
//         makelist(
//             synblock,
//             makelist(
//                 macdeflvar,
//                 makeintern("name"),
//                 makestring("chibi")),
//             makelist(
//                 basprint,
//                 makeintern("name"))),
//         makelist(
//             basprint,
//             makeintern("name")));

// source =
//     makelist(
//         makelist(
//             maclet,
//             makelist(
//                 makeintern("none"),
//                 makelist(
//                     makeintern("name"),
//                     makestring("moco"))),
//             makelist(
//                 basprint,
//                 makeintern("none")),
//             makelist(
//                 basprint,
//                 makeintern("name"))));

// source =
//     makelist(
//         makelist(
//             maclet,
//             makelist(
//                 makelist(
//                     makeintern("name"),
//                     nil)),
//             makelist(
//                 macsetq,
//                 makeintern("name"),
//                 makestring("moco")),
//             makelist(
//                 basdebprint,
//                 makeintern("name")),
//             nil));

// source =
//     makelist(
//         makelist(
//             maclet,
//             makelist(
//                 makelist(
//                     makeintern("temp"),
//                     makelist(
//                         synquote,
//                         makeintern("temp-symbol")))),
//             nil));

// source =
//     makelist(
//         maclet,
//         makelist(
//             makelist(
//                 makeintern("name"),
//                 makestring("moco"))),
//         makelist(
//             synquoteback,
//             makelist(
//                 makeunquote(
//                     makeintern("name")))));

// try {
//     console.log("" + source.toLisp() + "");
//     // console.log("" + source.evaluatearg().toLisp() + "");
//     console.log("" + source.expandarg() + "");
// }

// catch (errorn){
//     strace.print();
//     // stracedb.print();
//     throw errorn;
// }

// source = makelist(
//     maccase, t,
//     makelist(nil, makeint(1)),
//     makelist(nil, makeint(2)),
//     makelist(t, makeint(3)));

// strace.unwindstrace(function (){
//     console.log("" + source.toLisp() + "");
//     console.log("" + source.evaluatearg().toLisp() + "");
//     stracedb.print();
// })();

// source =
//     makelist(
//         maccond,
//         makelist(nil, makeint(1)),
//         makelist(nil, makeint(2)),
//         makelist(t, makeint(3)));

// strace.unwindstrace(function (){
//     console.log("" + source.toLisp() + "");
//     console.log("" + source.evaluatearg().toLisp() + "");
//     stracedb.print();
// })();
 
// source = makelist(
//     maclet,
//     makelist(
//         makelist(
//             makeintern("name"),
//             makelist(
//                 bassymbolmake,
//                 makestring("namesym")))),
//     makelist(
//         synquoteback,
//         makelist(
//             makeint(1),
//             makeint(2),
//             makeint(3),
//             makeunquote(
//                 makeintern("name")))));

// strace.unwindstrace(function (){
//     console.log("" + source + "");
//     console.log("" + source.evaluatearg() + "");
// })();

// source = makelist(
//     maclet,
//     makelist(
//         makelist(
//             makeintern("name"),
//             makelist(
//                 bassymbolmake,
//                 makestring("namesym")))),
//     makelist(
//         synsetf,
//         makelist(
//             bassymbolvalue,
//             makeintern("name")),
//         makestring("moco")),
//     makelist(
//         basdebprint,
//         makelist(
//             bassymbolvalue,
//             makeintern("name"))));

// strace.unwindstrace(function (){
//     console.log("" + source + "");
//     console.log("" + source.evaluatearg() + "");
// })();

// source = makelist(macwhen, t, t);

// strace.unwindstrace(function (){
//     console.log("" + source + "");
//     console.log("" + source.evaluatearg() + "");
// })();

// source = makelist(macunless, nil, t);

// strace.unwindstrace(function (){
//     console.log("" + source + "");
//     console.log("" + source.evaluatearg() + "");
// })();
    
// source = makelist(
//     basconnreverse,
//     makelist(
//         basconlist,
//         makeint(1),
//         makeint(2),
//         makeint(3)));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
// })();

// source = makelist(
//     basconcopy,
//     makelist(
//         basconlist,
//         makeint(1),
//         makeint(2),
//         makeint(3)));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
// })();

// source = makelist(
//     basconpositionif,
//     makelist(
//         maclambda,
//         makelist(
//             makeintern("a")),
//         makeintern("a")),
//     makelist(
//         basconlist,
//         nil,
//         nil,
//         makestring("non nil")));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
// })();

// source = makelist(
//     basconfindif,
//     makelist(
//         maclambda,
//         makelist(
//             makeintern("a")),
//         makeintern("a")),
//     makelist(
//         basconlist,
//         nil,
//         nil,
//         makestring("non nil")));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
// })();

// source = makelist(
//     basconappend,
//     makelist(
//         basconlist,
//         makeint(1)),
//     makelist(
//         basconlist,
//         makeint(2)),
//     makelist(
//         basconlist,
//         makeint(3)),
//     makelist(
//         basconlist,
//         makeint(4)));

// strace.unwindstrace(function (){
//     console.log("" + source + "");
//     console.log("" + source.evaluatearg() + ""); // ** error
// })();

// source = makelist(
//     basconnth,
//     makeint(1),
//     makelist(
//         basconlist,
//         makeint(1),
//         makeint(2),
//         makeint(3)));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
// })();

// source = 
//     makelist(
//         basconlength,
//         makelist(
//             basconlist,
//             makeint(1),
//             makeint(2),
//             makeint(3)));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
// })();

// source = 
//     makelist(
//         basconreduce,
//         basadd,
//         makelist(
//             basconlist,
//             makeint(1),
//             makeint(10),
//             makeint(100)));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
// })();

// source = 
//     makelist(
//         basconfilter,
//         makelist(
//             maclambda,
//             makelist(
//                 makeintern("a")),
//             makeintern("a")),
//         makelist(
//             basconlist,
//             nil,
//             makeint(1),
//             nil,
//             makeint(2),
//             nil,
//             makeint(3)));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
// })();

// source = 
//     makelist(
//         basconmap,
//         makelist(
//             maclambda,
//             makelist(
//                 makeintern("a")),
//             makelist(
//                 basadd,
//                 makeintern("a"),
//                 makeint(10))),
//         makelist(
//             basconlist,
//             makeint(1),
//             makeint(2),
//             makeint(3)));

// strace.unwindstrace(function (){
//     console.log(source + "");
//     console.log(source.evaluatearg() + "");
// })();

// source = 
//     makelist(
//         synprogn,
//         makelist(
//             debprint,
//             makelist(
//                 macand)),
//         makelist(
//             debprint,
//             makelist(
//                 macand,
//                 makeint(1),
//                 nil,
//                 makeint(3))),
//         makelist(
//             debprint,
//             makelist(
//                 macand,
//                 makeint(1),
//                 makeint(2),
//                 makeint(3))));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
//     console.log(source.expandarg() + "");
// })();

// source = 
//     makelist(
//         synprogn,
//         makelist(
//             debprint,
//             makelist(
//                 macor)),
//         makelist(
//             debprint,
//             makelist(
//                 macor,
//                 nil,
//                 nil,
//                 makeint(3))),
//         makelist(
//             debprint,
//             makelist(
//                 macor,
//                 makeint(1),
//                 makeint(2),
//                 makeint(3))));

// strace.unwindstrace(function (){
//     // console.log(source + "");
//     console.log(source.evaluatearg() + "");
//     console.log(source.expandarg() + "");
// })();
