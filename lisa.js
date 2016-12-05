
// argumentstoarray

function slice (sequence, beginning, end){
    return Array.prototype.slice.call(sequence, beginning, end);
};

// get object keys

function keys (some){
    return Object.keys(some);
};

// log

function Log (size){
    this.log = [];
    this.count = 0;
    this.size = size || 1024;
};

Log.prototype.clear = function (){
    this.log = [];
};

Log.prototype.push = function (message){
    if (this.size < this.log.length)
        this.log.shift();
    this.log.push((this.count++) + ": " + message);
};

Log.prototype.print = function (message){
    var index;
    for (index = 0; index < this.log.length; index++)
        console.log(this.log[index]);
};

// strace

function Strace (size){
    this.strace = [];
    this.size = size || 1024;
};

Strace.prototype.push = function (message){
    if (this.size < this.strace.length)
        this.strace.shift();
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

Strace.prototype.willstrace = function (message, func){
    
    var self = this;
    
    return function willstrace_closure (){

        var temp;
        var index;
        var source;

        source = message + this.toLisp();
        index = self.push(source);
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

function makeexpanded(source){
    return new Expanded(source);
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
    this.cons = cons;
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
    return this.get().setcar(value);
};

ConsCarReferenceClass.prototype.onexpandarg = function (){
    return new Expanded(this.cons.expandarg() + ".car");
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
    return this.get().setcdr(value);
};

ConsCdrReferenceClass.prototype.onexpandarg = function (){
    return new Expanded(this.cons.expandarg() + ".cdr");
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
    return this.value.getvalue();
};

SymbolValueReferenceClass.prototype.set = function (value){
    return this.value.setvalue(value);
};

SymbolValueReferenceClass.prototype.onexpandarg = function (){
    return this.value.getvarname();
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
    return this.value.getfuncname();
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

ArrayClass.prototype.get = function (index){
    return this.value[index];
};

ArrayClass.prototype.set = function (index, value){
    this.value[index] = value;
    return value;
};

ArrayClass.prototype.length = function (){
    return this.value.length;
};

ArrayClass.prototype.toArray = function (){ return this.value; };
ArrayClass.prototype.toPlain = function (){ return "" + this.value.map(toplain).join(",") + ""; };
ArrayClass.prototype.toString = function (){ return "[" + this.value.map(tostring).join(",") + "]"; };
ArrayClass.prototype.toLisp = function (){ return "#(" + this.value.map(tolisp).join(" ") + ")"; };

// class array class 
//     <- array class

function ClassArrayClass (array){
    this.value = array;
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
    return "," + this.value.toLisp();
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

SpecialFunctionClass.prototype.evaluate =
    strace.willstrace("evaluate special function = ", SpecialFunctionClass.prototype.evaluate);

SpecialFunctionClass.prototype.expand =
    strace.willstrace("expand special function = ", SpecialFunctionClass.prototype.expand);

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

    var bound;
    var consa, consb;

    bound = "";
    consa = this.args;
    consb = ConsClass.toCons(arguments);

    for (; consa != nil && consb != nil;
         consa = consa.cdr, consb = consb.cdr){
        if (consa.car == rest) break;
        if (consa.car == optional) break;
        bound =
            makecons(
                makelist(
                    synsetf,
                    makelist(
                        bassymbolvalue,
                        makelist(
                            basdecvariable,
                            makelist(
                                synquote,
                                consa.car))),
                    consb.car),
                bound);
    };

    if (consa.car == makeintern("&optional")){
        consa = consa.cdr;
        for (; consa != nil && consb != nil;
             consa = consa.cdr, consb = consb.cdr){
            if (consa.car == rest) break;

            var sym =
                    consa.car instanceof ConsClass ?
                    consa.car.car : consa.car;

            var value =
                    consa.car instanceof ConsClass ?
                    consb ? consb.car : consa.car.cdr.car :
                consb ? consb.car : nil;

            bound = makecons(
                makelist(
                    synsetf,
                    makelist(
                        bassymbolvalue,
                        makelist(
                            basdecvariable,
                            makelist(
                                synquote,
                                sym))),
                    value),
                bound);
        };
    };

    if (consa.car == makeintern("&rest")){
        consa = consa.cdr;
        bound = makecons(
            makelist(
                synsetf,
                makelist(
                    bassymbolvalue,
                    makelist(
                        basdecvariable,
                        makelist(
                            synquote,
                            consa.car))),
                makecons(
                    basconlist,
                    consb)),
            bound);
    };

    var formula = makelist(
        synblock,
        makecons(synprogn, bound.reverse()),
        makecons(synprogn, this.rest));

    var message = "user function <- " + formula.toLisp();
    strace.push(message);
    stracedb.push(message);
    
    return formula.evaluatearg();
};

UserFunctionClass.prototype.onexpandarg = function (){

    var cons = this.args;
    var index = 0;
    var source = "";

    inp.nest();

    for (;cons != nil; cons = cons.cdr){
        if (cons.car == makeintern("&rest")) break;
        if (cons.car == makeintern("&optional")) break;
        inp.scopevar.addf(cons.car, cons.car);
        source += "var " + cons.car + "=arguments[" + (index++) + "];";
    };

    if (cons.car == makeintern("&optional")){
        cons = cons.cdr;
        for (;cons != nil; cons = cons.cdr){
            if (cons.car == makeintern("&rest")) break;
            inp.scopevar.addf(cons.car, cons.car);
            source += "var " + cons.car + "=arguments[" + (index++) + "]||null;";
        };
    };

    if (cons.car == makeintern("&rest")){
        cons = cons.cdr;
        inp.scopevar.addf(cons.car, cons.car);
        source += "var " + cons.car + "=Array.prototype.slice.call(arguments, " + (index++) + "]";
    };

    source = new Expanded(
        "function(){" + source +
            "return " + makecons(synprogn, this.rest).expandarg() + "}"); // build source

    inp.exit();
    
    return source;
};

// macro class
//     <- atom class

function MacroClass (){}

MacroClass.prototype = 
    Object.create(CallableClass.prototype);

// MacroClass.prototype.label = "<#macro instance>";

MacroClass.prototype.toString = function (){
    return nil.toString();
};

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

// PrimitiveMacroClass.prototype.label = "<#primitive macro instance>";

// user macro class
//     <- macro class

function UserMacroClass (args, rest){
    this.args = args || null;
    this.rest = rest || null;
};

UserMacroClass.prototype = 
    Object.create(MacroClass.prototype);

UserMacroClass.prototype.toLisp = function (){
    return makelist(
        synmacro,
        this.args,
        this.rest).toLisp();
};

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
                            basdecvariable,
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
                    makecons(makeintern("defvar"),
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
                            basdecvariable,
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

// ** define class

var logmadesymbols = new Log();

function SymbolFamilyClass (name){
    
    if (name instanceof StringClass == false &&
        name instanceof NilClass == false)
        throw new Error("" + name + " is should be nil or string instance.");

    logmadesymbols.push(name.toLisp()); // logging made symbols
    
    this.name = name;
};

SymbolFamilyClass.prototype =
    Object.create(AtomClass.prototype);

SymbolFamilyClass.prototype.toString = function (){return this.name.toPlain();};
SymbolFamilyClass.prototype.toPlain = function (){return this.name.toPlain();};
SymbolFamilyClass.prototype.toLisp = function (){return this.toString();};
SymbolFamilyClass.prototype.getvalue = function (){throw new Error("getvalue has not defined yet in symbol family class.");};
SymbolFamilyClass.prototype.getfunc = function (){throw new Error("getfunc has not defined yet in symbol family class.");};
SymbolFamilyClass.prototype.getfunce = function (){throw new Error("getfunce has not defined yet in symbol family class.");};
SymbolFamilyClass.prototype.setvalue = function (){throw new Error("setvalue has not defined yet in symbol family class.");};
SymbolFamilyClass.prototype.getvaluee = function (){throw new Error("getvaluee has not defined yet in symbol family class.");};
SymbolFamilyClass.prototype.setfunc = function (){throw new Error("setfunc has not defined yet in symbol family class.");};
SymbolFamilyClass.prototype.getvarname = function (){throw new Error("getvarname has not defined yet in symbol family class");};
SymbolFamilyClass.prototype.getfuncname = function (){throw new Error("getfuncname has not defined yet in symbol family class");};
SymbolFamilyClass.prototype.set = function (){return this.setvalue.apply(this, arguments);};
SymbolFamilyClass.prototype.get = function (){return this.getvalue.apply(this, arguments);};
SymbolFamilyClass.prototype.onevaluatearg = function (){return this.getvalue();};
SymbolFamilyClass.prototype.onexpandarg = function (){throw new Error("onexpandarg has not defined yet in symbol family class.");};

SymbolFamilyClass.prototype.onevaluate = function (){
    var found = this.getfunce();
    return found.evaluate.apply(found, arguments);
};

SymbolFamilyClass.prototype.onexpand = function (){ // ** should check again

    // find function with this symbol
    // from current scope.
    
    var found = this.getfunc();

    // if found was found, is macro, is special function
    // then expand with found.

    if (found &&
        found instanceof MacroClass ||
        found instanceof SpecialFunctionClass)
        return found.expand.apply(found, arguments);

    // if found was not found, not macro, not special function
    // then expand with this symbol name.
    
    return new Expanded(
        "" + this.expandarg() + "(" +
            slice(arguments).map(expandarg).join(",") + ")");
};

// ** define class

function SymbolClass (){
    SymbolFamilyClass.apply(this, arguments);
};

SymbolClass.prototype =
    Object.create(SymbolFamilyClass.prototype);

SymbolClass.prototype.setvalue = function (value){
    return inp.scopevar.set(this, value);
};

SymbolClass.prototype.setfunc = function (value){
    return inp.scopefunc.set(this, value);
};

SymbolClass.prototype.getvalue = function (){ // search a value from current scope.
    return inp.scopevar.get(this);
};

SymbolClass.prototype.getfunc = function (){ // search a function from current scope.
    return inp.scopefunc.get(this);
};

SymbolClass.prototype.getvaluee = function (){
    var found = this.getvalue();
    if (found == null)
        throw new Error("symbol " + this + " has no value.");
    return found;
};

SymbolClass.prototype.getfunce = function (){
    var found = this.getfunc();
    if (found == null)
        throw new Error("symbol " + this + " has no function.");
    return found;
};

SymbolClass.prototype.getvarname = function (){
    return inp.scopevar.getvarname(this);
};

SymbolClass.prototype.getfuncname = function (){
    return inp.scopefunc.getvarname(this);
};

SymbolClass.prototype.onexpandarg = function (){
    return new Expanded(this.toString());
};

// ** define class

var interneds = [];

function InternSymbolClass (name){

    var index;
    for (index = 0; index < interneds.length; index++)
        if (interneds[index].name.toString() == name.toString())
            return interneds[index];

    var sym = new SymbolClass(name);
    interneds.push(sym);
    inp.scopevar.add(sym, null);
    inp.scopefunc.add(sym, null);
    return sym;
};

InternSymbolClass.prototype =
    Object.create(SymbolClass.prototype);

function stoc (source){
    return new CharClass(
        source.charCodeAt());
};

function makeintern(name){
    return new InternSymbolClass(
        new StringClass(
            slice(name).map(stoc)));
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

StreamClass.prototype.toString = function (){
    return "#stream direction: " +
        (this.isin() == false ? "r" : "-") + 
        (this.isout() == false ? "w" : "-");
};

StreamClass.prototype.toLisp = function (){
    return this.toString();
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
    return this.iseof() ? nil : this.source.get(this.index);
};

StringStreamClass.prototype.get = function (){
    this.shouldin();
    return this.iseof() ? nil : this.source.get(this.index++);
};

StringStreamClass.prototype.put = function (charInstance){
    this.shouldout();
    return this.source.push(charInstance);
};

StringStreamClass.prototype.toString = function (){
    return StreamClass.prototype.toString.apply(this, arguments) +
        " :seek " + this.index.toString() +
        " :source " + this.source.toString();
};

// ** alias

function makestrstreamin (source){ return new StringStreamClass(StreamClass.direction.input, makestring(source)); };
function makestrstreamout (source){ return new StringStreamClass(StreamClass.direction.output, makestring("")); };

// obarray pair class
//     <- native, function class

function ObarrayPair (name, value){

    if (name instanceof SymbolFamilyClass == false) throw new Error("" + name + " should be symbol family instance.");
    if (value && value instanceof AtomClass == false) throw new Error("" + value + " should be atom instance.");

    this.name = name;
    this.value = value || null;
};

ObarrayPair.prototype.toString = function (){ // ** for debugging
    return "(" + this.name.toLisp() + " = " +
        (this.value && this.value.toLisp()) + ")";
};

// variable obarray pair class
//     <- obarray pair class

function VariableObarrayPair (){
    ObarrayPair.apply(this, arguments);
    this.varname = null;
};

VariableObarrayPair.prototype =
    Object.create(ObarrayPair.prototype);

VariableObarrayPair.prototype.getvarname = function (){
    if (this.varname == null)
        this.varname = inp.namegen.generate();
    return this.varname;
};

// obarray class
//     <- native, function class

function Obarray (){
    this.obarray = [];
}

Obarray.prototype.find = function (name){
    var index;
    for (index = 0; index < this.obarray.length; index++)
        if (this.obarray[index].name.toPlain() == name.toPlain())
            return this.obarray[index];
    return null;
};

Obarray.prototype.finde = function (name){
    var found;
    if ((found = this.find(name)) == null){
        throw new Error("" + name + " was not found.");}
    return found;
};

Obarray.prototype.get = function (name){
    return this.finde(name).value;
};

Obarray.prototype.getvarname = function (name){
    return this.finde(name).getvarname();
};

Obarray.prototype.set = function (name, value){ // return an argument of name.
    var found;
    found = this.finde(name);
    return found.value = value, name;
};

Obarray.prototype.add = function (name, value){ // return an argument of name.
    if (this.find(name) == null)
        return this.addf(name, value);
    return this.set(name, value);
};

Obarray.prototype.addf = function (name, value){ // return an argument of name. ** this method for add()
    this.obarray.unshift(
        new VariableObarrayPair(name, value));
    return this.set(name, value);
};

Obarray.prototype.length = function (){
    var count, current;
    for (count = 0, current = this; current; current = current.parent)
        count += 1;
    return count;
};

Obarray.prototype.toString = function (){ // ** for debugging
    var source, index, current;
    for (source = "", index = 0, current = this; current; current = current.parent)
        source += "" + (index++) + " deepness " + current.obarray.length + " items " + this.obarray.join(" ") + "\n";
    return source;
};

// obarrays class
//     <- native, function class

function Obarrays (parent){
    this.parent = parent || null;
    this.obarray = new Obarray();
};

Obarrays.prototype.find = function (name){
    var current, found;
    for (current = this; current; current = current.parent)
        if ((found = current.obarray.find(name)))
            return found;
    return null;
};

Obarrays.prototype.finde =
    Obarray.prototype.finde;

Obarrays.prototype.get =
    Obarray.prototype.get;

Obarrays.prototype.getvarname =
    Obarray.prototype.getvarname;

Obarrays.prototype.set =
    Obarray.prototype.set;

Obarrays.prototype.add =
    Obarray.prototype.add;

Obarrays.prototype.addf = function (name, value){
    return this.obarray.addf(name, value);
};

Obarrays.prototype.length = function (){
    var count, current;
    for (count = 0, current = this; current; current = current.parent)
        count += current.obarray.length();
    return count;
};

Obarrays.prototype.nest = function (){
    return new Obarrays(this);
};

Obarrays.prototype.exit = function (){
    return this.parent;
};

Obarrays.prototype.toString = function (){ // ** for debugging
    var source, index, current;
    for (source = "", index = 0, current = this; current; current = current.parent)
        source += "" + (index++) + " deepness " + current.obarray.toString();
    return source;
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
    this.scopevar = new Obarrays();
    this.scopefunc = new Obarrays();
    this.readerscope = new ReaderScope();
    this.namegen = new NameGenerator("abcdefghijklmnopqrstuvwxyz");
};

Interpreter.prototype.nest = function (){
    this.scopevar = this.scopevar.nest();
    this.scopefunc = this.scopefunc.nest();
};

Interpreter.prototype.exit  =function (){
    this.scopevar = this.scopevar.exit();
    this.scopefunc = this.scopefunc.exit();
};

var inp = new Interpreter();

// ** alias

function onlisp (func){
    return function onlisp_closure (){
        try {
            strace.clear();
            return func.apply(this, arguments);
        }
        catch (errorn){
            strace.print();
            console.log("variable bindings = \n" + inp.scopevar.toString() + "");
            console.log("function bindings = \n" + inp.scopefunc.toString() + "");
            throw errorn;
        };
    };
};

function readlisp (source){
    return rdread.evaluate(
        makestrstreamin(source));
};

function evallisp (source){
    return readlisp(source)
        .evaluatearg();
};

function explisp (source){
    return readlisp(source)
        .expandarg();
};

readlisp = onlisp(readlisp);
evallisp = onlisp(evallisp);
explisp = onlisp(explisp);

// define argument options

var rest = makeintern("&rest");
var optional = makeintern("&optional");

// threre defining the native stream methods
// there are not able to expand to javascript code. ** should check again

var streamlook = new PrimitiveFunctionClass();
var streamget = new PrimitiveFunctionClass();
var streamput = new PrimitiveFunctionClass();

streamlook.label = "$stream-look";
streamget.label = "$stream-get";
streamput.label = "$stream-put";

streamlook.onevaluate = function (stream){
    return stream.look();
};

streamget.onevaluate = function (stream){
    return stream.get();
};

streamput.onevaluate = function (element, stream){
    return stream.put(element);
};

streamlook.onexpand = function (){ throw new Error("" + this + " cannot expand to the source code."); };
streamget.onexpand = function (){ throw new Error("" + this + " cannot expand to the source code."); };
streamput.onexpand = function (){ throw new Error("" + this + " cannot expand to the source code."); };

// define reader methods
// there has no expand method

var rdignoreindent = new PrimitiveFunctionClass();
var rdread = new PrimitiveFunctionClass();
var rdreaddefault = new PrimitiveFunctionClass();
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
    while (stream.isalive() && stream.look().value == " ".charCodeAt())
        stream.get();
    return nil;
};

rdread.onevaluate = function (stream){
    
    rdignoreindent.evaluate(stream);
    
    var rdscope, rdscopea;
    var charactor, charactora;
    
    for (rdscope = inp.readerscope, charactor = nil; stream.isalive();){
        charactora = stream.look();
        rdscopea = rdscope.get(charactora);
        if (rdscopea == null) break;
        charactor = stream.get();
        rdscope = rdscopea;
    };

    if (rdscope.getcurrent() == null)
        throw new Error("reader macro error.");
    return rdscope.getcurrent().evaluate(charactor, stream);
};

rdreaddefault.onevaluate = function (charactor, stream){ // ** should check

    var charactora;
    
    switch ((charactora = stream.get()).toString()){

        // combination
        
    case ",":
        switch ((stream.look()).toString()){
        case "@": return rdreadunquoteat.evaluate(stream.get(), stream);
        default: return rdreadunquote.evaluate(charactora, stream);
        }
        
        // single
        
    case "'": return rdreadquote.evaluate(charactora, stream);
    case "`": return rdreadquoteback.evaluate(charactora, stream);
    case '"': return rdreadstring.evaluate(charactora, stream);
    case "(": return rdreadopenbrace.evaluate(charactora, stream);
    case ")": return rdreadclosebrace.evaluate(charactora, stream);
    case "-": return rdreadminus.evaluate(charactora, stream);
    case "0":
    case "1":
    case "2":
    case "3":
    case "4":
    case "5":
    case "6":
    case "7":
    case "8":
    case "9": return rdreadnumber.evaluate(charactora, stream);
    default: return rdreadintern.evaluate(charactora, stream);};
};

rdreadintern.onevaluate = function (charactor, stream){ // ** should check

    var name =
            makelist(
                synif,
                charactor,
                charactor,
                makeexpanded(""))
            .evaluatearg()
            .toString();
    
    while (stream.isalive() &&
           stream.look().toString() != " " && 
           stream.look().toString() != "(" &&
           stream.look().toString() != ")")
        name += stream.get().toString();

    if (name.length == 0)
        return nil;

    return makeintern(name);
};

rdreadminus.onevaluate = function (stream){ // ** should check
    return makelist(
        basnumsub2,
        makeint(0),
        rdread.evaluate(stream));
};

rdreadnumber.onevaluate = function (charactor, stream){ // ** should check

    var num = charactor.toString();

    while (stream.isalive() &&
           stream.look().toString() == "0" ||
           stream.look().toString() == "1" ||
           stream.look().toString() == "2" ||
           stream.look().toString() == "3" ||
           stream.look().toString() == "4" ||
           stream.look().toString() == "5" ||
           stream.look().toString() == "6" ||
           stream.look().toString() == "7" ||
           stream.look().toString() == "8" ||
           stream.look().toString() == "9" ||
           stream.look().toString() == ".")
        num += stream.get().toString();

    return (num.indexOf(".") >= 0)?
        makefloat(parseFloat(num)):
        makeint(parseInt(num));
};

rdreadstring.onevaluate = function (charactor, stream){
    var source, charactora;
    for (source = ""; stream.isalive() && (charactora = stream.get()).toString() == '"';)
        source += charactora;
    return makestring(source);
};

rdreadopenbrace.onevaluate = function (charactor, stream){

    var cons;
    var consa;

    cons = nil;

    while (true){
        
        if (stream.isalive() == false)
            throw new Error("could not find close brace.");

        consa = rdread.evaluate(stream);
        
        if (consa == rdreadclosebrace_unique) break;

        cons = makecons(consa, cons);
    }

    return cons.reverse();
};

rdreadclosebrace.onevaluate = function (charactor, stream){
    return rdreadclosebrace_unique;
};

rdreadquote.onevaluate = function (charactor, stream){
    return makelist(synquote, rdread.evaluate(stream));
};

rdreadquoteback.onevaluate = function (charactor, stream){
    return makelist(synquoteback, rdread.evaluate(stream));
};

rdreadunquote.onevaluate = function (charactor, stream){
    return new UnQuoteClass(rdread.evaluate(stream));
};

rdreadunquoteat.onevaluate = function (charactor, stream){
    return new UnQuoteAtClass(rdread.evaluate(stream));
};

rdreadchar.onevaluate = function (charactor, stream){
    return new CharClass(stream.get());
};

rdreadpre.onevaluate = function (charactor, stream){
    var formula = rdread.evaluate(stream);
    return formula.evaluatearg();
};

rdreadnative.onevaluate = function (charactor, stream){
    var sym = rdread.evaluate(stream);
    return new Expanded(sym.name.toPlain());
};

inp.readerscope.setcurrent(rdreaddefault);
// inp.readerscope.setcurrent(rdreadintern);
// inp.readerscope.dig("'").setcurrent(rdreadquote);
// inp.readerscope.dig("`").setcurrent(rdreadquoteback);
// inp.readerscope.dig(",").setcurrent(rdreadunquote);
// inp.readerscope.dig(",").dig("@").setcurrent(rdreadunquoteat);
// inp.readerscope.dig("-").setcurrent(rdreadminus);
// inp.readerscope.dig('"').setcurrent(rdreadstring);
// inp.readerscope.dig("(").setcurrent(rdreadopenbrace);
// inp.readerscope.dig(")").setcurrent(rdreadclosebrace);
// inp.readerscope.dig("0").setcurrent(rdreadnumber);
// inp.readerscope.dig("1").setcurrent(rdreadnumber);
// inp.readerscope.dig("2").setcurrent(rdreadnumber);
// inp.readerscope.dig("3").setcurrent(rdreadnumber);
// inp.readerscope.dig("4").setcurrent(rdreadnumber);
// inp.readerscope.dig("5").setcurrent(rdreadnumber);
// inp.readerscope.dig("6").setcurrent(rdreadnumber);
// inp.readerscope.dig("7").setcurrent(rdreadnumber);
// inp.readerscope.dig("8").setcurrent(rdreadnumber);
// inp.readerscope.dig("9").setcurrent(rdreadnumber);
// inp.readerscope.dig("?").setcurrent(rdreadchar);
// inp.readerscope.dig("@").dig(".").setcurrent(rdreadpre);
// inp.readerscope.dig("@").dig("@").setcurrent(rdreadnative);

// define stanrad source code

var standard = "";

// define syntax functions

var synif = new SpecialFunctionClass();
var synblock = new SpecialFunctionClass();
var synprogn = new SpecialFunctionClass();
var synsetf = new SpecialFunctionClass();
var synquote = new SpecialFunctionClass();
var synquoteback = new SpecialFunctionClass();
var synlambda = new SpecialFunctionClass();
var synmacro = new SpecialFunctionClass();

inp.scopefunc.add(makeintern("if"), synif);
inp.scopefunc.add(makeintern("block"), synblock);
inp.scopefunc.add(makeintern("progn"), synprogn);
inp.scopefunc.add(makeintern("setf"), synsetf);
inp.scopefunc.add(makeintern("quote"), synquote);
inp.scopefunc.add(makeintern("quoteb"), synquoteback);
inp.scopefunc.add(makeintern("lambda"), synlambda);
inp.scopefunc.add(makeintern("macro"), synmacro);

synif.label = "$if";
synblock.label = "$block";
synprogn.label = "$progn";
synsetf.label = "$setf";
synquote.label = "$quote";
synquoteback.label = "$quoteback";
synlambda.label = "$lambda";
synmacro.label = "$macro";

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
    inp.nest();
    var temp = synprogn.evaluate.apply(synprogn, arguments);
    inp.exit();
    return temp;
};

synblock.onexpand = function (){
    inp.nest();
    var temp = synprogn.expand.apply(synprogn, arguments);
    inp.exit();
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
    return this.evaluate.apply(this, arguments).expanddata();
};

synquoteback.onevaluate = function (some){

    var message  = "unquoteback " + some.toLisp() + " = " + some.clone().toLisp() + "";
    strace.push(message);
    stracedb.push(message);
    
    return some.clone();
};

synquoteback.onexpand = function (some){
    return this.evaluate.apply(this, arguments).expanddata();
};

synlambda.onevaluate = function (args){
    return new UserFunctionClass(args, ConsClass.toCons(slice(arguments, 1)));
};

synlambda.onexpand = function (args){
    return this.evaluate.apply(this, arguments).expandarg();
};

synmacro.onevaluate = function (args){
    return new UserMacroClass(args, ConsClass.toCons(slice(arguments, 1)));
};

synmacro.onexpand = function (args){
    return this.evaluate.apply(this, arguments).expandarg();
};

// define basic scope methods

var basdecvariable = new PrimitiveFunctionClass();
var basdecfunction = new PrimitiveFunctionClass();

basdecvariable.label = "$declare-variable";
basdecfunction.label = "$declare-function";

inp.scopefunc.add(makeintern("declare-variable"), basdecvariable);
inp.scopefunc.add(makeintern("declare-function"), basdecfunction);

var logdecvariable = new Log();
var logdecfunction = new Log();

basdecvariable.onevaluate = function (sym){
    logdecvariable.push("(declare-variable " + sym + ")");
    return inp.scopevar.addf(sym);
};

basdecfunction.onevaluate = function (sym){
    logdecfunction.push("(declare-function " + sym + ")");
    return inp.scopefunc.addf(sym);
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

inp.scopefunc.add(makeintern("symbol-function"), bassymbolfunction);
inp.scopefunc.add(makeintern("symbol-value"), bassymbolvalue);
inp.scopefunc.add(makeintern("symbol-name"), bassymbolname);
inp.scopefunc.add(makeintern("intern"), bassymbolintern);
inp.scopefunc.add(makeintern("make-symbol"), bassymbolmake);

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
    return new SymbolClass(name);
};

// define basci boolean

standard +=
    makelist(
        synsetf,
        makelist(
            bassymbolvalue,
            makelist(
                basdecvariable,
                makelist(
                    synquote,
                    makeintern("nil")))),
        nil)
    .expandarg();

standard +=
    makelist(
        synsetf,
        makelist(
            bassymbolvalue,
            makelist(
                basdecvariable,
                makelist(
                    synquote,
                    makeintern("t")))),
        t)
    .expandarg();

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

var basfnfuncall = new SpecialFunctionClass();
var basfnapply = new SpecialFunctionClass();

basfnfuncall.label = "$funcall";
basfnapply.label = "$apply";

inp.scopefunc.add(makeintern("funcall"), basfnfuncall);
inp.scopefunc.add(makeintern("apply"), basfnapply);

basfnfuncall.onevaluate = function (func){
    var callable = func.evaluatearg();
    return callable.evaluate.apply(callable, slice(arguments, 1));
};

basfnfuncall.onexpand = function (func){
    return func.expand.apply(func, slice(arguments, 1));
};

basfnapply.onevaluate = function (func, args){
    var callable = func.evaluatearg();
    return callable.evaluate.apply(callable, args.toArray());
};

basfnapply.onexpand = function (func, args){
    return func.expand.apply(func, ConsClass.toCons(args));
};

// define basic debug methods

var basprint = new PrimitiveFunctionClass();
var basstrace = new PrimitiveFunctionClass();
var basstracedb = new PrimitiveFunctionClass();

basprint.label = "$print";
basstrace.label = "$strace";
basstracedb.label = "$stracedb";

inp.scopefunc.add(makeintern("print"), basprint);
inp.scopefunc.add(makeintern("strace"), basstrace);
inp.scopefunc.add(makeintern("stracedb"), basstracedb);

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

baseq2.label = "$eq2";

baseq2.onevaluate = function (a,b){
    return a == b ? t : nil;
};

baseq2.onexpand = function (a,b){
    return new Expanded("(" + a + "===" + b + ")");
};

// define basic cons methods

var basconconsp = new PrimitiveFunctionClass();
var basconcons = new PrimitiveFunctionClass();
var basconcar = new PrimitiveFunctionClass();
var basconcdr = new PrimitiveFunctionClass();
var basconlist = new PrimitiveFunctionClass();

inp.scopefunc.add(makeintern("cons"), basconcons);
inp.scopefunc.add(makeintern("car"), basconcar);
inp.scopefunc.add(makeintern("cdr"), basconcdr);
inp.scopefunc.add(makeintern("list"), basconlist);

basconcons.label = "$cons";
basconcar.label = "$car";
basconcdr.label = "$cdr";
basconlist.label = "$list";

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

basconcons.onexpand = function (car, cdr){
    return new Expanded("new Cons(" + car + "," + cdr + ")");
};

basconcar.onexpand = function (cons){
    return this.evaluate.apply(this, arguments).expandarg();
};

basconcdr.onexpand = function (cons){
    return this.evaluate.apply(this, arguments).expandarg();
};

basconlist.onevaluate = function (){
    return new ConsReferenceClass(
        ConsClass.toCons(arguments));
};

// define basic boolean

standard +=
    makelist(
        synsetf,
        makelist(
            bassymbolvalue,
            makelist(
                basdecvariable,
                makelist(
                    synquote,
                    makeintern("nil")))),
        nil)
    .expandarg();

standard +=
    makelist(
        synsetf,
        makelist(
            bassymbolvalue,
            makelist(
                basdecvariable,
                makelist(
                    synquote,
                    makeintern("t")))),
        t)
    .expandarg();

// define defun, defmacro

evallisp("(setf (symbol-function 'defun) (macro (name &rest rest) `(setf (symbol-function (declare-function (quote ,name))) (lambda ,@rest))))");
evallisp("(setf (symbol-function 'defmacro) (macro (name &rest rest) `(setf (symbol-function (declare-function (quote ,name))) (macro ,@rest))))");
evallisp("(defmacro defvar (name value) `(setf (symbol-value (declare-variable (quote ,name))) ,value))");
evallisp("(defmacro setq (name value) `(setf (symbol-value (quote ,name)) ,value))");
evallisp("(defmacro set-car (con value) `(setf (car ,con) ,value))");
evallisp("(defmacro set-cdr (con value) `(setf (cdr ,con) ,value))");

// define cons methods

evallisp("(defmacro caar (con) `(car (car ,con)))");
evallisp("(defmacro cdar (con) `(cdr (car ,con)))");
evallisp("(defmacro cadr (con) `(car (cdr ,con)))");
evallisp("(defmacro cddr (con) `(cdr (cdr ,con)))");
evallisp("(defmacro null (value) `(if ,value nil t))");
evallisp("(defmacro not (value) `(if ,value nil t))");
evallisp("(defmacro and (&rest rest) (if (null rest) t (if (null (cdr rest)) (car rest) `(if ,(car rest) (and ,@(cdr rest)) nil))))");
evallisp("(defmacro or (&rest rest) (if (null rest) nil (if (null (cdr rest)) (car rest) `(if ,(car rest) ,(car rest) (or ,@(cdr rest))))))");
evallisp("(defmacro when (status &rest rest) `(if ,status (progn ,@rest) nil))");
evallisp("(defmacro unless (status &rest rest) `(if ,status nil (progn ,@rest)))");
evallisp("(defmacro cond (&rest rest) (if (null rest) nil `(if ,(caar rest) (progn ,@(cdar rest)) (cond ,@(cdr rest)))))");
evallisp("(defmacro push (value sym) `(setq ,sym (cons ,value ,sym)))");
evallisp("(defmacro pop (sym) `(prog1 (car ,sym) (setq ,sym (cdr ,sym))))");
standard += explisp("(defun copy (seq) (cons (car seq) (copy (cdr seq))))");
standard += explisp("(defun map (func seq) (and seq nil (cons (funcall func (car seq)) (map func (cdr seq)))))");
standard += explisp("(defun filter (func seq) (and seq nil (if (funcall func (car seq)) (cons (car seq) (filter func (cdr seq))) (filter func (cdr seq)))))");
standard += explisp("(defun foreach (func seq) (and seq (funcall (car seq)) (foreach func (cdr seq))))");
standard += explisp("(defun reduce (func seq) (and seq (reducein func (car seq) (cdr seq))))");
standard += explisp("(defun reducein (func sum seq2) (if (null seq2) sum (reducein func (funcall func sum (car seq2)) (cdr seq2))))");
standard += explisp("(defun find-if (func seq) (and seq (if (funcall (car seq)) (car seq) (find-if func (cdr seq)))))");
standard += explisp("(defun position-if (func cons) (position-ifin func cons 0))");
standard += explisp("(defun position-ifin (func cons count) (and cons (if (funcall func (car cons)) count (position-ifin func (cdr cons) (+1 count)))))");
standard += explisp("(defun append2 (seq seqe) (if (null seq) seqe (cons (car seq) (append2 (cdr seq) seqe))))");
standard += explisp("(defun append (&rest seqs) (reduce 'append2 seqs))");
standard += explisp("(defun reverse (seq) (and seq (cons (car seq) (reverse (cdr seq)))))");

// define basic macros

evallisp("(defmacro let (binds &rest rest) `(block ,@(letin binds rest)))");
evallisp("(defmacro flet (binds &rest rest) `(block ,@(fletin binds rest)))");
evallisp("(defmacro mlet (binds &rest rest) `(block ,@(mletin binds rest)))");
evallisp("(defun letin (binds rest) (if (null binds) (if rest rest '(nil))" +
         "(cons `(setf (symbol-value (declare-variable (quote ,(caar binds)))) ,(car (cdar binds))) (letin (cdr binds) rest)))))");
evallisp("(defun fletin (binds rest) (if (null binds) (if rest rest '(nil))" +
         "(cons `(setf (symbol-function (declare-variable (quote ,(caar binds)))) (lambda ,@(cdar binds))) (letin (cdr binds) rest)))))");
evallisp("(defun mletin (binds rest) (if (null binds) (if rest rest '(nil))" +
         "(cons `(setf (symbol-function (declare-variable (quote ,(caar binds)))) (macro ,@(cdar binds))) (letin (cdr binds) rest)))))");
evallisp("(defmacro prog1 (bind &rest rest) `(let ((temp ,bind)) ,@rest temp))");

// ** test
