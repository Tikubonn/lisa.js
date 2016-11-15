
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
        index = self.push(message + this);
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
    return this.get().evaluatearg();
};

ReferenceClass.prototype.onevaluate = null;
ReferenceClass.prototype.onexpand = null;
ReferenceClass.prototype.onexpandarg = null;

function getreference (reference){
    return reference instanceof ReferenceClass ?
        reference.get() : 
        reference;
};

function setreference (reference, value){
    if (reference instanceof ReferenceClass == false)
        throw new Error("reference is not reference instance.");
    return reference.set(value);
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

// nil reference class
//     <- reference class

function NilReferenceClass (){}

NilReferenceClass.prototype = 
    Object.create(ReferenceClass.prototype);

NilReferenceClass.prototype.get = function (){return nil;};
NilReferenceClass.prototype.set = function (){throw new Error("nil reference could not assign.");};

var nilf = new NilReferenceClass();

// symbol reference class
//      <- reference class

function SymbolReferenceClass (value){

    // ** check the reference value.

    if (value instanceof SymbolFamilyClass == false)
        throw new Error("in the symbol reference, " + value + " is not symbol instance.");

    // ** set the members.

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
    // return "(" + this.value.toString() + "|0)";
    return this.value.toString();
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

ConsClass.prototype.getcar = function (){return this.car;};
ConsClass.prototype.getcdr = function (){return this.cdr;};
ConsClass.prototype.setcar = function (value){this.car = value; return value;};
ConsClass.prototype.setcdr = function (value){this.cdr = value; return value;};

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
    for (ncons = nil, cons = this; cons != nil; cons = cons.cdr){
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
        else ncons = new ConsClass(cons.car, ncons);}
    return ncons.reverse();
};

ConsClass.prototype.toArray = function (){
    this.shouldlinear();
    var sequence, cons;
    for (sequence = [], cons = this; cons != nil; cons = cons.cdr)
        sequence.push(cons.car);
    return sequence;
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

NilClass.prototype.getcar = function (){return new NilReferenceClass();};
NilClass.prototype.getcdr = function (){return new NilReferenceClass();};
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

UserFunctionClass.prototype.label = "<#user function class>";

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
                makecons(macdeflvar,
                         makecons(consa.car,
                                  makecons(
                                      makecons(basconlist, consb)))),
                bound);
    };

    // reverse binding arguments.

    bound = bound.reverse();
    
    // build formula.
 
    formula = makecons(synblock,
                    makecons(bound, this.rest));

    var message;    
    message = ("user function <- " + formula + "");
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
    aftergetreference(
        afterevaluatearg(
            CallableClass.prototype.evaluate));

MacroClass.prototype.expand = 
    afterexpandarg(MacroClass.prototype.evaluate);

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
    var expand = UserFunctionClass.prototype.onevaluate.apply(this, arguments);
    stracedb.push("user macro <- " + expand + "");
    return expand;
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
    return this.obarray[name.toPlain()] = sym;
};

Obarray.prototype.intern = function (name){
    if (this.find(name) == null)
        this.set(name, new VariableSymbolClass(name));
    return new InternSymbolClass(name);
};

// Obarray.prototype.list = function (){ // ** for degug
//     var sequence, names, index;
//     for (sequence = [], names = Object.keys(this.obarray), 
//          index = 0; index < names.length; index++)
//         sequence.push(this.obarray[names[index]]);
//     return sequence;
// };

// obarrays class
//     <- native, function class

function Obarrays (parent){
    this.obarray = new Obarray();
    this.parent = parent || null;
};

// Obarrays.prototype.length = function (){ // ** for debug
//     var count, current;
//     for (count = 0, current = this; current; current = current.parent, count++);
//     return count;
// };

// Obarrays.prototype.list = function (){ // ** for debug
//     var sequence, current;
//     for (sequence = [], current = this; current; current = current.parent)
//         sequence = sequence.concat(current.obarray.list());
//     return sequence;
// };

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

// Obscope.prototype.length = function (){ // ** for debug
//     var count, current;
//     for (count = 0, current = this; current; current = current.parent)
//         count += current.obarray.length();
//     return count;
// };

// Obscope.prototype.list = function (){ // ** for debug
//     var sequence, current;
//     for (sequence = [], current = this; current; current = current.parent)
//         sequence = sequence.concat(current.listin());
//     return sequence;
// };

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

inp.scope.intern(makestring("if")).setfunc(synif);
inp.scope.intern(makestring("block")).setfunc(synblock);
inp.scope.intern(makestring("progn")).setfunc(synprogn);
inp.scope.intern(makestring("setf")).setfunc(synsetf);
inp.scope.intern(makestring("quote")).setfunc(synquote);
inp.scope.intern(makestring("quoteb")).setfunc(synquoteback);

synif.label = "<#syntax if>";
synblock.label = "<#syntax block>";
synprogn.label = "<#syntax progn>";
synsetf.label = "<#syntax setf>";
synquote.label = "<#syntax quote>";
synquoteback.label = "<#syntax quoteback>";
synlambda.label = "<#syntax lambda>";
synmacro.label = "<#syntax macro>";

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

synprogn.onevaluate = function (){
    var res, index;
    for (res = nil, index = 0; index < arguments.length; index++)
        res = arguments[index].evaluatearg();
    return res;
};

synsetf.onevaluate = function (formula, value){
    strace.push("setf <" + formula + "> is <" + value + ">");
    var valued = getreference(value.evaluatearg());
    var formulaed = formula.evaluatearg();
    return formulaed.set(valued);
};

synquote.onevaluate = function (some){
    return some;
};

synquoteback.onevaluate = function (some){
    return some.clone();
};

synlambda.onevaluate = function (args){
    return new UserFunctionClass(args, ConsClass.toCons(slice(arguments, 1)));
};

synmacro.onevaluate = function (args){
    return new UserMacroClass(args, ConsClass.toCons(slice(arguments, 1)));
};

// define basic macro functions

var macprog1 = new PrimitiveMacroClass();
var macdefun = new PrimitiveMacroClass();
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
inp.scope.intern(makestring("defun")).setfunc(macdefun);
inp.scope.intern(makestring("defmacro")).setfunc(macdefmacro);
inp.scope.intern(makestring("setq")).setfunc(macsetq);
inp.scope.intern(makestring("incf")).setfunc(macincf);
inp.scope.intern(makestring("decf")).setfunc(macdecf);
inp.scope.intern(makestring("let")).setfunc(maclet);
inp.scope.intern(makestring("flet")).setfunc(macflet);
inp.scope.intern(makestring("mlet")).setfunc(macmlet);
inp.scope.intern(makestring("defvar")).setfunc(macdefvar);
inp.scope.intern(makestring("deflvar")).setfunc(macdeflvar);

macprog1.label = "<#primtive macro prog1>";
macdefun.label = "<#primtive macro defun>";
macdefmacro.label = "<#primtive macro defmacro>";
macsetq.label = "<#primtive macro setq>";
macincf.label = "<#primtive macro incf>";
macdecf.label = "<#primtive macro decf>";
maclet.label = "<#primtive macro let>";
macflet.label = "<#primtive macro flet>";
macmlet.label = "<#primtive macro mlet>";
macdefvar.label = "<#primtive macro defvar>";
macdeflvar.label = "<#primtive macro deflvar>";

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
    return makelist(
        synsetf,
        formula,
        makelist(
            basadd,
            formula,
            makeint(1)));
};

macdecf.onevaluate = function (formula){
    return makelist(
        synsetf,
        formula,
        makelist(
            bassub,
            formula,
            makeint(1)));
};

macdefvar.onevaluate = function (sym, value){
    return makelist(
        synsetf,
        makelist(
            bassymbolvalue,
            makelist(
                basglobal,
                makelist(
                    synquote,
                    sym))),
        value);
}

macdeflvar.onevaluate = function (sym, value){
    return makelist(
        synsetf,
        makelist(
            bassymbolvalue,
            makelist(
                baslocal,
                makelist(
                    synquote, 
                    sym))),
        value);
};

macdefun.onevaluate = function (name){
    return new ConsClass(synsetf,
                         new ConsClass(
                             new QuoteClass(
                                 new SymbolFunctionReferenceClass(name)),
                             new ConsClass(
                                 new ConsClass(synlambda,
                                               ConsClass.toCons(slice(arguments, 1))))));
};

macdefmacro.onevaluate = function (name){
    return new ConsClass(synsetf,
                     new ConsClass(
                         new QuoteClass(
                             new SymbolFunctionReferenceClass(name)),
                         new ConsClass(
                             new ConsClass(synmacro,
                                           ConsClass.toCons(slice(arguments, 1))))));
};

macsetq.onevaluate = function (sym, value){
    return new ConsClass(synsetf,
                         new ConsClass(
                             new QuoteClass(
                                 new SymbolValueReferenceClass(sym)),
                             new ConsClass(value)));
};

maclet.onevaluate = function (bounds){
    var bound, formula;
    for (bound = makecons(synprogn); bounds != nil; bounds = bounds.cdr)
        bound = makecons(makecons(macdeflvar, bounds.car), bound);
    return makecons(synblock,
                    makecons(bound.reverse(),
                             ConsClass.toCons(slice(arguments, 1))));
};

macflet.onevaluate = function (bounds){
    var bound, formula;
    for (bound = makecons(synprogn); bounds != nil; bounds = bounds.cdr)
        bound = makecons(
            makelist(
                synsetf, 
                makelist(
                    bassymbolfunction, 
                    makelist(synquote, bounds.car.car)),
                makecons(synlambda, bounds.car.cdr)), bound);
    return makecons(synblock,
                    makecons(bound.reverse(),
                             ConsClass.toCons(slice(arguments, 1))));
};

macmlet.onevaluate = function (bounds){
    var bound, formula;
    for (bound = makecons(synprogn); bounds != nil; bounds = bounds.cdr)
        bound = makecons(
            makelist(
                synsetf, 
                makelist(
                    bassymbolfunction, 
                    makelist(synquote, bounds.car.car)),
                makecons(synmacro, bounds.car.cdr)), bound);
    return makecons(synblock,
                    makecons(bound.reverse(),
                             ConsClass.toCons(slice(arguments, 1))));
};

// define basic scope methods

var baslocal = new PrimitiveFunctionClass();
var basglobal = new PrimitiveFunctionClass();

baslocal.label = "<#primitive local>";
basglobal.label = "<#primitive global>";

baslocal.onevaluate = function (sym){
    return inp.scope.internf(sym.name);
};

basglobal.onevaluate = function (sym){
    return inp.scoperoot.internf(sym.name);
};

// define basic symbol methods

var bassymbolfunction = new PrimitiveFunctionClass();
var bassymbolvalue = new PrimitiveFunctionClass();
var bassymbolname = new PrimitiveFunctionClass();
var bassymbolintern = new PrimitiveFunctionClass();
var bassymbolmake = new PrimitiveFunctionClass();

bassymbolfunction.label = "<#primitive symbol-function>";
bassymbolvalue.label = "<#primitive symbol-value>";
bassymbolname.label = "<#primitive symbol-name>";
bassymbolintern.label = "<#primitive symbol-intern>";
bassymbolmake.label = "<#primtive symbol-make>";

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

basfnfuncall.label = "<#primitive funcall>";
basfnapply.label = "<#primitive apply>";

basfnfuncall.onevaluate = function (func){
    return func.evaluate.apply(func, slice(arguments, 1));
};

basfnapply.onevaluate = function (func, args){
    return func.evaluate.apply(func, args.toArray());
};

// define basic debug methods

var basdebprint = new PrimitiveFunctionClass();
var basdebstrace = new PrimitiveFunctionClass();
var basdebstracedb = new PrimitiveFunctionClass();

basdebprint.label = "<#debug print>";
basdebstrace.label = "<#debug strace>";

basdebprint.onevaluate = function (some){
    console.log(some + "");
    return some;
};

basdebstrace.onevaluate = function (){
    strace.print();
    return nil;
};

basdebstracedb.onevaluate = function (){
    stracedb.print();
    return nil;
};

// define basic cons methods

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

basconcons.label = "<#primitive cons cons>";
basconcar.label = "<#primitive cons car>";
basconcdr.label = "<#primitive cons cdr>";
basconcaar.label = "<#primitive cons caar>";
basconcadr.label = "<#primitive cons cadr>";
basconcdar.label = "<#primitive cons cdar>";
basconcddr.label = "<#primitive cons cddr>";
basconlist.label = "<#primitive cons list>";

inp.scope.intern(makestring("cons")).setfunc(basconcons);
inp.scope.intern(makestring("car")).setfunc(basconcar);
inp.scope.intern(makestring("cdr")).setfunc(basconcdr);
inp.scope.intern(makestring("caar")).setfunc(basconcaar);
inp.scope.intern(makestring("cadr")).setfunc(basconcadr);
inp.scope.intern(makestring("cdar")).setfunc(basconcdar);
inp.scope.intern(makestring("cddr")).setfunc(basconcddr);
inp.scope.intern(makestring("list")).setfunc(basconlist);

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
            basconcdr,
            makelist(
                basconcar,
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
            basconcar,
            makelist(
                basconcdr,
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
// with user macro class

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
var maccase_rest = makeintern("&rest");
var maccase_args = makeintern("args");

basnull.label = "<#primitive basic null>";
basnot.label = "<#primitive basic not>";
macand.label = "<#primitive macro and>";
macor.label = "<#primitive macro or>";
macwhen.label = "<#primitive macro when>";
macunless.label = "<#primitive macro unless>";
maccond.label = "<#primitive macro cond>";
maccase.label = "<#primitive macro case>";

inp.scope.intern(makestring("null")).setfunc(basnull);
inp.scope.intern(makestring("not")).setfunc(basnot);
inp.scope.intern(makestring("and")).setfunc(macand);
inp.scope.intern(makestring("or")).setfunc(macor);
inp.scope.intern(makestring("when")).setfunc(macwhen);
inp.scope.intern(makestring("unless")).setfunc(macunless);
inp.scope.intern(makestring("cond")).setfunc(maccond);
inp.scope.intern(makestring("case")).setfunc(maccase);

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
                            macor_args)))
            )));

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
    (defmacro cond (&rest conds)
        (if (null conds) nil
            (list if 
                (caar conds)
                (cdar conds)
                (cons cond (cdr conds)))))
-- */

// define basic cons methods
// with user function class

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

var basconlength = new UserFunctionClass();
var basconlength_sequence = inp.scope.intern(makestring("sequence"));

var basconnth = new UserFunctionClass();
var basconnth_index = inp.scope.intern(makestring("index"));
var basconnth_sequence = inp.scope.intern(makestring("sequence"));

var basconappend2 = new UserFunctionClass();
var basconappend2_sequence = inp.scope.intern(makestring("sequence"));
var basconappend2_sequencec = inp.scope.intern(makestring("sequencec"));

var basconappend = new UserFunctionClass();
var basconappend_rest = inp.scope.intern(makestring("&rest"));
var basconappend_args = inp.scope.intern(makestring("args"));

var basconfindif = new UserFunctionClass();
var basconfindif_func = inp.scope.intern(makestring("func"));
var basconfindif_sequence = inp.scope.intern(makestring("sequence"));

var basconpositionif = new UserFunctionClass();
var basconpositionif_func = inp.scope.intern(makestring("func"));
var basconpositionif_sequence = inp.scope.intern(makestring("sequence"));

var basconpositionifin = new UserFunctionClass();
var basconpositionifin_func = inp.scope.intern(makestring("func"));
var basconpositionifin_sequence = inp.scope.intern(makestring("sequence"));
var basconpositionifin_count = inp.scope.intern(makestring("count"));

var basconcopy = new UserFunctionClass();
var basconcopy_sequence = inp.scope.intern(makestring("sequence"));

var basconnreverse = new UserFunctionClass();
var basconnreverse_sequence = inp.scope.intern(makestring("sequence"));

var basconnreversein = new UserFunctionClass();
var basconnreversein_sequence = inp.scope.intern(makestring("sequence"));
var basconnreversein_before = inp.scope.intern(makestring("before"));
var basconnreversein_after = inp.scope.intern(makestring("after"));

basconmap.label = "<#primitive cons map>";
basconfilter.label = "<#primitive cons filter>";
basconreduce.label = "<#primitive cons reduce>";
basconreducein.label = "<#primitive cons reducein>";
basconlength.label = "<#primitive cons length>";
basconnth.label = "<#primitive cons nth>";
basconappend2.label = "<#primitive cons append2>";
basconappend.label = "<#primitive cons append>";
basconfindif.label = "<#primitive cons findif>";
basconpositionif.label = "<#primitive cons positionif>";
basconcopy.label = "<#primitive cons copy>";
basconnreverse.label = "<#primitive cons nreverse>";
basconnreversein.label = "<#primitive cons nreversein>";

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

source = makelist(
    basconappend,
    makelist(
        basconlist,
        makeint(1)),
    makelist(
        basconlist,
        makeint(2)),
    makelist(
        basconlist,
        makeint(3)),
    makelist(
        basconlist,
        makeint(4)));

strace.unwindstrace(function (){
    console.log("" + source + "");
    console.log("" + source.evaluatearg() + ""); // ** error
})();

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
