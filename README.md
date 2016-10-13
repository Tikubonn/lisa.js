# lisa.js
lisa is a translator. this language will be convert to javascript source code. 

```lisp
(progn
  (defun hello (name)
    (print (concat "hello, " name)))

(hello "lisa")) ;; hello, lisa
```

```js
(a=function(b){return ((c=("hello ,"+b)),(console.log(c)));},a("lisa"))
```