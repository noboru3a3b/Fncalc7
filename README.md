# Fncalc7
Fncalc7 is a slightly extended version of the functional programming language fncalc, designed by M.  Hiroi and implemented in Gauche.
I undertook this project to study the EBNF syntax used in compiler development.
fncalc originally came with a list library that applied closures,
but I rewrote it to use vectors.
Then I created a red-black tree library using vectors.
I received assistance from AI in creating this library, which was extremely helpful.
  
I have recently managed to resolve the conflict I had been considering between "continuations" and "tail-call optimization."
My thanks go to Claude Opus 4.8.

Subsequently, I improved the red-black tree library to a level where it could serve as the foundation for a database.
Along with that, I also made some minor enhancements to Fncalc.
My thanks go to Claude Code.　　
　　
## Usage:
PS C:\Users\user\Gauche\Fncalc7> gosh -r7 -A . fncalc9 scm  
  
```python
load("list2.cal");  
=> 1  
; ジェネレータ生成  
Calc> def make_gen(proc, ls)  
  let resume = 0 in  
    resume = fn(ret)  
      proc(fn(x) ret = callcc(fn(cont) resume = cont; ret(x); end); end, ls);  
      ret(nil);  
    end;  
    fn() callcc(fn(cont) resume(cont); end); end;  
  end  
end  
=> closure
; 順列生成  
Calc> def perm(f, ls)  
  let iter = 0 in  
    iter = fn(ls, a)  
      if null(ls) then  
        f(a);  
      else  
        foreach(fn(x) iter(remove(x, ls), cons(x, a)); end, ls);  
      end  
    end;  
    iter(ls, nil);  
  end  
end  
=> closure
; 順列の生成テスト（１回目）  
Calc> g = make_gen(perm, iota(1, 3));  
=> closure  
Calc> printlist(g());  
(3 2 1)  
=> 0  
Calc> printlist(g());  
(2 3 1)  
=> 0  
Calc> printlist(g());  
(3 1 2)  
=> 0  
Calc> printlist(g());  
(1 3 2)  
=> 0  
Calc> printlist(g());  
(2 1 3)  
=> 0  
Calc> printlist(g());  
(1 2 3)  
=> 0  
Calc> printlist(g());  
()  
=> 0
; 順列の生成テスト（２回目）  
Calc> g = make_gen(perm, iota(1, 3));			<--- make_gen() は何度でも動く  
=> closure  
Calc> printlist(g());  
(3 2 1)  
=> 0
```  
  
```python
Calc> load("sample2.cal");  
=> 1  
Calc> quick_sort([5, 6, 4, 7, 3, 8, 2, 9, 1, 0]);  
=> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```
  
```python
Calc> load("list2.cal");  
=> 1  
Calc> a = iota(1, 10);  
=> [1, [2, [3, [4, [5, [6, [7, [8, [9, [10, nil]]]]]]]]]]  
Calc> printlist(a);  
(1 2 3 4 5 6 7 8 9 10)  
=> 0
```  
  
```python
Calc> load("rbtree4.cal");
=> 1
Calc> load("rbtree4_test.cal");

--- 0. validator self-test (each case MUST be detected)
  (the following violations are expected output)
  RB-VIOLATION: root is red at key 7
  RB-VIOLATION: right-leaning red link at key 20
  RB-VIOLATION: BST order violated at key 2
  RB-VIOLATION: size field inconsistent at key 7
  RB-VIOLATION: red node with red left child at key 3
  RB-VIOLATION: black height mismatch at key 3
  RB-VIOLATION: black height mismatch at key 7

--- 1. basic operations

--- 2. deterministic insert / delete patterns

--- 3. delete_min / delete_max  (RB-02 regression)

--- 4. randomized ops with full model check

--- 5. ordered queries

--- 6. traversal

--- 7. rb_from_sorted, exhaustive n = 0..200
  black height: bulk=7  sequential=9

--- 8. string keys

--- 9. persistent updates and snapshots

--- 10. regressions from the rbtree3.cal audit

--- 11. scale
  20000 puts -> size 20000, black height 13

=========================================
  assertions : 108
  failures   : 0
  RESULT     : ALL TESTS PASSED
=========================================
=> 1
Calc>
