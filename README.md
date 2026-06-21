# Fncalc7
Fncalc7 is a slightly extended version of the functional programming language fncalc, designed by M.  Hiroi and implemented in Gauche.
I undertook this project to study the EBNF syntax used in compiler development.
fncalc originally came with a list library that applied closures,
but I rewrote it to use vectors.
Then I created a red-black tree library using vectors.
I received assistance from AI in creating this library, which was extremely helpful.

## Usage:
PS C:\Users\user\Gauche\Fncalc7> gosh -r7 -A    \fncalc7 scm  
  
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
Calc> load("rbtree3.cal");  
=== Red-Black Tree Test ===  
  
Inserting: 10, 5, 20, 15, 30, 25, 35, 3, 7  
  
In-order traversal:  
3  
5  
7  
10  
15  
20  
25  
30  
35  
  
Tree is valid (black height: 4)  
  
Searching for key 15:  
ddd  
  
Deleting key 10:  
  
In-order traversal after deletion:  
3  
5  
7  
15  
20  
25  
30  
35  
  
Tree is valid (black height: 4)  
  
Deleting key 20:  
  
In-order traversal after deletion:  
3  
5  
7  
15  
25  
30  
35  
  
Tree is valid (black height: 3)  
  
Deleting key 5:  
In-order traversal:  
3  
7  
15  
25  
30  
35  
Tree is valid (black height: 3)  
  
=== Stress Test: Insert and Delete ===  
After 50 insertions, node count: 50  
Tree is valid (black height: 6)  
  
Deleting every other node:  
After deletions, node count: 25  
Tree is valid (black height: 5)  
  
Remaining keys (should be odd numbers):  
1  
3  
5  
7  
9  
11  
13  
15  
17  
19  
21  
23  
25  
27  
29  
31  
33  
35  
37  
39  
41  
43  
45  
47  
49  
                B 1  
            B 3  
                B 5  
        B 7  
                B 9  
            B 11  
                B 13  
    R 15  
                B 17  
            B 19  
                B 21  
        B 23  
                B 25  
            B 27  
                B 29  
B 31  
            B 33  
        B 35  
            B 37  
    B 39  
                B 41  
            R 43  
                B 45  
        B 47  
            B 49  
Tree is valid (black height: 5)  
=== Test Complete ===
```
=> 1
Calc>
