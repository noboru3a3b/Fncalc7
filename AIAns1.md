fncalc6.scmへの継続機能の復活は可能です。
以前の解決策と同じアプローチで修正できます。

## 修正内容

### 1. `factor`関数の`callcc`部分を復活

```scheme
((callcc)
 ;; 継続 callcc(f)
 ;; ldct next args 1 引数 f の評価 app next ...
 (get-token)
 (unless (eq? *token* 'lpar)
   (compile-error "callcc: '(' expected"))
 (get-token)
 (let ((code (append (list 'args 1) (expression env) (list 'app))))
   (unless (eq? *token* 'rpar)
     (compile-error "callcc: invalid token"))
   (get-token)
   (append (list 'ldct (length code)) code)))
```

この部分を、現在の以下のコード：
```scheme
((callcc)
 ;; A1 方針: 末尾再帰最適化を優先し、callcc は無効化する
 (compile-error "callcc is disabled"))
```
と置き換えてください。

### 2. `compile`関数を修正

```scheme
(define (compile)
  (cond ((eq? *token* 'def)
         ;; 関数定義
         (get-token)
         (unless (eq? *token* 'ident)
           (compile-error "invalid def form"))
         (let ((name *value*)
               (code (append (compile-block (list (get-parameter)))
                     (list 'rtn))))
           (optimize code)  ;; 関数定義内のコードは最適化OK
           (list 'ldf code 'gset (get-gvar name))))
        (else
         (let ((code (compile-statement '())))
           ;; 継続を含む可能性があるトップレベルコードは最適化しない
           code))))
```

変更点：関数定義内で`optimize`を呼び出し、`else`節ではトップレベルコードを最適化せずに返します。

### 3. `calc`関数での最適化をコメントアウト

```scheme
(let ((code (append (compile) (list 'halt))))
  ;; (optimize code)  ;; トップレベルでは最適化しない（継続との共存のため）
  (pass3 code)
  (let* ((s (current-jiffy))
         (val (vm '() '() code '())))
    ...
```

### 4. `vm`関数の`ldct`と`continuation`処理を確認

既存のコードは正しいですが、念のため確認してください：

```scheme
((ldct)
 ;; 継続
 (vm (cons (list 'continuation s e (drop (cddr c) (cadr c)) d) s)
     e
     (cddr c)
     d))
```

```scheme
(cd Ga
 (vm (cons (car lvar) (car clo)) (cadr clo) (caddr clo) (cadddr clo)))
```

これらは`app`と`tapp`の両方に含まれている必要があります。

## 理由

- **関数定義内の最適化**: 関数定義では、コンパイル後すぐに最適化を実行し、その結果を`ldf`に格納します。
これにより、`ldct`が見るコードは既に最適化済みで、スキップ長の計算が正確になります。

- **トップレベルでの最適化無効化**: トップレベルで`callcc`が使われる可能性があるため、最適化を無効にします。
これにより、`ldct`が計算したスキップ長と実行時のコード構造が一致します。

## 動作確認

修正後、以下のテストコードで動作を確認できます：

```
load("list.cal");
def make_gen(proc, ls)
  let resume = 0 in
    resume = fn(ret)
      proc(fn(x) ret = callcc(fn(cont) resume = cont; ret(x); end); end, ls);
      ret(nil);
    end;
    fn() callcc(fn(cont) resume(cont); end); end;
  end
end
def perm(f, ls)
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
g = make_gen(perm, iota(1, 3));
printlist(g());
printlist(g());
g = make_gen(perm, iota(1, 3));  # 2回目も動作するはず
printlist(g());
```

この修正により、`return`/`break`機能と継続機能が共存し、末尾再帰最適化も関数定義内では有効なままになります。

