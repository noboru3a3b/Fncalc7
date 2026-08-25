;;;
;;; fncalc8.scm : 関数型電卓プログラム (R7RS-small 対応版)
;;;
;;;               Copyright (C) 2011-2021 Makoto Hiroi
;;;               (1) コンパイラを作成
;;;               (2) SECD 仮想マシンを作成
;;;               (3) 継続機能を追加
;;;               (4) 末尾再帰最適化を追加
;;;               (5) ベクタを追加
;;;               (6) サンプルプログラムを作成
;;;               (7) if文を文の連続に対応
;;;               (8) 末尾再帰最適化を無効化（継続の機能を一部壊していたため）
;;;               (9) 末尾再帰最適化を有効化。継続を削除。break、return を追加
;;;              (10) 継続を復活。末尾再帰最適化、break、return は維持
;;;              (11) 末尾再帰最適化を micro_Scheme8 方式へ変更。後付けの
;;;                   大域 optimize パスを廃止し、関数本体(def/fn/let)の構築時に
;;;                   末尾位置を確定して最適化（トップレベルを含め一様に適用）
;;;              (12) fncalc9: 以下の 3 点を修正 (FC-01 / FC-02 / FC-03)
;;;                   FC-01: return が let を貫通するようにした。
;;;                          let はクロージャ＋即時適用にコンパイルされるため、
;;;                          従来は let 本体の return が「関数」ではなく「let」から
;;;                          抜けていた。let 専用の適用命令 lapp を新設し、ダンプへ
;;;                          'call ではなく 'let タグのフレームを積む。rtn は両方を
;;;                          受理し、ret (return) は 'call だけを探して巻き戻す。
;;;                   FC-02: 文字列プリミティブを追加（従来は文字列の長さも内容比較も
;;;                          取得できなかった）。length をベクタ／文字列の両対応に。
;;;                   FC-03: load-file のエラー処理を with-exception-handler から
;;;                          guard + 再送出へ変更。非継続例外のハンドラから戻ることに
;;;                          よる二次例外を解消し、失敗位置（ファイル名・行・桁）を表示。
;;;
(import (scheme base) (scheme cxr) (scheme char) (scheme inexact)
        (scheme file) (scheme read) (scheme write) (scheme time)
        (gauche base))

;;;
;;; マクロ定義
;;;

;;; 多値は考慮しない簡略版
(define-syntax begin0
  (syntax-rules ()
    ((_ a) a)
    ((_ a b ...) (let ((x a)) (begin b ...) x))))

;;; データの追加
(define-syntax push!
  (syntax-rules ()
    ((_ place x) (set! place (cons x place)))))

;;; データの取得
(define-syntax pop!
  (syntax-rules ()
    ((_ place)
     (let ((x (car place)))
       (set! place (cdr place))
       x))))

;;;
;;; コンパイル時の状態（break 用）
;;;
(define *loop-depth* 0)  ; while のネスト深さ（コンパイル時のみ使用）

;;; コード追跡用関数
;; DEBUGスイッチ ON: #t, OFF: #f
(define *debug* #f)

(define (pass str)
  (if *debug* (format #t "~a  ~a ~a ~%" str *token* *value*)))

(define (pass2 str val)
  (if *debug* (format #t "  ~a  ~a ~a (~a) ~%" str *token* *value* val)))

(define (pass3 code)
  (if *debug* (print code)))

;;;
;;; リスト操作関数
;;;

;;; 末尾のセルを求める
(define (last-pair xs)
  (if (null? (cdr xs))
      xs
      (last-pair (cdr xs))))

;;; 末尾の要素を求める
(define (last xs)
  (car (last-pair xs)))

;;; 先頭から n 個の要素を取り除く
(define (drop xs n)
  (if (or (zero? n) (null? xs))
      xs
      (drop (cdr xs) (- n 1))))

;;; 末尾の要素を取り除く
(define (butlast xs)
  (if (null? (cdr xs))
      '()
      (cons (car xs) (butlast (cdr xs)))))

;;;
;;; 大域変数
;;;
(define *ch*    #f)
(define *token* #f)
(define *value* #f)
(define *input* (current-input-port))
(define *line*  #f)
(define *col*   #f)

;;;
;;; グローバルな環境
;;;
(define *global-environment*
  `((exp      primitive ,exp)
    (log      primitive ,log)
    (sin      primitive ,sin)
    (cos      primitive ,cos)
    (tan      primitive ,tan)
    (asin     primitive ,asin)
    (acos     primitive ,acos)
    (atan     primitive ,atan)
    (sqrt     primitive ,sqrt)
    (expt     primitive ,expt)
    (abs      primitive ,abs)
    (floor    primitive ,floor)
    (ceiling  primitive ,ceiling)
    (round    primitive ,round)
    (truncate primitive ,truncate)
    (max      primitive ,max)
    (min      primitive ,min)
    (gcd      primitive ,gcd)
    (lcm      primitive ,lcm)
    (quotient primitive ,quotient)
    (remainder   primitive ,remainder)
    (modulo   primitive ,modulo)
    (sinh     primitive ,sinh)
    (cosh     primitive ,cosh)
    (tanh     primitive ,tanh)
    (number   primitive ,(lambda (x) (if (number? x) 1 0)))
    (string   primitive ,(lambda (x) (if (string? x) 1 0)))
    (function primitive ,(lambda (x) (if (pair? x) 1 0)))
    (vector   primitive ,(lambda (x) (if (vector? x) 1 0)))
    (make_vector primitive ,(lambda (x y) (make-vector x y)))
    ;; length はベクタと文字列の両方に対応 (FC-02)
    (length   primitive ,(lambda (x)
                           (cond ((vector? x) (vector-length x))
                                 ((string? x) (string-length x))
                                 (else (error "length: vector or string required" x)))))
    ;;; 文字列プリミティブ (FC-02)
    ;; 文字列キー・可変長レコード・シリアライズのための最小セット。
    ;; 従来は string(x) による型判定と display しか無く、内容比較すら出来なかった。
    ;; 注意: eq は eqv? による同一性比較なので、内容の一致には string_equal を使う。
    (string_compare   primitive ,(lambda (a b)
                                   (cond ((string<? a b) -1)
                                         ((string>? a b)  1)
                                         (else            0))))
    (string_equal     primitive ,(lambda (a b) (if (string=? a b) 1 0)))
    (string_length    primitive ,string-length)
    (string_ref       primitive ,(lambda (s i) (char->integer (string-ref s i))))
    (string_from_code primitive ,(lambda (n) (string (integer->char n))))
    (substring        primitive ,substring)
    (string_append    primitive ,string-append)
    (number_to_string primitive ,number->string)
    (string_to_number primitive ,(lambda (s)
                                   (let ((n (string->number s)))
                                     (if n
                                         n
                                         (error "string_to_number: not a number" s)))))
    (load     primitive ,(lambda (x) (load-file x) 1))
    (display  primitive ,(lambda (x) (print-data x) x))
    (newline  primitive ,(lambda () (newline) 0))
    (print    primitive ,(lambda (x) (print-data x) (newline) x))))

;;; ベクタの表示
(define (print-vector v)
  (display "[")
  (do ((i 0 (+ i 1)))
      ((= i (vector-length v)) (display "]"))
    (if (vector? (vector-ref v i))
        (print-vector (vector-ref v i))
      (display (vector-ref v i)))
    (when (< i (- (vector-length v) 1))
      (display ", "))))

;;; データの表示
(define (print-data data)
  (cond ((pair? data) (display (car data)))
        ((vector? data) (print-vector data))
        (else (display data))))

;;; 大域変数を求める
(define (get-gvar sym)
  (let ((val (assoc sym *global-environment*)))
    (unless val
      (set! val (cons sym 0))
      (push! *global-environment* val))
    val))

;;;
;;; 入力処理
;;;

;;; 文字の読み込み
(define (nextch)
  (set! *ch* (read-char *input*))
  (cond ((eof-object? *ch*)
         (set! *ch* #\null))
        ((eqv? *ch* #\newline)
         (set! *line* (+ *line* 1))
         (set! *col* 0))
        (else
         (set! *col* (+ *col* 1)))))

;;; コンパイルエラー
(define (compile-error mes)
  (error mes *token* *line* *col*))

;;; 先読み記号の取得
(define (getch) *ch*)

;;; 数値
(define (get-number)
  (let ((buff '()))
    ;; 整数を buff に格納
    (define (get-numeric)
      (do ()
          ((not (char-numeric? (getch))))
        (push! buff (getch))
        (nextch)))
    ;; 整数部
    (get-numeric)
    (case (getch)
      ((#\.)
       ;; 小数部
       (push! buff (getch))
       (nextch)
       (get-numeric)
       (case (getch)
         ((#\d #\D #\e #\E)
          ;; 指数部
          (push! buff (getch))
          (nextch)
          (when (or (eqv? (getch) #\+)
                    (eqv? (getch) #\-))
            (push! buff (getch))
            (nextch))
          ;; 指数の数字
          (get-numeric))))
      ((#\/)
       ;; 分数
       (push! buff (getch))
       (nextch)
       (get-numeric)))
    (string->number (list->string (reverse buff)))))

;;; 識別子
(define (get-ident)
  (let loop ((a '()))
    (if (and (not (char-alphabetic? (getch)))
             (not (char-numeric? (getch)))
             (not (eqv? (getch) #\_)))
        (string->symbol (list->string (reverse a)))
      (loop (begin0 (cons (getch) a) (nextch))))))

;;; 文字列
(define (escape-code c)
  (case c
    ((#\t) #\tab)
    ((#\n) #\newline)
    (else c)))

(define (get-string)
  (nextch)
  (let loop ((buff '()))
    (cond ((eqv? (getch) #\null)
           (compile-error "unterminated string literal"))
          ((eqv? (getch) #\")
           (nextch)
           (list->string (reverse buff)))
          ((eqv? (getch) #\\)
           ;; エスケープ記号
           (nextch)
           (when (eqv? (getch) #\null)
             (compile-error "unterminated string escape"))
           (loop (begin0 (cons (escape-code (getch)) buff) (nextch))))
          (else
           (loop (begin0 (cons (getch) buff) (nextch)))))))

;;; トークンの切り出し
(define (get-token)
  ;; 空白文字の読み飛ばし
  (do ()
      ((not (char-whitespace? (getch))))
    (nextch))
  (cond ((char-numeric? (getch))
         (set! *token* 'number)
         (set! *value* (get-number)))
        ((char-alphabetic? (getch))
         (set! *value* (get-ident))
         (case *value*
           ((def end if then else and or not while do begin let in fn eq callcc break return)
            (set! *token* *value*))
           (else
            (set! *token* 'ident))))
        (else
         (case (getch)
          ((#\#)
           ;; コメントの読み飛ばし
           (do ()
               ((or (eqv? (getch) #\newline)
                    (eqv? (getch) #\null)))
             (nextch))
           (when (eqv? (getch) #\newline)
             (nextch))
           (get-token))
          ((#\")
           ;; 文字列
           (set! *token* 'string)
           (set! *value* (get-string)))
          ((#\=)
           (set! *token* '=)
           (nextch)
           (when (eqv? (getch) #\=)
             (set! *token* '==)
             (nextch)))
          ((#\+)
           (set! *token* '+)
           (nextch))
          ((#\-)
           (set! *token* '-)
           (nextch))
          ((#\*)
           (set! *token* '*)
           (nextch))
          ((#\%)
           (set! *token* '%)
           (nextch))
          ((#\/)
           (set! *token* '/)
           (nextch)
           (when (eqv? (getch) #\/)
             (set! *token* '//)
             (nextch)))
          ((#\()
           (set! *token* 'lpar)
           (nextch))
          ((#\))
           (set! *token* 'rpar)
           (nextch))
          ((#\[)
           (set! *token* 'lbra)
           (nextch))
          ((#\])
           (set! *token* 'rbra)
           (nextch))
          ((#\<)
           (set! *token* '<)
           (nextch)
           (when (eqv? (getch) #\=)
             (set! *token* '<=)
             (nextch)))
          ((#\>)
           (set! *token* '>)
           (nextch)
           (when (eqv? (getch) #\=)
             (set! *token* '>=)
             (nextch)))
          ((#\!)
           (set! *token* 'not)
           (nextch)
           (when (eqv? (getch) #\=)
             (set! *token* '!=)
             (nextch)))
          ((#\,)
           (set! *token* 'comma)
           (nextch))
          ((#\;)
           (set! *token* 'semic)
           (nextch))
          ((#\null)
           (set! *token* 'eof))
          (else
           (set! *token* 'others))))))

; [EBNF]
;
; 文       = def文 | 実行文.
; def文    = "def", 識別子, "(", [仮引数リスト], ")", block文.
; 実行文   = begin文 | if文 | while文 | let文 | break文 | return文 | 式文.
; begin文  = "begin", block文.
; if文     = "if", 式, "then", {実行文}, ("else", {実行文}, "end" | "end").
; while文  = "while", 式, "do", block文.
; let文    = "let", 代入式１, {",", 代入式１}, "in", block文.
; block文  = 実行文, {実行文}, "end".
; 式文     = 式, ";".
; break文  = "break", ";".
; return文 = "return", 式, ";".
;
; 式       = 代入式 | 式１.
; 代入式   = 左辺値, "=", 式.
; 左辺値   = 変数 | 変数, "[", 式, "]", {"[", 式, "]"}.
; 代入式１ = 変数, "=", 式１.
; 式１     = 式２, { ("and" | "or"), 式２}.
; 式２     = 式３, ("eq" | "==" | "!=" | "<" | "<=" | ">" | ">="), 式３.
; 式３     = 項, {("+" | "-"), 項}.
; 項       = 因子, {("*" | "/" | "%"), 因子}.
; 因子     = 定数 | ("+" | "-" | "not"), 因子 | "(", 式, ")" | 変数 | fn式 |
;            変数, "(", [引数リスト], ")" | fn式, "(", [引数リスト], ")" |
;            配列生成式 | 変数, "[", 式, "]", {"[", 式, "]"}.
; fn式     = "fn", "(", [仮引数リスト], ")", block文.
; 変数     = 識別子.
; 定数     = 数値 | 文字列.
;
; 配列生成式   = "[", [要素リスト], "]".
; 要素リスト   = 式, {",", 式}.
;
; 仮引数リスト = 変数, {",", 変数}.
; 引数リスト   = 式, {",", 式}.
;
; [注意] 数値, 識別子, 文字列の定義は省略

;;;
;;; 式の評価
;;;
(define (expression env)
  (pass "Expr ")
  (let ((val (expr1 env)))
    (pass2 "Expr " val)
    (case *token*
      ((=)
       (get-token)
       (cond ((eq? (last val) 'vref)
              ;; ベクタの代入
              (append (butlast val) (expression env) (list 'vset)))
             (else
              (case (car val)
                ((ld)
                 ;; 局所変数の代入
                 (append (expression env) (list 'lset (cadr val))))
                ((ldg)
                 ;; 大域変数の代入
                 (append (expression env) (list 'gset (cadr val))))
                (else
                 (compile-error "invalid assignment form"))))))
      (else val))))

;; 論理演算子 (and と or の優先順位は同じとする)
(define (expr1 env)
  (pass "Expr1")
  (let loop ((val1 (expr2 env)))
    (pass2 "Expr1" val1)
    (case *token*
      ((and)
       (get-token)
       (loop (append val1
                     (list 'not
                           'sel
                           (list 'ldc 0 'join)
                           (append (expr2 env) (list 'join))))))
      ((or)
       (get-token)
       (loop (append val1
                     (list 'dup
                           'sel
                           (list 'join)
                           (append (list 'pop) (expr2 env) (list 'join))))))
      (else val1))))

;;; 比較演算子 (==, !=, <, <=, >, >= の優先順位は同じとする)
(define (expr2 env)
  (pass "Expr2")
  (let ((val1 (expr3 env)))
    (pass2 "Expr2" val1)
    (case *token*
      ((==)
       (get-token)
       (append val1 (expr3 env) (list '==)))
      ((!=)
       (get-token)
       (append val1 (expr3 env) (list '!=)))
      ((<)
       (get-token)
       (append val1 (expr3 env) (list '<)))
      ((<=)
       (get-token)
       (append val1 (expr3 env) (list '<=)))
      ((>)
       (get-token)
       (append val1 (expr3 env) (list '>)))
      ((>=)
       (get-token)
       (append val1 (expr3 env) (list '>=)))
      ((eq)
       (get-token)
       (append val1 (expr3 env) (list 'eq)))
      (else val1))))

(define (expr3 env)
  (pass "Expr3")
  (let loop ((val (term env)))
    (pass2 "Expr3" val)
    (case *token*
      ((+)
       (get-token)
       (loop (append val (term env) (list '+))))
      ((-)
       (get-token)
       (loop (append val (term env) (list '-))))
      (else val))))

;;; 項
(define (term env)
  (pass "Term ")
  (let loop ((val (factor env)))
    (pass2 "Term " val)
    (case *token*
      ((*)
       (get-token)
       (loop (append val (factor env) (list '*))))
      ((/)
       (get-token)
       (loop (append val (factor env) (list '/))))
      ((//)
       (get-token)
       (loop (append val (factor env) (list '//))))
      ((%)
       (get-token)
       (loop (append val (factor env) (list '%))))
      (else val))))

;;; 実引数のコンパイル
(define (compile-argument env)
  (get-token)
  (if (eq? *token* 'rpar)
      (begin (get-token) (list 'args 0))
    (let loop ((n 1) (a '()))
      (let ((expr (expression env)))
        (case *token*
          ((rpar)
           (get-token)
           (append (append a expr) (list 'args n)))
          ((comma)
           (get-token)
           (loop (+ n 1) (append a expr)))
          (else
           (compile-error "unexpected token")))))))

;;; 仮引数の取得
(define (get-parameter)
  (get-token)
  (unless (eq? *token* 'lpar)
    (compile-error "'(' expected"))
  (get-token)
  (let loop ((a '()))
    (let ((val *value*))
      (case *token*
        ((rpar)
         (get-token)
         (reverse a))
        ((ident)
         (let ((val *value*))
           (get-token)
           (loop (cons val a))))
        ((comma)
         (get-token)
         (loop a))
        (else
         (compile-error "unexpected token"))))))

;;; 位置を求める
(define (position var ls)
  (let loop ((i 0) (ls ls))
    (cond ((null? ls) #f)
          ((eqv? var (car ls)) i)
          (else
           (loop (+ i 1) (cdr ls))))))

;;; フレームと局所変数の位置を求める
(define (location var ls)
  (let loop ((i 0) (ls ls))
    (if (null? ls)
        #f
      (let ((j (position var (car ls))))
        (if j
            (cons i j)
          (loop (+ i 1) (cdr ls)))))))

;;; ベクタの生成
(define (create-vector env)
  (get-token)
  (if (eq? *token* 'rbra)
      (begin (get-token) (list 'mvec 0))
    (let loop ((n 1) (a '()))
      (let ((expr (expression env)))
        (case *token*
          ((rbra)
           (get-token)
           (append (append a expr) (list 'mvec n)))
          ((comma)
           (get-token)
           (loop (+ n 1) (append a expr)))
          (else
           (compile-error "unexpected token")))))))

;;; ベクタのコンパイル
(define (compile-vector code env)
  (let loop ((code1 code))
    (get-token)
    (let ((pos (append (expression env) (list 'vref))))
      (unless (eq? *token* 'rbra)
        (compile-error "']' expected"))
      (get-token)
      (cond ((not (eq? *token* 'lbra))
             (append code1 pos))
            (else
             (loop (append code1 pos)))))))

;;; 因子
(define (factor env)
  (pass "Fact ")
  (case *token*
    ((lpar)
     (get-token)
     (begin0
      (expression env)
      (if (eq? *token* 'rpar)
          (get-token)
          (compile-error "')' expected"))))
    ((number)
     (begin0 (list 'ldc *value*) (get-token)))
    ((string)
     (begin0 (list 'ldc *value*) (get-token)))
    ((lbra)
     ;; ベクタの生成
     (create-vector env))
    ((not)
     (get-token)
     (append (factor env) (list 'not)))
    ((+)
     ;; 単項演算子 (+ をはずすだけ)
     (get-token)
     (factor env))
    ((-)
     ;; 単項演算子
     (get-token)
     (append (factor env) (list 'neg)))
    ((fn)
     ;; クロージャの生成
     (let ((code (list 'ldf
                       (append (make-tail! (compile-block (cons (get-parameter) env)))
                               (list 'rtn)))))
       (get-token)
       (if (eq? *token* 'lpar)
           ;; 関数呼び出し
           (append (compile-argument env) code (list 'app))
         code)))
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
    ((ident)
     (let ((code #f)
           (pos (location *value* env)))
       (if pos
           ;; 局所変数
           (set! code (list 'ld pos))
         ;; 大域変数
         (set! code (list 'ldg (get-gvar *value*))))
       (get-token)
       (cond ((eq? *token* 'lpar)
              ;; 関数呼び出し
              (append (compile-argument env) code (list 'app)))
             ((eq? *token* 'lbra)
              ;; ベクタのアクセス
              (compile-vector code env))
             (else
              ;; 変数
              code))))
    (else
     (compile-error "unexpected token"))))


;;; then 部: 文の並びを 'else' または 'end' の手前までコンパイル。戻り時 *token* は消費しない。
(define (compile-then-part env)
  (let loop ((code '()))
    (cond ((or (eq? *token* 'end) (eq? *token* 'else))
           (if (null? code) (list 'ldc 0 'join) (append code (list 'join))))
          (else
           (let ((code1 (compile-statement env)))
             (get-token)
             (cond ((or (eq? *token* 'end) (eq? *token* 'else))
                    (append code code1 (list 'join)))
                   (else
                    (loop (append code code1 (list 'pop))))))))))

;;; else 部: 文の並びを 'end' の手前までコンパイル。戻り時 *token* は 'end'（消費しない）。
(define (compile-else-part env)
  (let loop ((code '()))
    (cond ((eq? *token* 'end)
           (list 'ldc 0 'join))
          (else
           (let ((code1 (compile-statement env)))
             (get-token)
             (cond ((eq? *token* 'end)
                    (append code code1 (list 'join)))
                   (else
                    (loop (append code code1 (list 'pop))))))))))

;;; if 文のコンパイル（then/else は end で囲まない文の並び、if 全体で end は 1 つ）
(define (compile-if env)
  (let ((test-form (expression env))
        (then-form #f)
        (else-form #f))
    (unless (eq? *token* 'then)
      (compile-error "if: then expected"))
    (get-token)
    (set! then-form (compile-then-part env))
    (cond ((eq? *token* 'end)
           ;; end は消費しない（外側の block が使う）
           (set! else-form (list 'ldc 0 'join)))
          ((eq? *token* 'else)
           (get-token)
           (set! else-form (compile-else-part env)))
          (else
           (compile-error "if: 'else' or 'end' expected")))
    (append test-form (list 'sel then-form else-form))))

;;; while 文のコンパイル
(define (compile-while env)
  (let ((test (expression env))
        (body #f))
    (unless (eq? *token* 'do)
      (compile-error "while: do expected"))
    (get-token)
    (set! *loop-depth* (+ *loop-depth* 1))
    (set! body (append (compile-block env) (list 'rpt)))
    (set! *loop-depth* (- *loop-depth* 1))
    (append (list 'bgn) test (list 'whl) (list body))))

;;; block 文のコンパイル
(define (compile-block env)
  (let loop ((code '()))
    (let ((code1 (compile-statement env)))
      (get-token)  ; 実行文の終端 (semic, end) を読み飛ばす
      (cond ((eq? *token* 'end)
             (append code code1))
            (else
             (loop (append code code1 (list 'pop))))))))

;;; let 文のコンパイル
(define (compile-let env)
  (let loop ((vars '()) (code '()))
    (cond ((eq? *token* 'in)
           (get-token)
           ;; 本体コードの生成
           ;; 末尾は app ではなく lapp（let 専用の適用命令）。
           ;; lapp はダンプへ 'let タグのフレームを積むので、let 本体の return は
           ;; ここで止まらず、外側の関数まで貫通する (FC-01)。
           (append code
                   (list 'args
                         (length vars)
                         'ldf
                         (append (make-tail! (compile-block (cons (reverse vars) env)))
                                 (list 'rtn))
                         'lapp)))
          ((eq? *token* 'ident)
           (let ((var *value*))
             (get-token)
             (unless (eq? *token* '=)
               (compile-error "let: invalid assignment form"))
             (get-token)
             (loop (cons var vars) (append code (expr1 env)))))
          ((eq? *token* 'comma)
           (get-token)
           (loop vars code))
          (else
           (compile-error "let: unexpected token")))))

;;; 実行文のコンパイル
(define (compile-statement env)
  (case *token*
    ((begin)
     (get-token)
     (compile-block env))
    ((if)
     (get-token)
     (compile-if env))
    ((while)
     (get-token)
     (compile-while env))
    ((let)
     (get-token)
     (compile-let env))
    ((break)
     (when (<= *loop-depth* 0)
       (compile-error "break: not in while"))
     (get-token)
     (begin0
       (list 'brk)
       (unless (eq? *token* 'semic)
         (compile-error "';' expected"))))
    ((return)
     (get-token)
     (begin0
       (append (expression env) (list 'ret))
       (unless (eq? *token* 'semic)
         (compile-error "';' expected"))))
    (else
     ;; 式文
     (begin0
       (expression env)
       (unless (eq? *token* 'semic)
         (compile-error "';' expected"))))))

;;; 末尾呼び出し最適化（micro_Scheme8 方式）
;;;
;;; 末尾位置は「関数本体（def / fn / let の本体）の最後の文」として
;;; コンパイル時の構造から確定する。後付けで全コードを走査する大域パスは使わない。
;;; 各関数本体を構築する箇所で make-tail! を呼び、その本体コード（末尾の rtn を
;;; 付ける前）を破壊的に末尾呼び出し化する。
;;;   - 末尾が関数呼び出し  ... app          → ... tapp
;;;   - 末尾が if/and/or     ... sel <t> <e>  → ... selr <t'> <e'>（両節を末尾化）
;;;   - それ以外（値を残すだけ）              → そのまま（本体末尾の rtn が値を返す）
;;; ※ 真のトップレベル文には適用しない（戻り先 call フレームが無いため）。
;;; ※ callcc の内部適用（app）は factor 側で常に非末尾として生成するため、
;;;    継続の捕捉・復帰と安全に共存する。
(define (make-tail! code)
  (let ((n (length code)))
    (cond ((zero? n) code)
          ;; 末尾が呼び出し: app / lapp → tapp
          ;; 末尾位置の let (lapp) も tapp でよい。新しいフレームを積まないので
          ;; let 本体の return は外側の関数の call フレームを見つける。
          ((memq (list-ref code (- n 1)) '(app lapp))
           (set-car! (list-tail code (- n 1)) 'tapp)
           code)
          ;; 末尾が if/and/or: sel <then> <else> → selr <then'> <else'>
          ((and (>= n 3) (eq? (list-ref code (- n 3)) 'sel))
           (let ((sel-pair (list-tail code (- n 3))))
             (set-car! sel-pair 'selr)
             (tailify-branch! (cadr sel-pair))    ; then 節
             (tailify-branch! (caddr sel-pair)))  ; else 節
           code)
          ;; それ以外は値を残すだけ。本体末尾の rtn が返す。
          (else code))))

;;; if/and/or の節（末尾が join）を末尾位置用に変換する（破壊的）。
;;;   末尾の join を rtn に置換し、その直前の文をさらに末尾化する。
(define (tailify-branch! branch)
  (let ((n (length branch)))
    (when (>= n 1)
      ;; 末尾 join → rtn
      (set-car! (list-tail branch (- n 1)) 'rtn)
      (cond ((and (>= n 2) (memq (list-ref branch (- n 2)) '(app lapp)))
             ;; ... app rtn → ... tapp rtn （lapp も同様）
             (set-car! (list-tail branch (- n 2)) 'tapp))
            ((and (>= n 4) (eq? (list-ref branch (- n 4)) 'sel))
             ;; ... sel <t> <e> rtn → ... selr <t'> <e'> rtn
             (let ((sel-pair (list-tail branch (- n 4))))
               (set-car! sel-pair 'selr)
               (tailify-branch! (cadr sel-pair))
               (tailify-branch! (caddr sel-pair))))))))

;;; コンパイル
(define (compile)
  (cond ((eq? *token* 'def)
         ;; 関数定義
         (get-token)
         (unless (eq? *token* 'ident)
           (compile-error "invalid def form"))
         (let ((name *value*)
               (code (append (make-tail! (compile-block (list (get-parameter))))
                     (list 'rtn))))
           ;; 末尾呼び出し化は make-tail! で本体構築時に実施済み
           (list 'ldf code 'gset (get-gvar name))))
        (else
         (let ((code (compile-statement '())))
           ;; 真のトップレベル文は末尾位置ではない（戻り先 call フレームが無い）。
           ;; 関数本体（def/fn/let）は各構築時に make-tail! で最適化済み。
           code))))

;;;
;;; 仮想マシン
;;;

;;; 局所変数の値を求める
(define (get-lvar e i j)
  (list-ref (list-ref e i) j))

;;; 局所変数の値を更新する
(define (set-lvar! e i j val)
  (set-car! (drop (list-ref e i) j) val))

(define (vm s e c d)
  (case (car c)
    ((+)
     (vm (cons (+ (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((-)
     (vm (cons (- (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((*)
     (vm (cons (* (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((/)
     (vm (cons (/ (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((//)
     (vm (cons (quotient (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((%)
     (vm (cons (modulo (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((==)
     (vm (cons (if (= (cadr s) (car s)) 1 0) (cddr s)) e (cdr c) d))
    ((!=)
     (vm (cons (if (= (cadr s) (car s)) 0 1) (cddr s)) e (cdr c) d))
    ((<)
     (vm (cons (if (< (cadr s) (car s)) 1 0) (cddr s)) e (cdr c) d))
    ((<=)
     (vm (cons (if (<= (cadr s) (car s)) 1 0) (cddr s)) e (cdr c) d))
    ((>)
     (vm (cons (if (> (cadr s) (car s)) 1 0) (cddr s)) e (cdr c) d))
    ((>=)
     (vm (cons (if (>= (cadr s) (car s)) 1 0) (cddr s)) e (cdr c) d))
    ((eq)
     (vm (cons (if (eqv? (cadr s) (car s)) 1 0) (cddr s)) e (cdr c) d))
    ((neg)
     (vm (cons (- (car s)) (cdr s)) e (cdr c) d))
    ((not)
     (vm (cons (if (zero? (car s)) 1 0) (cdr s)) e (cdr c) d))
    ((ld)
     (let ((pos (cadr c)))
       (vm (cons (get-lvar e (car pos) (cdr pos)) s) e (cddr c) d)))
    ((ldc)
     (vm (cons (cadr c) s) e (cddr c) d))
    ((ldg)
     ;; c = (ldg (sym . val) ...)
     (vm (cons (cdr (cadr c)) s) e (cddr c) d))
    ((ldf)
     (vm (cons (list 'closure (cadr c) e) s) e (cddr c) d))
    ((ldct)
     ;; 継続
     (vm (cons (list 'continuation s e (drop (cddr c) (cadr c)) d) s)
         e
         (cddr c)
         d))
    ((lset)
     (let ((pos (cadr c)))
       (set-lvar! e (car pos) (cdr pos) (car s))
       (vm s e (cddr c) d)))
    ((gset)
     ;; c = (gset (sym . val) ...)
     (set-cdr! (cadr c) (car s))
     (vm s e (cddr c) d))
    ((app)
     (let ((clo (car s)) (lvar (cadr s)))
       (case (pop! clo)
         ((primitive)
          ;; (primitive function)
          (vm (cons (apply (car clo) lvar) (cddr s)) e (cdr c) d))
         ((continuation)
          (vm (cons (car lvar) (car clo)) (cadr clo) (caddr clo) (cadddr clo)))
         (else
          ;; (closure code env)
          (vm '()
              (cons lvar (cadr clo))
              (car clo)
              (cons (list 'call (cddr s) e (cdr c)) d))))))
    ((lapp)
     ;; let の適用 (FC-01)
     ;; app と同じだが、ダンプへ積むフレームのタグが 'call ではなく 'let。
     ;; rtn は 'let も受理するので let は正しく値を返し、
     ;; ret (return) は 'call だけを探すので let を貫通して関数から抜ける。
     ;; let の被適用体は compile-let が ldf で作ったクロージャに限られる。
     (let ((clo (car s)) (lvar (cadr s)))
       (pop! clo)                       ; 'closure タグを捨てる
       (vm '()
           (cons lvar (cadr clo))
           (car clo)
           (cons (list 'let (cddr s) e (cdr c)) d))))
    ((tapp)
     (let ((clo (car s)) (lvar (cadr s)))
       (case (pop! clo)
         ((primitive)
          ;; (primitive function)
          (vm (cons (apply (car clo) lvar) (cddr s)) e (cdr c) d))
         ((continuation)
          (vm (cons (car lvar) (car clo)) (cadr clo) (caddr clo) (cadddr clo)))
         (else
          ;; (closure code env)
          (vm (cddr s) (cons lvar (cadr clo)) (car clo) d)))))
    ((rtn)
     ;; 関数本体・let 本体のどちらの終端でも使われるので両方のタグを受理する
     (let ((save (car d)))
       (unless (and (pair? save) (memq (car save) '(call let)))
         (error "rtn: call frame not found" save))
       (vm (cons (car s) (cadr save)) (caddr save) (cadddr save) (cdr d))))
    ((ret)
     ;; return: dump を巻き戻して call フレームへ戻る。
     ;; 'let フレーム（let の適用）と 'loop フレーム（while）は読み飛ばすので、
     ;; let や while の内側の return も関数から抜ける (FC-01)。
     (let loop ((d1 d))
       (when (null? d1)
         (error "ret: call frame not found"))
       (let ((save (car d1)))
         (if (and (pair? save) (eq? (car save) 'call))
             (vm (cons (car s) (cadr save)) (caddr save) (cadddr save) (cdr d1))
             (loop (cdr d1))))))
    ((sel)
     (let ((t-clause (cadr c))
           (e-clause (caddr c)))
       (if (zero? (car s))
           (vm (cdr s) e e-clause (cons (cdddr c) d))
         (vm (cdr s) e t-clause (cons (cdddr c) d)))))
    ((selr)
     (let ((t-clause (cadr c))
           (e-clause (caddr c)))
       (if (zero? (car s))
           (vm (cdr s) e e-clause d)
         (vm (cdr s) e t-clause d))))
    ((join)
     (vm s e (car d) (cdr d)))
    ((pop)
     (vm (cdr s) e (cdr c) d))
    ((dup)
     (vm (cons (car s) s) e (cdr c) d))
    ((args)
     (let loop ((n (cadr c)) (a '()))
       (if (zero? n)
           (vm (cons a s) e (cddr c) d)
         (loop (- n 1) (cons (pop! s) a)))))
    ((bgn)
     ;; while 用のループフレームを積む（break で識別する）
     (vm s e (cdr c) (cons (list 'loop (cdr c)) d)))
    ((whl)
     (if (zero? (car s))
         (vm (cons 0 (cdr s)) e (cddr c) (cdr d))
       (vm (cdr s) e (cadr c) d)))
    ((rpt)
     (let ((frame (car d)))
       (unless (and (pair? frame) (eq? (car frame) 'loop))
         (error "rpt: loop frame not found" frame))
       (vm (cdr s) e (cadr frame) d)))
    ((brk)
     ;; 一番内側の while から脱出
     (let loop ((d1 d))
       (when (null? d1)
         (error "brk: loop frame not found"))
       (let ((frame (car d1)))
         (if (and (pair? frame) (eq? (car frame) 'loop))
             (let* ((saved (cadr frame))
                    (rest (let find ((xs saved))
                            (cond ((null? xs)
                                   (error "brk: whl not found in saved code" saved))
                                  ((eq? (car xs) 'whl) (cddr xs))
                                  (else (find (cdr xs)))))))
               ;; whl の false と同じく 0 を返し、ループフレームを捨てる
               ;; break はスタックをクリアして 0 だけ残す（より堅牢）
               (vm (list 0) e rest (cdr d1)))
             (loop (cdr d1))))))
    ((vref)
     (vm (cons (vector-ref (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((vset)
     (let ((v (car s)))
       (vector-set! (caddr s) (cadr s) v)
       (vm (cons v (cdddr s)) e (cdr c) d)))
    ((mvec)
     (let ((a (make-vector (cadr c))))
       (let loop ((n (cadr c)))
         (cond ((zero? n)
                (vm (cons a s) e (cddr c) d))
               (else
                (vector-set! a (- n 1) (pop! s))
                (loop (- n 1)))))))
    ((halt)
     (car s))
    (else
     (error "vm: unexpected code:" (car c)))))

;;; ファイルのロード
(define (load-file name)
  (define (restore-env xs)
    (set! *input* (list-ref xs 0))
    (set! *token* (list-ref xs 1))
    (set! *value* (list-ref xs 2))
    (set! *ch*    (list-ref xs 3))
    (set! *line*  (list-ref xs 4))
    (set! *col*   (list-ref xs 5)))
  (call-with-input-file name
    (lambda (in)
      (let ((env (list *input* *token* *value* *ch* *line* *col*)))
        (set! *input* in)
        (set! *line* 1)
        (set! *col*  0)
        (nextch)
        ;; エラー時は入力環境を復旧してから元の例外をそのまま再送出する (FC-03)。
        ;; 旧版は with-exception-handler のハンドラから「戻って」いたが、
        ;; error による非継続例外のハンドラから戻ることは R7RS では許されず、
        ;; 本来のエラーが二次例外に包まれて読めなくなっていた。
        (guard (err (#t
                     (let ((ln *line*) (cl *col*))
                       (restore-env env)
                       (format #t "  [in file ~a, line ~a, column ~a]~%" name ln cl))
                     (raise err)))
          (let loop ()
            (get-token)
            (when (not (eq? *token* 'eof))
                  (vm '() '() (append (compile) (list 'halt)) '())
                  (loop)))
          (restore-env env))))))

;;; 入力をクリアする
(define (clear-input-data)
  (do ()
      ((eqv? *ch* #\newline))
    (nextch)))

;;; プロンプトの表示
(define (prompt)
  (display "Calc> ")
  (flush-output-port)
  (set! *line* 0)
  (set! *col* 0))

(define (calc)
  (prompt)
  (nextch)
  (call/cc
    (lambda (break)
      (let loop ()
        (guard (err
                (else (display "ERROR: ")
                      (display (error-object-message err))
                      (unless
                       (null? (error-object-irritants err))
                       (display (error-object-irritants err)))
                      (newline)
                      (clear-input-data)))
          (get-token)
          (when (eqv? *token* 'eof) (break #t))
          (let ((code (append (compile) (list 'halt))))
            ;; 末尾最適化は関数本体（def/fn/let）の構築時に実施済み
            (pass3 code)
            (let* ((s (current-jiffy))
                   (val (vm '() '() code '())))
;               (display (inexact (/ (- (current-jiffy) s) (jiffies-per-second))))
;               (newline)
              (display "=> ")
              (print-data val)
              (newline))))
        (prompt)
        (loop)))))

;;; 実行
(calc)

;; 起動法
; gosh -r7 -A . fncalc9.scm
; load("list2.cal");
;=> 1
;; ジェネレータ生成
;Calc> def make_gen(proc, ls)
;  let resume = 0 in
;    resume = fn(ret)
;      proc(fn(x) ret = callcc(fn(cont) resume = cont; ret(x); end); end, ls);
;      ret(nil);
;    end;
;    fn() callcc(fn(cont) resume(cont); end); end;
;  end
;end
;=> closure
;; 順列生成
;Calc> def perm(f, ls)
;  let iter = 0 in
;    iter = fn(ls, a)
;      if null(ls) then
;        f(a);
;      else
;        foreach(fn(x) iter(remove(x, ls), cons(x, a)); end, ls);
;      end
;    end;
;    iter(ls, nil);
;  end
;end
;=> closure
;; 順列の生成テスト（１回目）
;Calc> g = make_gen(perm, iota(1, 3));
;=> closure
;Calc> printlist(g());
;(3 2 1)
;=> 0
;Calc> printlist(g());
;(2 3 1)
;=> 0
;Calc> printlist(g());
;(3 1 2)
;=> 0
;Calc> printlist(g());
;(1 3 2)
;=> 0
;Calc> printlist(g());
;(2 1 3)
;=> 0
;Calc> printlist(g());
;(1 2 3)
;=> 0
;Calc> printlist(g());
;()
;=> 0
;; 順列の生成テスト（２回目）
;Calc> g = make_gen(perm, iota(1, 3));			<--- make_gen() は何度でも動く
;=> closure
;Calc> printlist(g());
;(3 2 1)
;=> 0
;
; load("sample2.cal");
; => 1
; quick_sort([5, 6, 4, 7, 3, 8, 2, 9, 1, 0]);
; => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
; sieve(100);
; 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 => 0
; factorization(5244319080000);
; 2 2 2 2 2 2 3 3 3 3 3 5 5 5 5 7 7 7 11 11 13 => 13
; count_leaf([1, [2, [3, 3, 3], 4], 5]);
; => 7
; flatten([1, [2, [3, 4, 5], 4], 5]);
; => [1, 2, 3, 4, 5, 4, 5]

;; 素数計算
; primes = make_vector(100, 0);
; => [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
; primes[0] = 2;
; => 2
; primes[1] = 3;
; => 3
; index = 2;
; => 2
; def is_prime(n)
;   let i= 1, p = 0 in
;     while (i < index) do
;       p = primes[i];
;       if n < (p * p) then return 1; end
;       if modulo(n, p) == 0 then return -1; end
;       i = i + 1;
;     end
;     1;
;   end
; end
; => closure
; def calc_primes(n)
;   let i = 5 in
;     while (i < n) do
;       if is_prime(i) > 0 then
;         primes[index] = i;
;         index = index + 1;
;         if index >= 100 then return 0; end
;       end
;       i = i + 2;
;     end
;   end
; end
; => closure
; calc_primes(500);
; => 0
; primes;
; => [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 0, 0, 0, 0, 0]

