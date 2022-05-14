#!/usr/local/bin/sbcl --script
;
; secd.l : SECD 仮想マシンによる Scheme コンパイラ
;
;          (1) 基本機能の実装
;          (2) 伝統的なマクロの実装
;          (3) 継続の実装
;          (4) 末尾再帰最適化
;
;          Copyright (C) 2009 Makoto Hiroi
;

; 大域変数
(defvar *macro-print-flag* nil)  ; add macro-print switch okada-n
(defvar *trace-print-flag* nil)  ; add trace-print switch okada-n
(defvar *trace-flag* nil)  ; add trace switch okada-n
(defvar *test-mode-flag* nil)  ; add test-mode switch okada-n
(defvar *test-count* 0)  ; add test-count okada-n
(defvar *pass-count* 0)  ; add pass-count okada-n
(defvar *ng-count* 0)  ; add ng-count okada-n

(setq *print-circle* t)  ; DEBUG for print-env okada-n

; 変数の位置を求める
(defun position-var (sym ls)
  (labels ((iter (i ls)
             (cond ((null ls) nil)
                   ((symbolp ls)
                    (if (eq sym ls) (- (1+ i)) nil))
                   ((eq sym (car ls)) i)
                   (t (iter (1+ i) (cdr ls))))))
    (iter 0 ls)))

; フレームと変数の位置を求める
(defun location (sym ls)
  (labels ((iter (i ls)
             (if (null ls)
                 nil
               (let ((j (position-var sym (car ls))))
                 (if j
                     (cons i j)
                   (iter (1+ i) (cdr ls)))))))
    (iter 0 ls)))

; 自己評価フォームか
(defun self-evaluation-p (expr)
  (and (atom expr) (not (symbolp expr))))

; マクロか
(defun macro-p (expr)
  (if (consp expr)  ; for gvar -> plist okada-n
      nil
    (let ((val (get expr :gvar)))
      (and val (consp val) (eq 'macro (car val))))))

; マクロのコードを取り出す
(defun get-macro-code (expr)
  (caddr (get-gvar expr)))

; S 式をコンパイルする
(defun compile-expr (expr)
  (comp expr '() '(stop) nil))

; コンパイル本体
(defun comp (expr env code tail)
  (cond ((self-evaluation-p expr)
         (list* 'ldc expr code))
        ((symbolp expr)
         (let ((pos (location expr env)))
           (if pos
               ; 局所変数
               (list* 'ld pos code)
             ; 大域変数
             (list* 'ldg expr code))))
        ((eq (car expr) 'test-start)  ; add test-start switch okada-n
         (setq *test-mode-flag* t)
         (setq *test-count* 0)
         (setq *pass-count* 0)
         (setq *ng-count* 0)
         (list* 'ldc *test-mode-flag* code))
        ((eq (car expr) 'test-end)  ; add test-end switch okada-n
         (setq *test-mode-flag* nil)
         (format t "~%( total: ~S  pass: ~S  NG: ~S )~%"
                 *test-count* *pass-count* *ng-count*)
         (list* 'ldc *test-mode-flag* code))
        ((eq (car expr) 'trace-print)  ; add trace-print switch okada-n
         (setq *trace-print-flag* (not *trace-print-flag*))
         (list* 'ldc *trace-print-flag* code))
        ((eq (car expr) 'macro-print)  ; add macro-print switch okada-n
         (setq *macro-print-flag* (not *macro-print-flag*))
         (list* 'ldc *macro-print-flag* code))
        ((eq (car expr) 'code)  ; add code injection okada-n
         (cadr expr))
        ((eq (car expr) 'quote)
         (list* 'ldc (cadr expr) code))
        ((eq (car expr) 'if)
         (if tail
             ; 末尾呼び出し
             (let ((t-clause (comp (caddr expr) env (list 'rtn) t))  ; okada-n
                   (f-clause
                    (if (null (cdddr expr))
                        (list 'ldc :undef 'rtn)  ; for DEBUG okada-n
                      (comp (cadddr expr) env (list 'rtn) t))))  ; okada-n
               (comp (cadr expr) env (list* 'selr t-clause f-clause (cdr code)) nil))
           (let ((t-clause (comp (caddr expr) env (list 'join) nil))  ; okada-n
                 (f-clause
                  (if (null (cdddr expr))
                      (list 'ldc ':undef 'join)
                    (comp (cadddr expr) env (list 'join) nil))))  ; okada-n
             (comp (cadr expr) env (list* 'sel t-clause f-clause code) nil))))
        ((eq (car expr) 'lambda)
         (let ((body (comp-body (cddr expr) (cons (cadr expr) env) (list 'rtn))))  ; okada-n
           (list* 'ldf body code)))
        ((eq (car expr) 'define)
         (comp (caddr expr) env (list* 'def (cadr expr) code) nil))
        ((eq (car expr) 'define-macro)
         (comp (caddr expr) env (list* 'defm (cadr expr) code) nil))
        ((eq (car expr) 'set!)
         (let ((pos (location (cadr expr) env)))
           (if pos
               ; 局所変数
               (comp (caddr expr) env (list* 'lset pos code) nil)
             ; 大域変数
             (comp (caddr expr) env (list* 'gset (cadr expr) code) nil))))
        ((eq (car expr) 'call/cc)
         (list* 'ldct code 'args 1 (comp (cadr expr) env (cons 'app code) nil)))
        ((eq (car expr) 'apply)
         (complis (cddr expr)
                  env
                  (list* 'args-ap
                         (length (cddr expr))
                         (comp (cadr expr) env (cons 'app code) nil))))
        ((macro-p (car expr))
         ; マクロ展開してからコンパイルする
         (when *macro-print-flag*  ; add macro-print function okada-n 
           (format t "====================~%")
           (format t "Macro: ~S~%" expr))
         (let ((new-expr (vm '()
                             (list (cdr expr))
                             (get-macro-code (car expr))
                             (list (list '() '() '(stop))))))
           (when *macro-print-flag*  ; add macro-print function okada-n
             (format t "Expand: ~S~%" new-expr)
             (format t "====================~%"))
           (comp new-expr env code nil)))
        ((and (symbolp (car expr))  ; add primitive function call 2011-05-18 okada-n
              (gboundp (car expr))
              (eq (car (get-gvar (car expr))) 'primitive))
         (complis (cdr expr)
                  env
                  (list* 'args (length (cdr expr)) 'prim (car expr) code)))
        (t  ; 関数呼び出し
         (complis (cdr expr)
                  env
                  (list* 'args
                         (length (cdr expr))
                         (comp (car expr) env (cons (if tail 'tapp 'app) code) nil))))))

; body のコンパイル
(defun comp-body (body env code)
  (if (null (cdr body))
      (comp (car body) env code t)
    (comp (car body)
          env
          (list* 'pop
                 (comp-body (cdr body) env code))
          nil)))

; 引数を評価するコードを生成する
(defun complis (expr env code)
  (if (null expr)
      code
    (comp (car expr) env (complis (cdr expr) env code) nil)))

;;; 仮想マシン

; 局所変数の値を求める
(defun get-lvar (e i j)
  (if (<= 0 j)
      (nth j (nth i e))
    (nthcdr (- (1+ j)) (nth i e))))

; 局所変数の値を更新する
(defun set-lvar (e i j val)
  (if (<= 0 j)
      (setf (nth j (nth i e)) val)
    (if (= j -1)
        (rplaca (nthcdr i e) val)
      (rplacd (nthcdr (- (+ j 2)) (nth i e)) val))))

(defun gboundp (sym)  ; for add prim 2011-05-18 okada-n
  (let ((val (get sym :gvar :undef)))
    (if (eq val :undef)
        nil
      t)))

; 大域変数の値を求める
(defun get-gvar (sym)
  (let ((val (get sym :gvar :undef)))  ; for gvar -> plist okada-n
    (if (eq val :undef)
        (error "unbound variable: ~S" sym)
      val)))

; 大域変数の値を書き換える
(defun set-gvar (sym val)
  (if (eq (get sym :gvar :undef) :undef)  ; for gvar -> plist okada-n
      (error "unbound variable: ~S" sym)
    (setf (get sym :gvar) val)))

;; Trace 関数
(defun print-stk (c s)
  (case (car c)
    ((sel pop def stop defm lset gset selr rtn prim)  ; add prim 2011-05-18 okada-n
     (format t "S-> ~S~%" (car s)))
    ((app tapp)
     (format t "S-> ~S~%" (car s))
     (format t "    ~S~%" (cadr s))) 
    ((args args-ap)
     (dotimes (n (cadr c))
       (if (= n 0)
           (format t "S-> ~S~%" (nth n s))
           (format t "    ~S~%" (nth n s)))))))

(defun print-env (c e)
  (case (car c)
    ((ld)
     (format t "E-> ~S~%" e))))

(defun print-cmd (c)
  (case (car c)
    ((app rtn join pop stop tapp)
     (format t "C-> ~S~%" (car c)))
    ((sel selr)
     (format t "C-> ~S~%" (car c))
     (format t "    ~S~%" (cadr c))
     (format t "    ~S~%" (caddr c)))
    (t
     (format t "C-> ~S~%" (car c))
     (format t "    ~S~%" (cadr c)))))

(defun print-dmp (c d)
  (case (car c)
    ((rtn join)
     (format t "D-> ~S~%" (car d)))))

; 仮想マシンでコードを実行する
(defun vm (s e c d)
  (when *trace-flag*  ; add trace function okada-n
    (format t "--------------------~%"))
  (loop
    (when *trace-flag*  ; add trace function okada-n
      (print-stk c s)
      (print-env c e)
      (print-cmd c)
      (print-dmp c d)
      (format t "--------------------~%"))
    (case (pop c)
      ((ld)
       (let ((pos (pop c)))
         (push (get-lvar e (car pos) (cdr pos)) s)))
      ((ldc)
       (push (pop c) s))
      ((ldg)
       (push (get-gvar (pop c)) s))
      ((ldf)
       (push (list 'closure (pop c) e) s))
      ((ldct)
       (push (list 'continuation s e (pop c) d) s))
      ((prim)  ; add prim 2011-05-18 okada-n
       (push (apply (cadr (get-gvar (pop c))) (pop s)) s))
      ((lset)
       (let ((pos (pop c)))
         (set-lvar e (car pos) (cdr pos) (car s))))
      ((gset)
       (set-gvar (pop c) (car s)))
      ((app)
       (let ((clo (pop s)) (lvar (pop s)))
         (case (pop clo)
           ((primitive)
            (push (apply (car clo) lvar) s))
           ((continuation)
            (setq s (cons (car lvar) (car clo))
                  e (cadr clo)
                  c (caddr clo)
                  d (cadddr clo)))
           (t
            (push (list s e c) d)
            (setq s nil
                  e (cons lvar (cadr clo))
                  c (car clo))))))
      ((tapp)
       (let ((clo (pop s)) (lvar (pop s)))
         (case (pop clo)
           ((primitive)
            (push (apply (car clo) lvar) s))
           ((continuation)
            (setq s (cons (car lvar) (car clo))
                  e (cadr clo)
                  c (caddr clo)
                  d (cadddr clo)))
           (t
            (setq e (cons lvar (cadr clo))
                  c (car clo))))))
      ((rtn)
       (let ((save (pop d)))
         (setq s (cons (car s) (car save))
               e (cadr save)
               c (caddr save))))
      ((sel)
       (let ((t-clause (pop c))
             (e-clause (pop c)))
         (push c d)
         (setq c (if (eq (pop s) 'false) e-clause t-clause))))
      ((selr)
       (let ((t-clause (pop c))
             (e-clause (pop c)))
         (setq c (if (eq (pop s) 'false) e-clause t-clause))))
      ((join)
       (setq c (pop d)))
      ((pop) (pop s))
      ((args)
       (let ((a nil))
         (dotimes (n (pop c) (push a s))
           (push (pop s) a))))
      ((args-ap)
       (let ((a (copy-list (pop s))))
         (dotimes (n (1- (pop c)) (push a s))
           (push (pop s) a))))
      ((def)
       (let ((sym (pop c)))
         (setf (get sym :gvar) (pop s))  ; for gvar -> plist okada-n
         (push sym s)))
      ((defm)
       (let ((sym (pop c)))
         (setf (get sym :gvar) (cons 'macro (pop s)))  ; for gvar -> plist okada-n
         (push sym s)))
      ((stop) (return (car s)))
      (t (error "unknown opcode")))))

; 大域変数
(setf (get 'true :gvar) 'true)  ; for gvar -> plist okada-n
(setf (get 'false :gvar) 'false)
(setf (get 'nil :gvar) 'nil)
(setf (get 'quit :gvar) 'quit)
(setf (get 'car :gvar) (list 'primitive #'(lambda (x)
                                            (if (null x)
                                                (error "type error -- car: NIL")
                                              (car x)))))
(setf (get 'cdr :gvar) (list 'primitive #'(lambda (x)
                                            (if (null x)
                                                (error "type error -- cdr: NIL")
                                              (cdr x)))))
(setf (get 'cons :gvar) (list 'primitive #'cons))
(setf (get 'list :gvar) (list 'primitive #'list))  ; 2011-05-18 okada-n
(setf (get 'null? :gvar) (list 'primitive #'(lambda (x) (if (null x) 'true 'false))))  ; 2011-05-18 okada-n
(setf (get 'not :gvar) (list 'primitive #'(lambda (x) (if (eq x 'false) 'true 'false))))  ; 2011-05-18 okada-n
(setf (get 'gensym :gvar) (list 'primitive #'gensym))
(setf (get 'eq? :gvar) (list 'primitive #'(lambda (x y) (if (eq x y) 'true 'false))))
(setf (get 'eqv? :gvar) (list 'primitive #'(lambda (x y) (if (eql x y) 'true 'false))))
(setf (get 'equal? :gvar) (list 'primitive #'(lambda (x y) (if (equal x y) 'true 'false))))
(setf (get 'pair? :gvar) (list 'primitive #'(lambda (x) (if (consp x) 'true 'false))))
(setf (get 'display :gvar) (list 'primitive #'(lambda (x) (princ x) ':undef)))
(setf (get 'newline :gvar) (list 'primitive #'(lambda () (terpri) ':undef)))
(setf (get '+ :gvar) (list 'primitive #'+))
(setf (get '- :gvar) (list 'primitive #'-))
(setf (get '* :gvar) (list 'primitive #'*))
(setf (get '/ :gvar) (list 'primitive #'/))
(setf (get '= :gvar) (list 'primitive #'(lambda (&rest x) (if (apply #'= x) 'true 'false))))
(setf (get '< :gvar) (list 'primitive #'(lambda (&rest x) (if (apply #'< x) 'true 'false))))
(setf (get '> :gvar) (list 'primitive #'(lambda (&rest x) (if (apply #'> x) 'true 'false))))
(setf (get '<= :gvar) (list 'primitive #'(lambda (&rest x) (if (apply #'<= x) 'true 'false))))
(setf (get '>= :gvar) (list 'primitive #'(lambda (&rest x) (if (apply #'>= x) 'true 'false))))

;;; read-eval-print-loop

(defun change-readtable ()
  (set-macro-character
   #\`
   #'(lambda (stream char)
       (declare (ignore char))
       (list 'backquote (read stream t nil t))))
  (set-macro-character
   #\,
   #'(lambda (stream char)
       (declare (ignore char))
       (cond ((char= (peek-char nil stream) #\@)
              (read-char stream)
              (list 'splice (read stream t nil t)))
             (t (list 'unquote (read stream t nil t)))))))

(defun repl (&rest file-list)
  (unwind-protect
      (progn
        (change-readtable)
        (dolist (file file-list)
          (with-open-file (in file :direction :input)
            (do ((output t))
                ((eq output nil) (terpri))
              (setf output (vm '() '() (compile-expr (read in nil)) '()))
              (format t "~S " output)
              (if *test-mode-flag*  ; okada-n
                (let ((ans (read in nil)))
                  (setq *test-count* (+ *test-count* 1))
                  (cond ((equal output ans)
                         (setq *pass-count* (+ *pass-count* 1))
                         (format t "~50T pass~%"))
                        (t (setq *ng-count* (+ *ng-count* 1))
                           (format t "~50T  NG  ( expected: ~S )~%" ans))))
                (format t "~%")))))
        (do ((output nil))
            ((eq output 'quit))
          (format t ">>> ")  ; okada-n
          (force-output)
          (let ((expr (compile-expr (read))))
            (format t "Compile => ~S~%" expr)  ; okada-n
            (let ((*trace-flag* *trace-print-flag*))  ; add trace function okada-n
              (setf output (vm '() '() expr '())))
            (format t "Value => ~S~%" output))))  ; okada-n
    (setq *readtable* (copy-readtable nil))))

;; main
(repl "mlib3.scm"  ; for add prim 2011-05-18 okada-n
      "test-case3.scm"  ; for reliability test
)
