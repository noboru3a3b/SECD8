### SECD8
SECD 仮想マシン方式による Scheme コンパイラです（バージョン 8）。  
私が考えたわけではなく、M.Hiroi さんによる以下のページが元になっています（有益な情報ありがとうございます）。  
http://www.nct9.ne.jp/m_hiroi/clisp/abcl55.html

SECD 仮想マシン & Scheme コンパイラを Common Lisp で実装しています。  
Common Lisp としては sbcl を仮定。

### 使い方
#### Linux の場合
rlwrap ./micro_Scheme8.lisp

#### Windows の場合
sbcl --script micro_Scheme8.lisp
