[Object Prevalence](http://prevayler.org/) の Common Lips の実装です。

[cl-prevalence](https://common-lisp.net/project/cl-prevalence/) からフォークしたプロジェクトです。

プロジェクト名は ウパニシャッド哲学のアートマン思想([梵我一如](http://ja.wikipedia.org/wiki/%E3%82%A6%E3%83%91%E3%83%8B%E3%82%B7%E3%83%A3%E3%83%83%E3%83%89))に良く似ているところから来ています。
まぁ、ヨギーなんで影響を受けているんです。

[Graph database](https://en.wikipedia.org/wiki/Graph_database)の [森羅万象](https://github.com/yanqirenshi/shinrabanshou) では永続化層にこの Upanishad を利用しています。


# Usage
データを保管する(snapshotと言う)ためのディレクトリを作成す。
```shell
cd ~/
mkdir ~/up
```

あとは lisp でこんな感じ。
```lisp
;; pool の開始 ※pool は db に脳内変換すると違和感がなくなります。
(defvar *pool* (make-pool "~/up/"))
    ：
  (いろいろな処理) ※準備中
    ：
;; snapshot の実行。  ※snapshot は commit に脳内変換。
(snapshot *test-system*)
    ：
  (いろいろな処理) ※準備中
    ：
;; restore の実行  ※restore は rollback に脳内変換。
(restore *bank-system*)

;; pool の停止
(close-open-streams *test-system*)
```

# Dependencies
これだけ。

| libraly    | description |
|------------|-------------|
| [alexandria](https://common-lisp.net/project/alexandria/) |             |
| [s-xml](https://common-lisp.net/project/s-xml/)      |             |
| [s-sysdeps](https://github.com/svenvc/s-sysdeps)  |             |

`qluickload` すれば気にする必要なし。

# Installation
飾りっけなし。
``` lisp
(ql:quickload :upanishad)
(ql:quickload :upanishad-test)
(upanishad-test:run!)
```
サンキュー quicklisp。

# Author

yanqirenshi (yanqirenshie@gmail.com)

# Copyright

Copyright (c) 2013 yanqirenshi (yanqirenshie@gmail.com)

# License

Licensed under the LLGPL License.
