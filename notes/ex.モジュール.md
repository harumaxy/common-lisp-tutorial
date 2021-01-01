# Common Lisp モジュール

ライブラリのようなシステムが制定されてない？
ros から repl を起動するときはとりあえず下のコマンドでできた

```sh
ros run --load 6.interface_to_world.lisp --load text_game.lisp
```


# load 関数

```lisp
(load "text_game.lisp")

```

ファイルの先頭で上のやつを書いとけば、
その中で定義した関数、変数が使えるようになってた。

ローカルインポートはこれで良さげ


# clisp の場合

`-i <input-file>` をつけて実行
最初にセットアップファイルがロードされる

