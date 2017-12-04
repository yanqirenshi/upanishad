# Overview

meme を管理するための器です。

memes は POOL の memes スロットで管理されます。

# Objects

```
 +---------------+
 | pool          |
 |===============|      +-----------+
 | memes         |o---->| memes     |
 |---------------|      |===========|        +----------------+
 | index-at      |      | objects   |o------>| list           |
 | add-index     |      | %id-index |o--+    |================|
 | remove-index  |      |-----------|   |    | +------+   |   |
 | add-object    |      +-----------+   |    | | meme |+  |<- - - -+
 | remove-object |                      |    | +------+|+ |   |    :
 +---------------+                      |    |  +------+| |   |    :
                                        |    |   +------+ |   |    :
                                        |    +----------------+    :
                                        |                         ref
                                        |   +-----------------+    :
                                       `--->| Hash table      |    :
                                            |=================|    :
                                            | key (%id)       |    :
                                            | value           |o- -+
                                            +-----------------+
```

## pool

## memes

### objects

### %id-index

# snapshot, restor

## snapshot
1. root-object:objects -----> plist: '({class-symbol} {list:object} ....)
2. plist: '({class-symbol} {list:object} ....) -----> xml

## restor

1. xml -----> plist: '({class-symbol} {list:object} ....)
2. plist: '({class-symbol} {list:object} ....) -----> root-object
