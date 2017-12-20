var CLASSES = [
    {
        "package": "upanishad.meme",
        "symbol": "brahman",
        "parents": ["t"],
        "description": "梵我一如を表すための思想的/象徴的なクラス。意味はない。"
    }
    , {
        "package": "upanishad.meme",
        "symbol": "atman",
        "parents": ["brahman"],
        "description": "梵我一如を表すための思想的/象徴的なクラス。意味はない。"
    }
    , {
        "package": "upanishad.meme",
        "symbol": "meme",
        "parents": ["atman"],
        "description": "このクラスも思想的/象徴的なクラスです。"
    }
    , {
        "package": "upanishad.meme",
        "symbol": "blob",
        "parents": ["meme"],
        "description": ""
    }
    , {
        "package": "upanishad.index",
        "symbol": "index",
        "parents": ["t"],
        "description": "インデックスのスーパークラスです。"
    }
    , {
        "package": "upanishad.index",
        "symbol": "slot-index",
        "parents": ["index"],
        "description": ""
    }
    , {
        "package": "upanishad.index",
        "symbol": "slot-index-unique",
        "parents": ["slot-index"],
        "description": ""
    }
    , {
        "package": "upanishad.index",
        "parents": ["slot-index"],
        "symbol": "slot-index-multiple",
        "description": ""
    }
    , {
        "package": "upanishad.memes",
        "symbol": "memes",
        "description": "meme を管理するクラス。 POOL の memes スロットで管理される。",
        "parents": ["t"]
    }
    , {
        "package": "upanishad",
        "symbol": "pool",
        "parents": ["brahman"],
        "description": "",
        "slots": [
            {
                "symbol": "memes",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "indexes",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "options",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "directory",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "snapshot",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "transaction-log",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "transaction-log-stream",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "serializer",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "deserializer",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "file-extension",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "serialization-state",
                "type": "",
                "description": ""
            }
            , {
                "symbol": "transaction-hook",
                "type": "",
                "description": ""
            }
        ]
    }
    , {
        "package": "upanishad",
        "symbol": "guarded-pool",
        "parents": ["pool"],
        "description": ""
    }
    , {
        "package": "upanishad",
        "symbol": "transaction",
        "parents": ["brahman"],
        "description": ""
    }
];
