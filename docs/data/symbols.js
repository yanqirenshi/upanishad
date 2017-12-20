var SYMBOLS = [
    {
        "package": "upanishad",
        "name": "pool",
        "type": "class",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "get-meme-at-slot",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "get-meme",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "find-meme",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "slot-value-changed-p",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-change-meme-slots",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-create-meme",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-delete-meme",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-add-memes",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-remove-memes",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "get-memes",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "get-preference",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-set-preference",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "all-preferences-keys",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "execute-transaction",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "getter-name",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "defreader",
        "type": "macro",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "defwriter",
        "type": "macro",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "set-pathname",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "init-snapshot-pathname",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "init-transaction-log-pathname",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "initialize-instance",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "transaction-log-stream",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "poolp",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "get-root-object",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "(setf get-root-object)",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "remove-root-object",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "get-index-object",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "(setf get-index-object)",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "get-option",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "(setf get-option)",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "print-object",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "no-rollback-error",
        "type": "condition",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "initiates-rollback",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "log-transaction",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "execute-on",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "execute",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "query",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "close-open-streams",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "stop",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "datastore-files",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "delete-all-files",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "clear-objects",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "totally-destroy",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "make-pool",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "make-transaction",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "reset-known-slots",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-create-%id-counter",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "next-%id",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "get-index",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "%tx-add-index",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-add-index",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "%tx-remove-index",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-remove-index",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-add-meme-to-index",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad",
        "name": "tx-remove-meme-from-index",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad.memes",
        "name": "memes",
        "type": "class",
        "description": ""
    }
    , {
        "package": "upanishad.memes",
        "name": "meme-class",
        "type": "function",
        "description": "accessor"
    }
    , {
        "package": "upanishad.memes",
        "name": "meme-list",
        "type": "function",
        "description": "accessor"
    }
    , {
        "package": "upanishad.memes",
        "name": "%id-index",
        "type": "function",
        "description": "accessor"
    }
    , {
        "package": "upanishad.memes",
        "name": "get-meme",
        "type": "function",
        "description": ""
    }
    , {
        "package": "upanishad.memes",
        "name": "add-meme",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad.memes",
        "name": "make-memes",
        "type": "generic-function",
        "description": ""
    }
    , {
        "package": "upanishad.memes",
        "name": "remove-meme",
        "type": "generic-function",
        "description": ""
    }
];
/*
    , {
        "package": "",
        "name": "",
        "type": "",
        "description": ""
    }
*/
