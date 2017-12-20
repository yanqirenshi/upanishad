class Simple_Redux_Store {
    constructor (reducer, contents) {
        this._reducer = reducer;
        this._contents = contents;
        this._subscribes = [];
    }

    state () {
        return Object.assign({}, this._contents);
    }

    get (keys) {
        var keyList = keys.split(".");
        var tmp = this._contents;
        for (var i=0;i<keyList.length;i++) {
            var key = keyList[i];
            if (tmp)
                tmp = tmp[key];
        }
        return tmp;
    }

    dispatch (action) {
        this._contents = this._reducer.put(this._contents, action);

        var funcs = this._subscribes;
        for (var i in funcs)
            funcs[i](action);
    }

    subscribe (func) {
        this._subscribes.push(func);
    }
}
