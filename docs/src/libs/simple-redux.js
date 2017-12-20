class Simple_Redux_Actions {
    constructor () {}
    sampleAction (data) {
        return {
            type: 'SAMPLE-ACTION',
            data: data
        };
    }
}
class Simple_Redux_Reducer {
    constructor () {}

    ht_p (v) {
        return v instanceof Object && !(v instanceof Array);
    }

    merge_core (ht, add_ht) {
        for (var k in add_ht) {
            var v = add_ht[k];
            if (!this.ht_p(v))
                ht[k] = v;
            else
                ht[k] = this.merge_core (ht[k], add_ht[k]);
        }
        return ht;
    }

    merge (state, add_state) {
        var new_state = Object.assign({}, state);

        new_state = this.merge_core(new_state, add_state);

        return new_state;
    }

    put (state, action) {
        switch (action.type) {

        case 'SAMPLE-ACTION':
            return this.merge(state, {
                'sample': {
                    action: state.data
                }
            });

        default:
            return state;
        }
    }
}
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
