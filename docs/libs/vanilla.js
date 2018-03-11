class Vanilla_Ajax {
    dump (o) {
        console.log(o);
    }

    /*
     * {
     *    scheme: 'localhost',
     *    port: '55555',
     *    cors: true,
     *    credentials: true,
     *    path: {
     *       prefix: '/pre'
     *    },
     * }
     */
    constructor(params) {
        this.protcol = params.scheme;
        this.host = params.host;
        this.port = params.port;
        this.cors = params.cors ? params.cors : false;
        this.credentials = params.credentials ? params.credentials : null;
        this.path = params.path ? params.path : {
            path: { prefix: '' }
        };
        this.callback = params.callback ? params.callback : {};
    }

    makeUri (path) {
        var port = '';
        if (this.port)
            port = ':' + this.port;

        return this.protcol + '://'
            + this.host
            + port
            + this.path.prefix
            + path;
    }

    error401 () {
        location.href = '/sign-in.html';
        return {};
    }

    errorCase (response) {
        let status = response.status;
        let callback = this.callback[status];

        if (callback)
            return callback(response, this);

        // Other Error Case
        console.log('Error!' + uri);

        return {};
    }

    makeData (method, body) {
        var data =  {
            method: method ? method : 'GET',
            headers: {
                'Accept' : 'application/json'
            }
        };

        if (method=='POST') {
            data.headers['Content-Type'] = 'application/x-www-form-urlencoded';

            if (body)
                data.body = JSON.stringify(body);
        }

        if (this.cors)
            data.mode = 'cors';

        if (this.credentials)
            data.credentials = this.credentials;

        return data;
    }

    get (path, callback) {
        var uri = this.makeUri(path);
        fetch(uri, this.makeData())
            .then(function (response) {
                if (response.ok)
                    return response.json();
                else
                    return this.errorCase(response);
            }.bind(this))
            .then(callback)
            .catch(function(error) {
                this.dump(error);
            }.bind(this));
    }

    post (path, data, callback) {
        var uri = this.makeUri(path);
        fetch(uri, this.makeData('POST', data))
            .then(function (response) {
                if(response.ok)
                    return response.json();
                else
                    return this.errorCase(response);
            })
            .then(callback);
    }

    put (path, data, callback) {
        var uri = this.makeUri(path);
        fetch(uri, this.makeData('PUT', data))
            .then(function (response) {
                if(response.ok)
                    return response.json();
                else
                    return this.errorCase(response);
            })
            .then(callback);
    }
}
class Vanilla_metronome {
    /*
     * {
     *   interval: 1000
     *   tick: function () {}
     * }
     */
    constructor(params) {
        this._timer = null;
        this._count = 0;

        this.interval = params.interval;
        this.tick = params.tick;
    }
    start () {
        if (this._timer)
            this.stop();

        let interval = this.interval ? this.interval : 1000;
        this._timer = setInterval(function () {
            this._count += 1;

            if (this._count==88888887)
                this._count = 1;

            this.tick(this._count);
        }.bind(this), interval);
    }
    stop () {
        if (this._timer)
            clearInterval(this._timer);

        this._count = 0;
        this._timer = null;
    }
}
class Vanilla_Redux_Actions {
    constructor () {}
    sampleAction (data) {
        return {
            type: 'SAMPLE-ACTION',
            data: data
        };
    }
}
class Vanilla_Redux_Reducer {
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
class Vanilla_Redux_Store {
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
class Vanilla_URI {
    parseUrlHash_list2ht (nodes) {
        let out = {
            page: null,
            type: null,
            symbol: null
        };

        let len = nodes .length;

        out.page = len==0 ? '/' : nodes[0];

        if (out.page=='symbols') {
            if (len>=2)
                out.type = nodes[1];
            if (len>=3)
                out.symbol = nodes[2];
            return out;
        }

        return out;
    }
    parseUrlHash (hash) {
        let ret = hash.match(/#(.*)/) || hash.match(/^#(.*)$/);
        let nodes = ret ? ret[1].split('/') : [];

        return this.parseUrlHash_list2ht(ret[1].split('/'));
    }
}
