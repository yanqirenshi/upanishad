console.log([location.protocol, location.host, location.port]);
var API = new Vanilla_Ajax({
    scheme: location.protocol,
    host: location.host,
    port: location.port
});
var ACTIONS = new Actions();
var REDUCER = new Reducer();
var STORE = new Store(REDUCER, {
    pages: {
        'readme': { code:'readme', label:'README', display: true },
        'page-1': { code:'page-1', label:'Packages', display: false,
                    packages: []
                  },
        'page-2': { code:'page-2', label:'Page-1', display: false },
        'page-3': { code:'page-3', label:'Page-2', display: false }
    }
});

route(function (a) {});
route.start();

riot.mount('*');
