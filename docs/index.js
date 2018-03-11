var Request = new Vanilla_Ajax('http', 'localhost', '');
var ACTIONS = new Actions();
var REDUCER = new Reducer();
var STORE = new Store(REDUCER, {
    pages: {
        'readme': { code:'readme', label:'README', display: true },
        'page-1': { code:'page-1', label:'Page-1', display: false },
        'page-2': { code:'page-2', label:'Page-1', display: false },
        'page-3': { code:'page-3', label:'Page-2', display: false }
    }
});

route(function (a) {});
route.start();

riot.mount('*');
