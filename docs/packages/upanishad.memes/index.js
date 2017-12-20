var STYLE = {
    display: function (v) {
        return v ? '' : 'display:none';
    }
};

var REDUCER = new Reducer ();
var ACTIONS = new Actions ();
var STORE = new Simple_Redux_Store (REDUCER, {
    config: CONFIG
    , classes: CLASSES
    , packages: PACKAGES
    , symbols: SYMBOLS
    , tags: {
        "home": true
        , "make-memes": false
        , "add-meme": false
        , "get-meme": false
        , "remove-meme": false
    }
});

route(function(collection, id, action) {
    STORE.dispatch(ACTIONS.movePage(collection));
});

route.start(true);

riot.mount('*');
