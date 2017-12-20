var STYLE = new Style ();
var REDUCER = new Reducer ();
var ACTIONS = new Actions ();
var STORE = new Simple_Redux_Store (REDUCER, {
    config: CONFIG
    , classes: CLASSES
    , packages: PACKAGES
    , symbols: SYMBOLS
    , tags: {
        "home": true
        , "add-meme": false
        , "make-memes": false
        , "remove-meme": false
    }
});

route(function(collection, id, action) {
    STORE.dispatch(ACTIONS.movePage(collection));
});

route.start(true);

riot.mount('*');
