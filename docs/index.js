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
        'page-2': { code:'page-2', label:'Classes', display: false,
                    classes: []
                  },
        'page-3': { code:'page-3', label:'Symbols', display: false,
                    symbols: []
                  }
    }
});

route(function (a) {});
route.start();

riot.mount('app');

/* DevCode */
let Metronome = new Vanilla_metronome({
    interval: 1000,
    tick: function (count) {
        if (location.host=='localhost')
            return;

        let page = function () {
            let pages = STORE.state().pages;
            for (var k in pages)
                if (pages[k].display)
                    return pages[k];
            return null;
        }();

        switch (page.code) {
        case 'page-1':
            ACTIONS.loadDataPage1();
            break;
        case 'page-2':
            ACTIONS.loadDataPage2();
            break;
        case 'page-3':
            ACTIONS.loadDataPage3();
            break;
        }

    }
});
Metronome.start();
