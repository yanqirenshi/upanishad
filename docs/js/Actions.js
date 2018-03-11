class Actions extends Vanilla_Redux_Actions {
    movePage (data) {
        let pages = STORE.state().pages;
        for (var i in pages)
            pages[i].display = pages[i].code == data;

        return {
            type: 'MOVE-PAGE',
            data: { pages: pages }
        };
    }
    loadDataPage1 () {
        API.get(location.pathname + 'data/page-1.json', function (response) {
            this.loadedDataPage1(response);
        }.bind(this));
    }
    loadedDataPage1 (response) {
         STORE.dispatch({
             type: 'LOADED-DATA-PAGE-1',
             data: {
                 pages: {
                     'page-1': response
                 }
             }
         });
    }
}
