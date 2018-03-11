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
}
