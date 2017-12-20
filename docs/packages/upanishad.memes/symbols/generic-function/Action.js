class Actions extends Simple_Redux_Actions {
    movePage (name) {
        var tags = Object.assign({}, STORE.state().tags);
        var tmp_name = name=='' ? 'home' : name;

        for (var k in tags)
            tags[k]=(k==tmp_name);

        return {
            type: 'MOVE-PAGE',
            data: { tags: tags }
        };
    }
}
