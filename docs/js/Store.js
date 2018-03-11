class Store extends Vanilla_Redux_Store {
    movePage (data) {
        return {
            type: 'MOVE-PAGE',
            data: data
        };
    }
}
