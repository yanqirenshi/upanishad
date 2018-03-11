class Reducer extends Vanilla_Redux_Reducer {
    put (state, action) {
        switch (action.type) {

        case 'MOVE-PAGE':
            return this.merge(state, action.data);

        case 'LOADED-DATA-PAGE-1':
            console.log(action.data);
            return this.merge(state, action.data);

        default:
            return state;
        }
    }
}
