class Reducer extends Simple_Redux_Reducer {
    put (state, action) {
        switch (action.type) {

        case 'MOVE-PAGE':
            return this.merge(state, action.data);

        default:
            return state;
        }
    }
}
