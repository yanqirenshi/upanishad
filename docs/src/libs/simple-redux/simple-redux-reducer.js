class Simple_Redux_Reducer {
    constructor () {}

    merge (state, newState) {
        return Object.assign( {}, state, newState);
    }

    put (state, action) {
        switch (action.type) {

        case 'SAMPLE-ACTION':
            return this.merge(state, {
                'sample': {
                    action: state.data
                }
            });

        default:
            return state;
        }
    }
}
