class HT {
    constructor () {}

    ht_p (obj) {
        return (obj instanceof Object) && !(obj instanceof Array);
    }

    merge (ht, add_ht) {
        for (var k in add_ht) {
            var v = add_ht[k];
            if (!this.ht_p(v))
                add_ht[k] = v;
            else
                this.merge_child (ht[k], add_ht[k]);
        }
    }
}
