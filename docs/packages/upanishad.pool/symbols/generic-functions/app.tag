<app>
    <home style={STYLE.display(STORE.get('tags.home'))}></home>
    <get-index style={STYLE.display(STORE.get('tags.get-index'))}></get-index>
    <get-memes style={STYLE.display(STORE.get('tags.get-memes'))}></get-memes>
    <tx-add-memes style={STYLE.display(STORE.get('tags.tx-add-memes'))}></tx-add-memes>

    <up-footer></up-footer>
    <up-nav></up-nav>

    <script>
     STORE.subscribe(function (action) {
         if (action.type=='MOVE-PAGE') {
             this.update();
             return;
         }
     }.bind(this));
    </script>
</app>
